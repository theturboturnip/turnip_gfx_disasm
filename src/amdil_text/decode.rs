mod alu;
mod registers;
mod grammar;
mod error;
pub use error::{AMDILError, AMDILErrorContext};

use alu::{parse_alu, ALUInstruction};

use crate::{amdil_text::decode::grammar::RegRelativeAddr, abstract_machine::vector::VectorComponent};

use self::{grammar::{parse_instruction_name, parse_dst, parse_src, DstWrite, parse_hex_literal, CtrlSpec, DstMod}, registers::AMDILContext};

use super::vm::{AMDILDeclaration, AMDILRegister};

#[derive(Debug, Clone)]
pub enum Instruction {
    DontCare(String),
    Decl(AMDILDeclaration),
    Alu(ALUInstruction),
    /// Early out based on the a set of args
    EarlyOut(AMDILRegister, VectorComponent), // TODO use UntypedScalar::Expr{} for nonzero or == zero
}

pub fn parse_lines(data: &str) -> Result<Vec<Instruction>, AMDILErrorContext> {
    let mut ctx = AMDILContext::new();
    data.trim_end_matches(char::from(0)) // trim off the trailing \0
        .lines()
        // Filter out empty lines
        .filter(|data| data.len() > 0)
        .map(|data| {
            parse_instruction(&mut ctx, data)
                .map_err(|err| AMDILErrorContext::new(data, err))
        })
        .collect()
}
fn parse_instruction(ctx: &mut AMDILContext, data: &str) -> Result<Instruction, AMDILError> {
    let (data, (instr, ctrl_specifiers, dst_mods)) = parse_instruction_name(data)?;

    let (data, instr) = match instr.as_str() {
        x if x.starts_with("il_vs") || x.starts_with("il_ps") => (data, Instruction::DontCare(instr)),
        "ret_dyn" => (data, Instruction::DontCare(instr)),
        "end" => (data, Instruction::DontCare(instr)),
        "" => (data, Instruction::DontCare(instr)),

        "discard_logicalnz" => {
            let (data, src) = parse_src(data)?;
            let src = ctx.src_to_vector(&src)?;
            assert!(src.1.0[0].is_some());
            (data, Instruction::EarlyOut(src.0.clone(), src.1.0[0].unwrap()))
        },

        d if d.starts_with("dcl_") => {
            parse_declare(ctx, data, instr, ctrl_specifiers, dst_mods)?
        },

        _ => {
            let (data, alu) = parse_alu(data, instr, ctrl_specifiers, dst_mods, ctx)?;
            (data, Instruction::Alu(alu))
        }
    };

    if data.len() != 0 {
        panic!("didn't parse '{}'", data);
    }

    Ok(instr)
}

fn parse_declare<'a>(ctx: &mut AMDILContext, data: &'a str, instr: String, ctrl_specifiers: Vec<CtrlSpec>, mods: Vec<DstMod>) -> Result<(&'a str, Instruction), AMDILError> {
    let (data, dst) = parse_dst(data, mods)?;
    // This is usually .x, .xy, .xyz, .xyzw; but if one of the components is unused then it can also be v1._yzw.
    // The length of the vector = the maximum index of a written component + 1
    let len = dst.write_mask
        .iter()
        .enumerate()
        .filter_map(|(i, dstwrite)| {
            match dstwrite {
                DstWrite::Write => Some(i),
                DstWrite::NoWrite => None,
            }
        })
        .max().unwrap() as u8 + 1;
    match instr.as_str() {
        "dcl_cb" => {
            assert_eq!(dst.regid.rel_addrs.len(), 1);
            let len = if let RegRelativeAddr::Literal(len) = dst.regid.rel_addrs[0] {
                len 
            } else {
                panic!("Can't dcl_cb with non-literal length");
            };
            Ok((data, Instruction::Decl(AMDILDeclaration::NamedBuffer {
                name: dst.regid.name,
                len,
            })))
        }
        "dcl_input_generic" => {
            
            Ok((data, Instruction::Decl(AMDILDeclaration::NamedInputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })))
        }
        "dcl_output_generic" => {
            Ok((data, Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })))
        }

        "dcl_output_position" => {
            Ok((data, Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Position".to_owned(),
            })))
        }
        
        "dcl_literal" => {
            let (data, x) = parse_hex_literal(data)?;
            let (data, y) = parse_hex_literal(data)?;
            let (data, z) = parse_hex_literal(data)?;
            let (data, w) = parse_hex_literal(data)?;
            let literal = [x, y, z, w];
            ctx.push_named_literal(dst.regid.name.clone(), Box::new(literal));
            Ok((data, Instruction::Decl(AMDILDeclaration::NamedLiteral(
                dst.regid.name,
                literal,
            ))))
        },
        "dcl_global_flags" => {
            Ok((data, Instruction::DontCare(instr)))
        },
        "dcl_user_data_api" => Ok((data, Instruction::DontCare(instr))), // TODO parse this
        "dcl_resource" => match matchable_ctrl_specs(&ctrl_specifiers)[..] {
            [
                ("id", id),
                ("type", "2d"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                Ok((data, Instruction::Decl(AMDILDeclaration::TextureResource(id.parse().unwrap()))))
            }
            _ => return Err(AMDILError::UnkInstruction(instr, ctrl_specifiers))
        }
        _ => return Err(AMDILError::UnkInstruction(instr, ctrl_specifiers))
    }
}

pub fn matchable_ctrl_specs<'a>(mods: &'a Vec<grammar::CtrlSpec>) -> Vec<(&'a str, &'a str)> {
    mods.iter()
        .map(|g_mod| (g_mod.name.as_str(), g_mod.value.as_str()))
        .collect()
}