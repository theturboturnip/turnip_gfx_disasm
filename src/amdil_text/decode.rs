mod alu;
mod registers;
mod grammar;
mod error;
pub use error::{AMDILError, AMDILErrorContext};

use alu::{parse_alu, ALUInstruction};

use crate::{amdil_text::decode::grammar::RegRelativeAddr, abstract_machine::{vector::VectorComponent, expr::Scalar}, hlsl::{syntax::{BinaryArithmeticOp, FauxBooleanOp, HLSLOperator}, kinds::{HLSLKind, HLSLKindBitmask}}};

use self::{grammar::{parse_instruction_name, parse_dst, parse_src, DstWrite, parse_hex_literal, CtrlSpec, DstMods, Dst}, registers::AMDILContext, error::NomGrammarResult};

use super::vm::{AMDILDeclaration, AMDILRegister};

#[derive(Debug, Clone)]
pub enum Instruction {
    DontCare(String),
    Decl(AMDILDeclaration),
    Alu(ALUInstruction),
    /// Early out based on the a set of args
    EarlyOut(Scalar<AMDILRegister>),
    If {
        cond: Scalar<AMDILRegister>,
        if_true: Vec<Instruction>,
        if_fals: Vec<Instruction>,
    }
}

pub fn parse_lines(data: &str) -> Result<Vec<Instruction>, AMDILErrorContext> {
    let mut ctx = AMDILContext::new();

    let instruction_lines = data
        .trim_end_matches(char::from(0)) // trim off the trailing \0
        .lines()
        // Filter out empty lines
        .filter(|data| data.len() > 0);

    for line in instruction_lines {
        let i = parse_instruction(&mut ctx, line)
            .map_err(|err| AMDILErrorContext::new(line, err))?;
        ctx.push_instruction(i);
    }
    Ok(ctx.finalize())
}
fn parse_instruction(ctx: &mut AMDILContext, line: &str) -> Result<Instruction, AMDILError> {
    let (line, (instr, ctrl_specifiers, dst_mods)) = parse_instruction_name(line)?;

    let (line, instr) = match instr.as_str() {
        x if x.starts_with("il_vs") || x.starts_with("il_ps") => (line, Instruction::DontCare(instr)),
        "ret_dyn" => (line, Instruction::DontCare(instr)),
        "end" => (line, Instruction::DontCare(instr)),
        "" => (line, Instruction::DontCare(instr)),

        "discard_logicalnz" => {
            let (line, src) = parse_src(line)?;
            let src = ctx.input_to_scalar(src)?;
            (line, Instruction::EarlyOut(Scalar::Expr {
                op: HLSLOperator::FauxBoolean(FauxBooleanOp::Ne),
                inputs: vec![
                    (src, HLSLKind::NUMERIC),
                    (Scalar::Literal(0), HLSLKind::NUMERIC)
                ],
                output_kind: HLSLKind::NUMERIC
            }))
        },

        d if d.starts_with("dcl_") => {
            parse_declare(ctx, line, instr, ctrl_specifiers)?
        },

        _ => {
            let (line, alu) = parse_alu(line, instr, ctrl_specifiers, dst_mods, ctx)?;
            (line, Instruction::Alu(alu))
        }
    };

    if line.len() != 0 {
        panic!("didn't parse '{}'", line);
    }

    Ok(instr)
}

fn parse_declare<'a>(ctx: &mut AMDILContext, line: &'a str, instr: String, ctrl_specifiers: Vec<CtrlSpec>) -> Result<(&'a str, Instruction), AMDILError> {
    let parse_dst = || -> NomGrammarResult<(Dst, u8)> {
        let (line, dst) = parse_dst(line)?;
        // This is usually .x, .xy, .xyz, .xyzw; but if one of the components is unused then it can also be v1._yzw.
        // The length of the vector = the maximum index of a written component + 1
        let n_comps = dst.write_mask
            .iter()
            .enumerate()
            .filter_map(|(i, dstwrite)| {
                match dstwrite {
                    DstWrite::Write => Some(i),
                    DstWrite::NoWrite => None,
                }
            })
            .max().unwrap() as u8 + 1;

        Ok((line, (dst, n_comps)))
    };
    
    
    match instr.as_str() {
        "dcl_cb" => {
            let (line, (dst, _n_comps)) = parse_dst()?;
            assert_eq!(dst.regid.rel_addrs.len(), 1);
            let len = if let RegRelativeAddr::Literal(len) = dst.regid.rel_addrs[0] {
                len
            } else {
                panic!("Can't dcl_cb with non-literal length");
            };
            Ok((line, Instruction::Decl(AMDILDeclaration::NamedBuffer {
                name: dst.regid.name,
                len,
            })))
        }
        "dcl_input_generic" => {
            let (line, (dst, len)) = parse_dst()?;
            Ok((line, Instruction::Decl(AMDILDeclaration::NamedInputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })))
        }
        "dcl_output_generic" => {
            let (line, (dst, len)) = parse_dst()?;
            Ok((line, Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })))
        }

        "dcl_output_position" => {
            let (line, (dst, len)) = parse_dst()?;
            Ok((line, Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Position".to_owned(),
            })))
        }
        
        "dcl_literal" => {
            let (line, (dst, _n_comps)) = parse_dst()?;
            let (line, x) = parse_hex_literal(line)?;
            let (line, y) = parse_hex_literal(line)?;
            let (line, z) = parse_hex_literal(line)?;
            let (line, w) = parse_hex_literal(line)?;
            let literal = [x, y, z, w];
            ctx.push_named_literal(dst.regid.name.clone(), Box::new(literal));
            Ok((line, Instruction::Decl(AMDILDeclaration::NamedLiteral(
                dst.regid.name,
                literal,
            ))))
        },
        "dcl_global_flags" => {
            let (line, (_dst, _len)) = parse_dst()?;
            Ok((line, Instruction::DontCare(instr)))
        },
        "dcl_user_line_api" => {
            let (line, (_dst, _len)) = parse_dst()?;
            Ok((line, Instruction::DontCare(instr)))
        }, // TODO parse this
        "dcl_resource" => match matchable_ctrl_specs(&ctrl_specifiers)[..] {
            [
                ("id", id),
                ("type", "2d"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                Ok((line, Instruction::Decl(AMDILDeclaration::Texture2D(id.parse().unwrap()))))
            }
            [
                ("id", id),
                ("type", "3d"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                Ok((line, Instruction::Decl(AMDILDeclaration::Texture3D(id.parse().unwrap()))))
            }
            [
                ("id", id),
                ("type", "cubemap"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                Ok((line, Instruction::Decl(AMDILDeclaration::TextureCube(id.parse().unwrap()))))
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