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
        match parse_instruction(&mut ctx, line)
            .map_err(|err| AMDILErrorContext::new(line, err))? {
                Some(instr) => ctx.push_instruction(instr),
                None => {},
            }
    }
    Ok(ctx.finalize())
}
fn parse_instruction(ctx: &mut AMDILContext, line: &str) -> Result<Option<Instruction>, AMDILError> {
    let (line, (leading_whitespace, instr, ctrl_specifiers, dst_mods)) = parse_instruction_name(line)?;

    let (line, instr) = match instr.as_str() {
        x if x.starts_with("il_vs") || x.starts_with("il_ps") => (line, None),
        "ret_dyn" => (line, None),
        "end" => (line, None),
        "" => (line, None),

        "discard_logicalnz" => {
            let (line, src) = parse_src(line)?;
            let src = ctx.input_to_scalar(src)?;
            let cond = Scalar::Expr {
                op: HLSLOperator::FauxBoolean(FauxBooleanOp::Ne),
                inputs: vec![
                    (src, HLSLKind::NUMERIC),
                    (Scalar::Literal(0), HLSLKind::NUMERIC)
                ],
                output_kind: HLSLKind::NUMERIC
            };
            (line, Some(Instruction::EarlyOut(cond)))
        },
        "discard_logicalz" => {
            let (line, src) = parse_src(line)?;
            let src = ctx.input_to_scalar(src)?;
            let cond = Scalar::Expr {
                op: HLSLOperator::FauxBoolean(FauxBooleanOp::Eq),
                inputs: vec![
                    (src, HLSLKind::NUMERIC),
                    (Scalar::Literal(0), HLSLKind::NUMERIC)
                ],
                output_kind: HLSLKind::NUMERIC
            };
            (line, Some(Instruction::EarlyOut(cond)))
        },
        "if_logicalnz" => {
            let (line, src) = parse_src(line)?;
            let src = ctx.input_to_scalar(src)?;
            let cond = Scalar::Expr {
                op: HLSLOperator::FauxBoolean(FauxBooleanOp::Ne),
                inputs: vec![
                    (src, HLSLKind::NUMERIC),
                    (Scalar::Literal(0), HLSLKind::NUMERIC)
                ],
                output_kind: HLSLKind::NUMERIC
            };
            ctx.start_if(cond);
            (line, None)
        }
        "if_logicalz" => {
            let (line, src) = parse_src(line)?;
            let src = ctx.input_to_scalar(src)?;
            let cond = Scalar::Expr {
                op: HLSLOperator::FauxBoolean(FauxBooleanOp::Eq),
                inputs: vec![
                    (src, HLSLKind::NUMERIC),
                    (Scalar::Literal(0), HLSLKind::NUMERIC)
                ],
                output_kind: HLSLKind::NUMERIC
            };
            ctx.start_if(cond);
            (line, None)
        }
        "else" => {
            ctx.encounter_else();
            (line, None)
        }
        "endif" => {
            ctx.end_if();
            (line, None)
        }

        d if d.starts_with("dcl_") => {
            parse_declare(ctx, line, instr, ctrl_specifiers)?
        },

        _ => {
            let (line, alu) = parse_alu(line, instr, ctrl_specifiers, dst_mods, ctx)?;
            (line, Some(Instruction::Alu(alu)))
        }
    };

    if instr.is_some() && (leading_whitespace != ctx.if_depth() * 4) {
        println!("Potential error: leading whitespace for instruction {instr:?} didn't match if-depth");
    }

    if line.len() != 0 {
        panic!("didn't parse '{}'", line);
    }

    Ok(instr)
}

fn parse_declare<'a>(ctx: &mut AMDILContext, line: &'a str, instr: String, ctrl_specifiers: Vec<CtrlSpec>) -> Result<(&'a str, Option<Instruction>), AMDILError> {
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
    
    let (line, decl) = match instr.as_str() {
        "dcl_cb" => {
            let (line, (dst, _n_comps)) = parse_dst()?;
            assert_eq!(dst.regid.rel_addrs.len(), 1);
            let len = if let RegRelativeAddr::Literal(len) = dst.regid.rel_addrs[0] {
                len
            } else {
                panic!("Can't dcl_cb with non-literal length");
            };
            (line, AMDILDeclaration::NamedBuffer {
                name: dst.regid.name,
                len,
            })
        }

        // TODO interpolation modifiers on these e.g. dcl_input_position_interp(linear_noperspective)
        "dcl_input_generic" => {
            let (line, (dst, len)) = parse_dst()?;
            (line, AMDILDeclaration::NamedInputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })
        }
        // TODO fix grammar to parse ctrlspecifier values with underscores inside them correctly.
        "dcl_input_position" | "dcl_input_position_interp(linear_noperspective)" => {
            let (line, (dst, len)) = parse_dst()?;
            (line, AMDILDeclaration::NamedInputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Position".to_owned(),
            })
        }
        "dcl_output_generic" => {
            let (line, (dst, len)) = parse_dst()?;
            (line, AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Generic".to_owned(),
            })
        }

        "dcl_output_position" => {
            let (line, (dst, len)) = parse_dst()?;
            (line, AMDILDeclaration::NamedOutputRegister {
                name: dst.regid.name,
                len,
                reg_type: "Position".to_owned(),
            })
        }
        
        "dcl_literal" => {
            let (line, (dst, _n_comps)) = parse_dst()?;
            let (line, x) = parse_hex_literal(line)?;
            let (line, y) = parse_hex_literal(line)?;
            let (line, z) = parse_hex_literal(line)?;
            let (line, w) = parse_hex_literal(line)?;
            let literal = [x, y, z, w];
            ctx.push_named_literal(dst.regid.name.clone(), Box::new(literal));
            (line, AMDILDeclaration::NamedLiteral(
                dst.regid.name,
                literal,
            ))
        },
        "dcl_global_flags" => {
            let (line, (_dst, _len)) = parse_dst()?;
            return Ok((line, None))
        },
        "dcl_user_data_api" => {
            let (line, (_dst, _len)) = parse_dst()?;
            return Ok((line, None))
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
                (line, AMDILDeclaration::Texture2D(id.parse().unwrap()))
            }
            [
                ("id", id),
                ("type", "3d"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                (line, AMDILDeclaration::Texture3D(id.parse().unwrap()))
            }
            [
                ("id", id),
                ("type", "cubemap"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                (line, AMDILDeclaration::TextureCube(id.parse().unwrap()))
            }
            _ => return Err(AMDILError::UnkInstruction(instr, ctrl_specifiers))
        }
        _ => return Err(AMDILError::UnkInstruction(instr, ctrl_specifiers))
    };
    Ok((line, Some(Instruction::Decl(decl))))
}

pub fn matchable_ctrl_specs<'a>(mods: &'a Vec<grammar::CtrlSpec>) -> Vec<(&'a str, &'a str)> {
    mods.iter()
        .map(|g_mod| (g_mod.name.as_str(), g_mod.value.as_str()))
        .collect()
}