use crate::{abstract_machine::vector::{MaskedSwizzle, ComponentOf}, Action, hlsl::{kinds::{HLSLKind, HLSLKindBitmask}, syntax::HLSLOperator}};

use self::registers::AMDILContext;

use super::{
    grammar,
    vm::{AMDILAbstractVM, AMDILMaskSwizVector, AMDILDeclaration, AMDILAction},
};

pub mod alu;
use alu::{decode_alu, ALUInstruction};
pub mod registers;

#[derive(Debug, Clone)]
pub enum AMDILTextDecodeError {
    BadValue(&'static str, grammar::Instruction),
    Generic(String),
}
#[derive(Debug, Clone)]
pub enum Instruction {
    Unknown(grammar::Instruction),
    DontCare(grammar::Instruction),
    Decl(AMDILDeclaration),
    Alu(ALUInstruction),
    /// Early out based on the a set of args
    EarlyOut(Vec<MatchableArg>),
}

/// Adaptation of [grammar::Arg] that's more suitable for match-cases
#[derive(Debug, Clone)]
pub enum MatchableArg {
    HexLiteral(u64),
    Named(String),
    NamedIndexed(String, u64),
    NamedSwizzled(String, MaskedSwizzle),
    NamedIndexedSwizzled(String, u64, MaskedSwizzle),
    Complex(grammar::Arg),
}
pub fn decode_args(args: &Vec<grammar::Arg>) -> Vec<MatchableArg> {
    args.iter()
        .map(|g_arg| match g_arg {
            grammar::Arg::HexLiteral(x) => MatchableArg::HexLiteral(*x),
            grammar::Arg::Named(name, mods) => match mods.as_slice() {
                &[] => MatchableArg::Named(name.clone()),
                &[grammar::ArgMod::Indexed(idx)] => MatchableArg::NamedIndexed(name.clone(), idx),
                &[grammar::ArgMod::Swizzled(swizzle)] => {
                    MatchableArg::NamedSwizzled(name.clone(), swizzle)
                }
                &[grammar::ArgMod::Indexed(idx), grammar::ArgMod::Swizzled(swizzle)] => {
                    MatchableArg::NamedIndexedSwizzled(name.clone(), idx, swizzle)
                }
                _ => MatchableArg::Complex(g_arg.clone()),
            },
        })
        .collect()
}

pub type MatchableInstrMod<'a> = (&'a str, &'a str);
pub fn decode_instr_mods<'a>(mods: &'a Vec<grammar::InstrMod>) -> Vec<MatchableInstrMod<'a>> {
    mods.iter()
        .map(|g_mod| (g_mod.name.as_str(), g_mod.value.as_str()))
        .collect()
}

pub fn push_instruction_actions(
    g_instr: grammar::Instruction,
    ctx: &mut AMDILContext, v: &mut Vec<AMDILAction>
) -> Result<Instruction, AMDILTextDecodeError> {
    let instr = match g_instr.instr.as_str() {
        "discard_logicalnz" => Instruction::EarlyOut(decode_args(&g_instr.args)),
        instr if check_if_dontcare(instr) => Instruction::DontCare(g_instr),
        instr if instr.starts_with("dcl_") => decode_declare(&g_instr)?,
        _ => {
            if let Some(alu) = decode_alu(&g_instr, ctx)? {
                Instruction::Alu(alu)
            } else {
                Instruction::Unknown(g_instr)
            }
        }
    };

    match &instr {
        Instruction::DontCare(..) => {}
        Instruction::Unknown(g_instr) => {
            if g_instr.args.len() >= 2 {
                todo!("Best-effort outcomes for Unknown instructions with an identifiable src and dst")
            } else {
                // Assume the unknown instruction doesn't do anything necessary
            }
        }
        Instruction::Decl(decl) => if let AMDILDeclaration::NamedLiteral(name, literal) = decl {
            ctx.push_named_literal(name.clone(), Box::new(*literal))
        },
        Instruction::Alu(alu) => alu.push_actions(v),
        Instruction::EarlyOut(inputs) => {
            let args: Result<Vec<(AMDILMaskSwizVector, HLSLKind)>, AMDILTextDecodeError> =
                inputs.iter().map(|a| {
                    Ok((ctx.arg_as_vector_data_ref(a)?, HLSLKindBitmask::NUMERIC.into()))
                }).collect();
            let args = args.unwrap();
            assert_eq!(args.len(), 1);
            assert_eq!(args[0].0.swizzle().num_used_components(), 1);
            assert!(args[0].0.swizzle().0[0].is_some());
            v.push(
                Action::If { inputs: vec![(ComponentOf{ vec: args[0].0.register().clone(), comp: args[0].0.swizzle().0[0].unwrap() }, args[0].1)], cond_operator: HLSLOperator::Assign, if_true: vec![Action::EarlyOut], if_fals: vec![] }
            )
        }
    }

    Ok(instr)
}

fn check_if_dontcare(instr: &str) -> bool {
    match instr {
        instr if instr.starts_with("il_vs") || instr.starts_with("il_ps") => true,
        "ret_dyn" => true,
        "end" => true,
        "" => true,
        _ => false,
    }
}

fn decode_declare(g_instr: &grammar::Instruction) -> Result<Instruction, AMDILTextDecodeError> {
    use AMDILTextDecodeError::BadValue;

    let instr = match (
        g_instr.instr.as_str(),
        decode_args(&g_instr.args).as_slice(),
    ) {
        ("dcl_cb", [MatchableArg::NamedIndexed(cb_name, len)]) => {
            Instruction::Decl(AMDILDeclaration::NamedBuffer {
                name: cb_name.to_string(),
                len: *len,
            })
        }
        ("dcl_input_generic", [arg]) => {
            let (name, swizzle) = match arg {
                MatchableArg::Named(name) => (name, MaskedSwizzle::identity(4)),
                MatchableArg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => {
                    return Err(BadValue(
                        "bad argument for dcl_input_generic",
                        g_instr.clone(),
                    ))
                }
            };
            Instruction::Decl(AMDILDeclaration::NamedInputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Generic".to_owned(),
            })
        }
        ("dcl_output_generic", [arg]) => {
            let (name, swizzle) = match arg {
                MatchableArg::Named(name) => (name, MaskedSwizzle::identity(4)),
                MatchableArg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => {
                    return Err(BadValue(
                        "bad argument for dcl_output_generic",
                        g_instr.clone(),
                    ))
                }
            };
            Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Generic".to_owned(),
            })
        }

        ("dcl_output_position", [arg]) => {
            let (name, swizzle) = match arg {
                MatchableArg::Named(name) => (name, MaskedSwizzle::identity(4)),
                MatchableArg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => {
                    return Err(BadValue(
                        "bad argument for dcl_input_generic",
                        g_instr.clone(),
                    ))
                }
            };
            Instruction::Decl(AMDILDeclaration::NamedOutputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Position".to_owned(),
            })
        }
        (
            "dcl_literal",
            [MatchableArg::Named(name), MatchableArg::HexLiteral(x), MatchableArg::HexLiteral(y), MatchableArg::HexLiteral(z), MatchableArg::HexLiteral(w)],
        ) => Instruction::Decl(AMDILDeclaration::NamedLiteral(
            name.clone(),
            [*x, *y, *z, *w],
        )),
        ("dcl_global_flags", [..]) => Instruction::DontCare(g_instr.clone()),
        _ => match decode_instr_mods(&g_instr.instr_mods)[..] {
            [
                ("id", id),
                ("type", "2d"),
                ("fmtx", "float"),
                ("fmty", "float"),
                ("fmtz", "float"),
                ("fmtw", "float"), // dummy comment to force formatting
            ] => {
                Instruction::Decl(AMDILDeclaration::TextureResource(id.parse().unwrap()))
            }
            _ => Instruction::Unknown(g_instr.clone()),
        },
    };

    Ok(instr)
}
