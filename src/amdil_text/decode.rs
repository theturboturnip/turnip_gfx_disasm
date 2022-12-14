use crate::{
    abstract_machine::vector::MaskedSwizzle,
    hlsl::compat::{HLSLCompatibleAction, HLSLCompatibleOutcome},
    DataKind, DataWidth, ScalarAction, ScalarOutcome, TypedVMRef,
};

use self::registers::arg_as_vector_data_ref;

use super::{
    grammar,
    vm::{AMDILAbstractVM, AMDILDataRef, AMDILDeclaration},
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
impl ScalarAction<AMDILAbstractVM> for Instruction {
    fn outcomes(&self) -> Vec<crate::ScalarOutcome<AMDILAbstractVM>> {
        match self {
            Instruction::DontCare(..) => {
                vec![]
            }
            Instruction::Unknown(g_instr) => {
                if g_instr.args.len() >= 2 {
                    todo!("Best-effort outcomes for Unknown instructions with an identifiable src and dst")
                } else {
                    // Assume the unknown instruction doesn't do anything necessary
                    vec![]
                }
            }
            Instruction::Decl(decl) => decl.outcomes(),
            Instruction::Alu(alu) => alu.outcomes(),
            Instruction::EarlyOut(inputs) => {
                let args: Result<Vec<AMDILDataRef>, AMDILTextDecodeError> =
                    inputs.iter().map(arg_as_vector_data_ref).collect();
                let args = args.unwrap();
                let scalar_args = args
                    .iter()
                    .map(|v_arg| {
                        v_arg
                            .swizzle
                            .0
                            .iter()
                            .filter_map(|comp| comp.map(|comp| (v_arg.name.clone(), comp)))
                    })
                    .flatten()
                    .map(|comp_ref| TypedVMRef {
                        data: comp_ref.into(),
                        kind: DataKind::Hole,
                        width: DataWidth::E32,
                    })
                    .collect();
                vec![ScalarOutcome::EarlyOut {
                    inputs: scalar_args,
                }]
            }
        }
    }
}
impl HLSLCompatibleAction<AMDILAbstractVM> for Instruction {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<AMDILAbstractVM>> {
        match self {
            Instruction::DontCare(..) => {
                vec![]
            }
            Instruction::Unknown(g_instr) => {
                if g_instr.args.len() >= 2 {
                    todo!("Best-effort outcomes for Unknown instructions with an identifiable src and dst")
                } else {
                    // Assume the unknown instruction doesn't do anything necessary
                    vec![]
                }
            }
            Instruction::Decl(decl) => decl.hlsl_outcomes(),
            Instruction::Alu(alu) => alu.hlsl_outcomes(),
            Instruction::EarlyOut(_) => vec![HLSLCompatibleOutcome::EarlyOut {
                inputs: Self::outcomes(&self)
                    .into_iter()
                    .map(|s_outcome| match s_outcome {
                        ScalarOutcome::EarlyOut { inputs } => inputs,
                        _ => panic!(),
                    })
                    .flatten()
                    .collect(),
            }],
        }
    }
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

pub fn decode_instruction(
    g_instr: grammar::Instruction,
) -> Result<Instruction, AMDILTextDecodeError> {
    let instr = match g_instr.instr.as_str() {
        "discard_logicalnz" => Instruction::EarlyOut(decode_args(&g_instr.args)),
        instr if check_if_dontcare(instr) => Instruction::DontCare(g_instr),
        instr if instr.starts_with("dcl_") => decode_declare(&g_instr)?,
        _ => {
            if let Some(alu) = decode_alu(&g_instr)? {
                Instruction::Alu(alu)
            } else {
                Instruction::Unknown(g_instr)
            }
        }
    };

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
        _ => Instruction::Unknown(g_instr.clone()),
    };

    Ok(instr)
}
