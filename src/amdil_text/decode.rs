use crate::abstract_machine::vector::{MaskedSwizzle, VectorDeclaration};

use super::grammar;

#[derive(Debug, Clone)]
pub enum AMDILTextDecodeError {
    BadValue(&'static str, grammar::Instruction),
}
#[derive(Debug, Clone)]
pub enum Instruction {
    Unknown(grammar::Instruction),
    DontCare(grammar::Instruction),
    Decl(VectorDeclaration),
    Version(String),
}
#[derive(Debug, Clone)]
pub enum Arg {
    HexLiteral(u64),
    Named(String),
    NamedIndexed(String, u64),
    NamedSwizzled(String, MaskedSwizzle),
    Complex(grammar::Arg),
}
pub fn decode_args(args: &Vec<grammar::Arg>) -> Vec<Arg> {
    args.iter()
        .map(|g_arg| match g_arg {
            grammar::Arg::HexLiteral(x) => Arg::HexLiteral(*x),
            grammar::Arg::Named(name, mods) => match mods.as_slice() {
                &[] => Arg::Named(name.clone()),
                &[grammar::ArgMod::Indexed(idx)] => Arg::NamedIndexed(name.clone(), idx),
                &[grammar::ArgMod::Swizzled(swizzle)] => Arg::NamedSwizzled(name.clone(), swizzle),
                _ => Arg::Complex(g_arg.clone()),
            },
        })
        .collect()
}

pub fn decode_instruction(
    g_instr: grammar::Instruction,
) -> Result<Instruction, AMDILTextDecodeError> {
    use AMDILTextDecodeError::BadValue;

    let instr = match (
        g_instr.instr.as_str(),
        decode_args(&g_instr.args).as_slice(),
    ) {
        ("dcl_cb", [Arg::NamedIndexed(cb_name, len)]) => {
            Instruction::Decl(VectorDeclaration::NamedBuffer {
                name: cb_name.to_string(),
                len: *len,
            })
        }
        ("dcl_input_generic", [arg]) => {
            let (name, swizzle) = match arg {
                Arg::Named(name) => (name, MaskedSwizzle::identity(4)),
                Arg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => {
                    return Err(BadValue(
                        "bad argument for dcl_input_generic",
                        g_instr.clone(),
                    ))
                }
            };
            Instruction::Decl(VectorDeclaration::NamedInputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Generic".to_owned(),
            })
        }
        ("dcl_output_generic", [arg]) => {
            let (name, swizzle) = match arg {
                Arg::Named(name) => (name, MaskedSwizzle::identity(4)),
                Arg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => return Err(BadValue("bad argument for dcl_output_generic", g_instr)),
            };
            Instruction::Decl(VectorDeclaration::NamedOutputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Generic".to_owned(),
            })
        }

        ("dcl_output_position", [arg]) => {
            let (name, swizzle) = match arg {
                Arg::Named(name) => (name, MaskedSwizzle::identity(4)),
                Arg::NamedSwizzled(name, swizzle) => (name, *swizzle),
                _ => return Err(BadValue("bad argument for dcl_input_generic", g_instr)),
            };
            Instruction::Decl(VectorDeclaration::NamedOutputRegister {
                name: name.to_owned(),
                len: swizzle
                    .as_nonzero_length()
                    .ok_or_else(|| BadValue("noncontiguous swizzle", g_instr.clone()))?,
                reg_type: "Position".to_owned(),
            })
        }
        (
            "dcl_literal",
            [Arg::Named(name), Arg::HexLiteral(x), Arg::HexLiteral(y), Arg::HexLiteral(z), Arg::HexLiteral(w)],
        ) => Instruction::Decl(VectorDeclaration::NamedLiteral(
            name.clone(),
            [*x, *y, *z, *w],
        )),

        (instr, _) => {
            if instr.starts_with("il_vs") || instr.starts_with("il_ps") {
                Instruction::DontCare(g_instr)
            } else if instr == "dcl_global_flags" {
                Instruction::DontCare(g_instr)
            } else {
                Instruction::Unknown(g_instr)
            }
        }
    };

    Ok(instr)
}
