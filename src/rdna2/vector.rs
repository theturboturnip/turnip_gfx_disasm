#![allow(non_camel_case_types)]
use bitutils::bits;

use crate::{Action, Dependency, ValueRef};

use super::{
    opcodes::{
        decode_opcode, VOP1_Opcode, VOP2_Opcode, VOP3A_Opcode, VOP3B_Opcode, VOP3P_Opcode,
        VOPC_Opcode,
    },
    utils::{decode_vector_src, extract_u32, ScalarInputOperand, VectorInputOperand},
    Decodable, RDNA2DecodeError,
};

#[derive(Debug, Clone, Copy)]
pub enum VOP {
    VOP2 {
        OP: VOP2_Opcode,
        VDST: u8,
        VSRC1: u8,
        SRC0: VectorInputOperand,
        extra: Option<u32>,
    },
    VOP1 {
        OP: VOP1_Opcode,
        VDST: u8,
        SRC0: VectorInputOperand,
        extra: Option<u32>,
    },
    VOPC {
        OP: VOPC_Opcode,
        VSRC1: u8,
        SRC0: VectorInputOperand,
        extra: Option<u32>,
    },
}
impl VOP {
    fn operand_to_valueref(SRC0: VectorInputOperand, extra: Option<u32>) -> ValueRef {
        match SRC0 {
            VectorInputOperand::Base(ScalarInputOperand::ValueRef(v)) => v,
            VectorInputOperand::Base(ScalarInputOperand::Extra32BitConstant) => {
                ValueRef::Literal(extra.unwrap() as u64)
            }
            VectorInputOperand::DPP8
            | VectorInputOperand::DPP16
            | VectorInputOperand::DPP8FI
            | VectorInputOperand::SDWA => {
                // TODO this is some form of parallelism between workers for DPP - mention that?
                ValueRef::GeneralPurposeRegister(bits!(extra.unwrap(), 0:7) as u64)
            }
            VectorInputOperand::LDSDirect => ValueRef::SpecialReg {
                name: "LDS (Local Data Shader) Direct Access",
                idx: 0,
            },
        }
    }
}
impl Decodable for VOP {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), RDNA2DecodeError> {
        // Read first 4 bytes, decide if we have an extra 32-bit literal constant
        let instr = extract_u32(data)?;

        if bits!(instr, 31:31) != 0 {
            Err(RDNA2DecodeError::BadValue(
                "vector ALU major opcode",
                instr.into(),
            ))
        } else {
            let SRC0 = decode_vector_src(bits!(instr, 0:8) as u16, true)?;
            if bits!(instr, 25:31) == 0b0111110 {
                // VOPC
                let (length, extra) = match SRC0 {
                    VectorInputOperand::DPP8 | VectorInputOperand::DPP8FI => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    VectorInputOperand::DPP16 => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::SDWA => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::Base(ScalarInputOperand::Extra32BitConstant) => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOPC {
                        OP: decode_opcode(bits!(instr, 17:24))?,
                        VSRC1: bits!(instr, 9:16) as u8,
                        SRC0,
                        extra: extra,
                    },
                ))
            } else if bits!(instr, 25:31) == 0b0111111 {
                // VOP1
                let (length, extra) = match SRC0 {
                    VectorInputOperand::DPP8 | VectorInputOperand::DPP8FI => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    VectorInputOperand::DPP16 => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::SDWA => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::Base(ScalarInputOperand::Extra32BitConstant) => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOP1 {
                        OP: decode_opcode(bits!(instr, 9:16))?,
                        VDST: bits!(instr, 17:24) as u8,
                        SRC0,
                        extra: extra,
                    },
                ))
            } else {
                // VOP2
                let (length, extra) = match SRC0 {
                    VectorInputOperand::DPP8 | VectorInputOperand::DPP8FI => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    VectorInputOperand::DPP16 => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::SDWA => (8, Some(extract_u32(&data[4..])?)),
                    VectorInputOperand::Base(ScalarInputOperand::Extra32BitConstant) => {
                        (8, Some(extract_u32(&data[4..])?))
                    }
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOP2 {
                        OP: decode_opcode(bits!(instr, 25:30))?,
                        VSRC1: bits!(instr, 9:16) as u8,
                        VDST: bits!(instr, 17:24) as u8,
                        SRC0,
                        extra: extra,
                    },
                ))
            }
        }
    }
}
impl Action for VOP {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        match self {
            Self::VOP1 {
                OP,
                VDST,
                SRC0,
                extra,
            } => vec![Dependency::new(
                vec![VOP::operand_to_valueref(*SRC0, *extra)],
                ValueRef::GeneralPurposeRegister(*VDST as u64),
            )],
            Self::VOP2 {
                OP,
                VDST,
                VSRC1,
                SRC0,
                extra,
            } => vec![Dependency::new(
                vec![
                    VOP::operand_to_valueref(*SRC0, *extra),
                    ValueRef::GeneralPurposeRegister(*VSRC1 as u64),
                ],
                ValueRef::GeneralPurposeRegister(*VDST as u64),
            )],
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VOP3 {
    VOP3A {
        OP: VOP3A_Opcode,
        CLMP: bool,
        OP_SEL: u8,
        ABS: u8,
        VDST: u8,
        NEG: u8,
        OMOD: u8,
        SRC2: VectorInputOperand,
        SRC1: VectorInputOperand,
        SRC0: VectorInputOperand,
        extra_literal: Option<u32>,
    },
    VOP3B {
        OP: VOP3B_Opcode,
        CLMP: bool,
        SDST: u8,
        VDST: u8,
        NEG: u8,
        OMOD: u8,
        SRC2: VectorInputOperand,
        SRC1: VectorInputOperand,
        SRC0: VectorInputOperand,
        extra_literal: Option<u32>,
    },
    VOP3P {
        OP: VOP3P_Opcode,
        CLMP: bool,
        OP_SEL: u8,
        NEG_HI: u8,
        VDST: u8,
        NEG: u8,
        OP_SEL_HI: u8,
        SRC2: VectorInputOperand,
        SRC1: VectorInputOperand,
        SRC0: VectorInputOperand,
        extra_literal: Option<u32>,
    },
}
impl Decodable for VOP3 {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), super::RDNA2DecodeError> {
        // Always 64bits
        let instr = extract_u32(data)?;
        let instr_top = extract_u32(&data[4..])?;

        let SRC0 = decode_vector_src(bits!(instr_top, 0:8) as u16, true)?;
        let SRC1 = decode_vector_src(bits!(instr_top, 9:17) as u16, false)?;
        let SRC2 = decode_vector_src(bits!(instr_top, 18:26) as u16, false)?;

        if [SRC0, SRC1, SRC2].iter().any(|x| match x {
            VectorInputOperand::DPP8
            | VectorInputOperand::DPP8FI
            | VectorInputOperand::DPP16
            | VectorInputOperand::SDWA => true,
            _ => false,
        }) {
            return Err(RDNA2DecodeError::BadValue(
                "VOP£ instruction uses DPP or SDWA",
                instr as u64 | ((instr_top as u64) << 32),
            ));
        }

        let (length, extra_literal) = if [SRC0, SRC1, SRC2]
            .iter()
            .any(|x| *x == VectorInputOperand::Base(ScalarInputOperand::Extra32BitConstant))
        {
            (12, Some(extract_u32(&data[8..])?))
        } else {
            (8, None)
        };

        match bits!(instr, 26:31) {
            0b110101 => {
                // VOP3A or VOP3B

                let opcode = bits!(instr, 16:25);

                // Try decoding as VOP3A, then if that doesn't work try VOP3B
                if let Ok(OP) = decode_opcode::<VOP3A_Opcode>(opcode) {
                    Ok((
                        &data[length..],
                        Self::VOP3A {
                            OP,
                            CLMP: bits!(instr, 15:15) != 0,
                            OP_SEL: bits!(instr, 11:14) as u8,
                            ABS: bits!(instr, 8:10) as u8,
                            VDST: bits!(instr, 0:7) as u8,
                            NEG: bits!(instr_top, 29:31) as u8,
                            OMOD: bits!(instr_top, 27:28) as u8,
                            SRC2,
                            SRC1,
                            SRC0,
                            extra_literal,
                        },
                    ))
                } else {
                    let OP = decode_opcode(opcode)?;

                    Ok((
                        &data[length..],
                        Self::VOP3B {
                            OP,
                            CLMP: bits!(instr, 15:15) != 0,
                            SDST: bits!(instr, 8:14) as u8,
                            VDST: bits!(instr, 0:7) as u8,
                            NEG: bits!(instr_top, 29:31) as u8,
                            OMOD: bits!(instr_top, 27:28) as u8,
                            SRC2,
                            SRC1,
                            SRC0,
                            extra_literal,
                        },
                    ))
                }
            }
            0b110011 => {
                // VOP3P

                let opcode = bits!(instr, 16:22);

                let OP_SEL_HI_1_0 = bits!(instr_top, 27:28) as u8;
                let OP_SEL_HI_2 = bits!(instr_top, 14:14) as u8;
                let OP_SEL_HI = (OP_SEL_HI_2 << 2) | OP_SEL_HI_1_0;

                Ok((
                    &data[length..],
                    Self::VOP3P {
                        OP: decode_opcode(opcode)?,
                        CLMP: bits!(instr, 15:15) != 0,
                        OP_SEL: bits!(instr, 11:13) as u8,
                        NEG_HI: bits!(instr, 8:10) as u8,
                        VDST: bits!(instr, 0:7) as u8,
                        NEG: bits!(instr_top, 29:31) as u8,
                        OP_SEL_HI,
                        SRC2,
                        SRC1,
                        SRC0,
                        extra_literal,
                    },
                ))
            }
            _ => Err(RDNA2DecodeError::BadValue(
                "VOP instruction major opcode",
                instr.into(),
            )),
        }
    }
}
impl Action for VOP3 {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        todo!()
    }
}
