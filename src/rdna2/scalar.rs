#![allow(non_camel_case_types)]
use bitutils::bits;

use super::opcodes::{
    decode_opcode, SMEM_Opcode, SOP1_Opcode, SOP2_Opcode, SOPC_Opcode, SOPK_Opcode, SOPP_Opcode,
};
use super::utils::{decode_scalar_src, extract_u32, ScalarInputOperand};
use super::vm::{RDNA2AbstractVM, RDNA2Outcome};
use super::{Decodable, RDNA2DecodeError};
use crate::ScalarAction;

#[derive(Debug, Clone, Copy)]
pub enum ScalarALUInstr {
    /// p238
    SOP2 {
        OP: SOP2_Opcode,
        SDST: u8,
        SSRC1: ScalarInputOperand,
        SSRC0: ScalarInputOperand,
        extra_literal: Option<u32>,
    },
    /// p240
    SOPK {
        OP: SOPK_Opcode,
        SDST: u8,
        SIMM16: u16,
    },
    /// p241
    SOP1 {
        OP: SOP1_Opcode,
        SDST: u8,
        SSRC0: ScalarInputOperand,
        extra_literal: Option<u32>,
    },
    // p244
    SOPC {
        OP: SOPC_Opcode,
        SSRC1: ScalarInputOperand,
        SSRC0: ScalarInputOperand,
        extra_literal: Option<u32>,
    },
    // p245
    SOPP {
        OP: SOPP_Opcode,
        SIMM: u16,
    },
}
impl Decodable for ScalarALUInstr {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), RDNA2DecodeError> {
        // Read first 4 bytes, decide if we have an extra 32-bit literal constant
        let instr = extract_u32(data)?;

        if bits!(instr, 23:31) == 0b10_1111111 {
            Ok((
                &data[4..],
                Self::SOPP {
                    OP: decode_opcode(bits!(instr, 16:22))?,
                    SIMM: bits!(instr, 0:15) as u16,
                },
            ))
        } else if bits!(instr, 23:31) == 0b10_1111110 {
            let SSRC0 = decode_scalar_src(bits!(instr, 0:7) as u8)?;
            let SSRC1 = decode_scalar_src(bits!(instr, 8:15) as u8)?;
            // TODO - is it valid for SSRC1 to be an extra 32-bit constant?
            let (length, extra_literal) = if SSRC0 == ScalarInputOperand::Extra32BitConstant
                || SSRC1 == ScalarInputOperand::Extra32BitConstant
            {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOPC {
                    OP: decode_opcode(bits!(instr, 16:22))?,
                    SSRC1,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 23:31) == 0b10_1111101 {
            let SSRC0 = decode_scalar_src(bits!(instr, 0:7) as u8)?;
            let (length, extra_literal) = if SSRC0 == ScalarInputOperand::Extra32BitConstant {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOP1 {
                    OP: decode_opcode(bits!(instr, 8:15))?,
                    SDST: bits!(instr, 16:22) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 28:31) == 0b1011 {
            Ok((
                &data[4..],
                Self::SOPK {
                    OP: decode_opcode(bits!(instr, 23:27))?,
                    SDST: bits!(instr, 16:22) as u8,
                    SIMM16: bits!(instr, 0:15) as u16,
                },
            ))
        } else if bits!(instr, 30:31) == 0b10 {
            let SSRC0 = decode_scalar_src(bits!(instr, 0:7) as u8)?;
            let SSRC1 = decode_scalar_src(bits!(instr, 8:15) as u8)?;
            // TODO - is it valid for SSRC1 to be an extra 32-bit constant?
            let (length, extra_literal) = if SSRC0 == ScalarInputOperand::Extra32BitConstant
                || SSRC1 == ScalarInputOperand::Extra32BitConstant
            {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOP2 {
                    OP: decode_opcode(bits!(instr, 23:29))?,
                    SDST: bits!(instr, 16:22) as u8,
                    SSRC1,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else {
            Err(RDNA2DecodeError::BadValue(
                "scalar ALU major opcode",
                instr.into(),
            ))
        }
    }
}
impl ScalarAction<RDNA2AbstractVM> for ScalarALUInstr {
    fn outcomes(&self) -> Vec<RDNA2Outcome> {
        match self {
            Self::SOPP { OP, .. } => match OP {
                SOPP_Opcode::S_INST_PREFETCH | SOPP_Opcode::S_NOP | SOPP_Opcode::S_ENDPGM => vec![],
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SMEM {
    OP: SMEM_Opcode,
    GLC: bool,
    DLC: bool,
    SDATA: u8,
    SBASE: u8,
    SOFFSET: u8,
    OFFSET: u32,
}
impl Decodable for SMEM {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), RDNA2DecodeError> {
        let instr = extract_u32(data)?;
        let instr_top = extract_u32(&data[4..])?;

        if bits!(instr, 26:31) == 0b111101 {
            Ok((
                &data[8..],
                Self {
                    OP: decode_opcode(bits!(instr, 18:25))?,
                    SBASE: bits!(instr, 0:5) as u8,
                    SDATA: bits!(instr, 6:12) as u8,
                    DLC: bits!(instr, 14:14) != 0,
                    GLC: bits!(instr, 16:16) != 0,
                    // 32:52 => 0:20 for instr_top
                    OFFSET: bits!(instr_top, 0:20) as u32,
                    // 57:63 => 25:31 for instr_top
                    SOFFSET: bits!(instr_top, 25:31) as u8,
                },
            ))
        } else {
            Err(RDNA2DecodeError::BadValue(
                "scalar ALU major opcode",
                instr.into(),
            ))
        }
    }
}
impl ScalarAction<RDNA2AbstractVM> for SMEM {
    fn outcomes(&self) -> Vec<RDNA2Outcome> {
        todo!()
    }
}
