#![allow(non_camel_case_types)]
use bitutils::bits;

use super::opcodes::{
    decode_opcode, SMEM_Opcode, SOP1_Opcode, SOP2_Opcode, SOPC_Opcode, SOPK_Opcode, SOPP_Opcode,
};
use super::{Decodable, DecodeError};
use crate::Action;

fn extract_u32(data: &[u8]) -> Result<u32, DecodeError> {
    if data.len() < 4 {
        Err(DecodeError::NotEnoughData)
    } else {
        Ok((data[0] as u32)
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24))
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ScalarALUInstr {
    /// p238
    SOP2 {
        OP: SOP2_Opcode,
        SDST: u8,
        SSRC1: u8,
        SSRC0: u8,
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
        SSRC0: u8,
        extra_literal: Option<u32>,
    },
    // p244
    SOPC {
        OP: SOPC_Opcode,
        SSRC1: u8,
        SSRC0: u8,
        extra_literal: Option<u32>,
    },
    // p245
    SOPP {
        OP: SOPP_Opcode,
        SIMM: u16,
    },
}
impl Decodable for ScalarALUInstr {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), DecodeError> {
        // Read first 4 bytes, decide if we have an extra 32-bit literal constant
        let instr = extract_u32(data)?;

        if bits!(instr, 31:23) == 0b10_1111111 {
            Ok((
                &data[4..],
                Self::SOPP {
                    OP: decode_opcode(bits!(instr, 22:16) as u8)?,
                    SIMM: bits!(instr, 15:0) as u16,
                },
            ))
        } else if bits!(instr, 31:23) == 0b10_1111110 {
            let SSRC0 = bits!(instr, 7:0) as u8;
            let (length, extra_literal) = if SSRC0 == 255 {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOPC {
                    OP: decode_opcode(bits!(instr, 22:16) as u8)?,
                    SSRC1: bits!(instr, 15:8) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 31:23) == 0b10_111101 {
            let SSRC0 = bits!(instr, 7:0) as u8;
            let (length, extra_literal) = if SSRC0 == 255 {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOP1 {
                    OP: decode_opcode(bits!(instr, 15:8) as u8)?,
                    SDST: bits!(instr, 22:16) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 31:28) == 0b1011 {
            Ok((
                &data[4..],
                Self::SOPK {
                    OP: decode_opcode(bits!(instr, 27:23) as u8)?,
                    SDST: bits!(instr, 22:16) as u8,
                    SIMM16: bits!(instr, 15:0) as u16,
                },
            ))
        } else if bits!(instr, 31:30) == 0b10 {
            let SSRC0 = bits!(instr, 7:0) as u8;
            let (length, extra_literal) = if SSRC0 == 255 {
                (8, Some(extract_u32(&data[4..])?))
            } else {
                (4, None)
            };
            Ok((
                &data[length..],
                Self::SOP2 {
                    OP: decode_opcode(bits!(instr, 29:23) as u8)?,
                    SDST: bits!(instr, 22:16) as u8,
                    SSRC1: bits!(instr, 15:8) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else {
            Err(DecodeError::BadValue(
                "scalar ALU major opcode",
                instr.into(),
            ))
        }
    }
}
impl Action for ScalarALUInstr {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        todo!()
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
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), DecodeError> {
        // Read first 4 bytes, decide if we have an extra 32-bit literal constant
        let instr = extract_u32(data)?;
        let instr_top = extract_u32(&data[4..])?;

        if bits!(instr, 31:26) == 0b111101 {
            Ok((
                &data[8..],
                Self {
                    OP: decode_opcode(bits!(instr, 25:18) as u8)?,
                    SBASE: bits!(instr, 5:0) as u8,
                    SDATA: bits!(instr, 12:6) as u8,
                    DLC: bits!(instr, 14:14) != 0,
                    GLC: bits!(instr, 16:16) != 0,
                    // 52:32 => 20:0 for instr_top
                    OFFSET: bits!(instr_top, 20:0) as u32,
                    // 63:57 => 31:25 for instr_top
                    SOFFSET: bits!(instr_top, 31:25) as u8,
                },
            ))
        } else {
            Err(DecodeError::BadValue(
                "scalar ALU major opcode",
                instr.into(),
            ))
        }
    }
}
impl Action for SMEM {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        todo!()
    }
}
