use bitutils::bits;

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
        OP: u8,
        SDST: u8,
        SSRC1: u8,
        SSRC0: u8,
        extra_literal: Option<u32>,
    },
    /// p240
    SOPK {
        OP: u8,
        SDST: u8,
        SIMM16: u16,
    },
    /// p241
    SOP1 {
        OP: u8,
        SDST: u8,
        SSRC0: u8,
        extra_literal: Option<u32>,
    },
    // p244
    SOPC {
        OP: u8,
        SSRC1: u8,
        SSRC0: u8,
        extra_literal: Option<u32>,
    },
    // p245
    SOPP {
        OP: u8,
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
                    OP: bits!(instr, 22:16) as u8,
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
                    OP: bits!(instr, 22:16) as u8,
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
                    OP: bits!(instr, 15:8) as u8,
                    SDST: bits!(instr, 22:16) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 31:28) == 0b1011 {
            Ok((
                &data[4..],
                Self::SOPK {
                    OP: bits!(instr, 27:23) as u8,
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
                    OP: bits!(instr, 29:23) as u8,
                    SDST: bits!(instr, 22:16) as u8,
                    SSRC1: bits!(instr, 15:8) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else {
            Err(DecodeError::NotApplicable)
        }
    }
}
impl Action for ScalarALUInstr {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        todo!()
    }
}
