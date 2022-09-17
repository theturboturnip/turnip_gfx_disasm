#![allow(non_camel_case_types)]
use std::convert::TryFrom;

use bitutils::bits;

use super::{utils::extract_u32, Decodable, RDNA2DecodeError};

#[derive(Debug, Clone, Copy)]
pub struct EXPORT {
    EN: u8,
    TARGET: TARGET,
    COMPR: bool,
    DONE: bool,
    VM: bool,
    VSRC0: u8,
    VSRC1: u8,
    VSRC2: u8,
    VSRC3: u8,
}
impl Decodable for EXPORT {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), RDNA2DecodeError> {
        let instr = extract_u32(data)?;
        let instr_top = extract_u32(&data[4..])?;

        if bits!(instr, 26:31) == 0b111110 {
            Ok((
                &data[8..],
                Self {
                    EN: bits!(instr, 0:3) as u8,
                    TARGET: (bits!(instr, 4:9) as u8).try_into()?,
                    COMPR: bits!(instr, 10:10) != 0,
                    DONE: bits!(instr, 11:11) != 0,
                    VM: bits!(instr, 12:12) != 0,
                    VSRC0: bits!(instr_top, 0:7) as u8,
                    VSRC1: bits!(instr_top, 8:15) as u8,
                    VSRC2: bits!(instr_top, 16:23) as u8,
                    VSRC3: bits!(instr_top, 24:31) as u8,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TARGET {
    RenderTarget(u8),
    Z,
    Null,
    Position(u8),
    PrimitiveData,
    Parameter(u8),
}
impl TryFrom<u8> for TARGET {
    type Error = RDNA2DecodeError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0..=7 => TARGET::RenderTarget(value),
            8 => TARGET::Z,
            9 => TARGET::Null,
            12..=15 => TARGET::Position(value - 12),
            20 => TARGET::PrimitiveData,
            32..=63 => TARGET::Parameter(value - 32),
            _ => {
                return Err(RDNA2DecodeError::BadValue(
                    "bad EXPORT TARGET",
                    value as u64,
                ))
            }
        })
    }
}
