#![allow(non_camel_case_types)]
use std::convert::TryFrom;

use bitutils::bits;

use crate::{
    abstract_machine::{
        scalar::{ScalarDataRef, ScalarDependency, ScalarOutput},
        DataKind, DataWidth, ValueRef,
    },
    Action, Dependency,
};

use super::{utils::extract_u32, Decodable, RDNA2DecodeError};

/// Chapter 11, p98
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
            let EN = bits!(instr, 0:3) as u8;
            let COMPR = bits!(instr, 10:10) != 0;

            if COMPR {
                match EN {
                    0x0 | 0x3 | 0xC | 0xF => {}
                    _ => {
                        return Err(RDNA2DecodeError::BadValue(
                            "COMPRessed EXPORT has bad EN: must be 0x0,3,C,F",
                            EN as u64,
                        ))
                    }
                }
            }

            Ok((
                &data[8..],
                Self {
                    EN,
                    TARGET: (bits!(instr, 4:9) as u8).try_into()?,
                    COMPR,
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
impl Action<ScalarDataRef> for EXPORT {
    fn dependencies(&self) -> Vec<ScalarDependency> {
        let mut deps = vec![];
        let possible_exports = if self.COMPR {
            [self.VSRC0, self.VSRC0, self.VSRC1, self.VSRC1]
        } else {
            [self.VSRC0, self.VSRC1, self.VSRC2, self.VSRC3]
        };
        for i in 0..4 {
            // If bit i is not set, continue
            if self.EN & (1 << i) == 0 {
                continue;
            }

            // bit i is set => output #i is enabled

            let output_ref = match self.TARGET {
                TARGET::Position(idx) => ScalarOutput::VertPosition {
                    idx: idx as u64,
                    vector_comp: i,
                },
                TARGET::Parameter(idx) => ScalarOutput::VertParameter {
                    idx: idx as u64,
                    vector_comp: i,
                },
                TARGET::RenderTarget(rt) => ScalarOutput::FragColor {
                    idx: rt as u64,
                    vector_comp: i,
                },
                TARGET::Z => ScalarOutput::Other {
                    name: "Z",
                    idx: 0,
                    vector_comp: i,
                },
                TARGET::PrimitiveData => ScalarOutput::Other {
                    name: "PrimitiveData",
                    idx: 0,
                    vector_comp: i,
                },
                TARGET::Null => continue,
            };

            let width = if self.COMPR {
                DataWidth::E16
            } else {
                DataWidth::E32
            };

            deps.push(Dependency::new(
                vec![ValueRef {
                    data: ScalarDataRef::GeneralPurposeRegister(possible_exports[i] as u64),
                    kind: DataKind::Untyped,
                    width,
                }],
                ValueRef {
                    data: ScalarDataRef::Output(output_ref),
                    kind: DataKind::Untyped,
                    width,
                },
            ));
        }

        deps
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
