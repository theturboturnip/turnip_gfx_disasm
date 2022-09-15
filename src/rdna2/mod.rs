//! This backend is based on the RDNA2 shader ISA as described in [the 2020 AMD ISA document](https://developer.amd.com/wp-content/resources/RDNA2_Shader_ISA_November2020.pdf).
//! All references to pages and sections are relative to that document unless otherwise stated.
//!
//! Note: this assumes binaries are LITTLE-ENDIAN!

#![allow(non_snake_case)]

use std::marker::PhantomData;

use bitutils::bits;

use super::Action;

enum DecodeError {
    NotApplicable,
    NotEnoughData,
    BadOpcode(u8),
}

trait Decodable: Sized {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), DecodeError>;
}

/// A decoded instruction. Top-level enum has archetypes, each archetype contains
/// an enum or struct describing the actual instruction under that archetype.
/// If a top-level enum doesn't have any contents, it hasn't been implemented yet.
///
/// Top-level archetypes taken from Table 62/page 236
#[derive(Debug, Clone, Copy)]
enum Instruction {
    ScalarALU(ScalarALUInstr),
    ScalarMemory(),
    VectorALU(),
    /// 64-bit version of VectorALU
    VectorALU_Long(),
    VectorParamInterp(),
    DataSharing(),
    VectorMemoryBuffer(),
    VectorMemoryImage(),
    Export(),
    FlatMemoryAccess(),
}
impl Decodable for Instruction {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), DecodeError> {
        // instruction decode "tag" bits are bits from [32:25], even for 64-bit instructions
        // read four bytes, take those 6 bits and use those to decide what instruction format this is
        if data.len() < 4 {
            return Err(DecodeError::NotEnoughData);
        }

        // Binaries are little-endian
        // take top 6 bits of fourth byte
        let opcode = data[3] >> 2;
        match opcode {
            // 0b10_XXXX = ScalarALU
            0b10_0000..=0b10_1111 => {
                let (remaining, instr) = ScalarALUInstr::decode_consuming(data)?;
                Ok((remaining, Instruction::ScalarALU(instr)))
            }
            // 0b111101 = SMEM
            0b111101 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::ScalarMemory()))
            }
            0b0_00000..=0b0_11111 => {
                todo!("VOP2 | VOP1 | VOPC - may be followed by SDWA, SDWAB, DPP16, DPP8")
            }
            0b110001 | 0b110101 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::VectorALU_Long()))
            }
            0b110010 => Ok((&data[4..], Instruction::VectorParamInterp())),
            0b110110 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::DataSharing()))
            }
            0b1110_00 | 0b1110_10 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::DataSharing()))
            }
            0b111100 => {
                todo!("MIMG (variable length)");
            }
            0b110111 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::FlatMemoryAccess()))
            }
            0b111110 => {
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                Ok((&data[8..], Instruction::Export()))
            }
            // anything beyond 6-bits is wrong
            0b1_000000 => panic!(
                "got value 0b{:b} which is larger than 6 bits from a (8-bit >> 2) shift",
                opcode
            ),
            _ => Err(DecodeError::BadOpcode(opcode)),
        }
    }
}
impl Action for Instruction {
    fn dependencies(&self) -> Vec<crate::Dependency> {
        match self {
            Self::ScalarALU(instr) => instr.dependencies(),
            _ => panic!("Action not implemented for {:?} yet", self),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ScalarALUInstr {
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
        if data.len() < 4 {
            return Err(DecodeError::NotEnoughData);
        }

        let instr: u32 = (data[0] as u32)
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24);

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
                // use literal constant
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                (
                    8,
                    Some(
                        (data[4] as u32)
                            | ((data[5] as u32) << 8)
                            | ((data[6] as u32) << 16)
                            | ((data[7] as u32) << 24),
                    ),
                )
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
                // use literal constant
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                (
                    8,
                    Some(
                        (data[4] as u32)
                            | ((data[5] as u32) << 8)
                            | ((data[6] as u32) << 16)
                            | ((data[7] as u32) << 24),
                    ),
                )
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
                // use literal constant
                if data.len() < 8 {
                    return Err(DecodeError::NotEnoughData);
                }
                (
                    8,
                    Some(
                        (data[4] as u32)
                            | ((data[5] as u32) << 8)
                            | ((data[6] as u32) << 16)
                            | ((data[7] as u32) << 24),
                    ),
                )
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

struct Decoder<'a> {
    _lifetime: PhantomData<&'a ()>, // NOTE: there's no generic type here!
}
impl<'a> super::Decoder for Decoder<'a> {
    type Input = &'a [u8];
    type BaseAction = Box<dyn Action>;
    type Err = DecodeError;

    fn decode(&self, mut data: Self::Input) -> Result<Vec<Self::BaseAction>, DecodeError> {
        loop {
            let (consumed_data, instr) = Instruction::decode_consuming(data)?;
            println!("{:?}", instr);
            data = consumed_data;
        }
    }
}
