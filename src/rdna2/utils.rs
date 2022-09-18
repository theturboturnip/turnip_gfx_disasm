use std::convert::TryInto;

use crate::abstract_machine::scalar::ScalarValueRef;

use super::RDNA2DecodeError;

pub fn extract_u32(data: &[u8]) -> Result<u32, RDNA2DecodeError> {
    if data.len() < 4 {
        Err(RDNA2DecodeError::NotEnoughData)
    } else {
        Ok((data[0] as u32)
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarInputOperand {
    ScalarValueRef(ScalarValueRef),
    Extra32BitConstant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VectorInputOperand {
    Base(ScalarInputOperand),
    DPP8,
    DPP8FI,
    SDWA,
    DPP16,
    LDSDirect,
}

pub fn decode_vector_src(src: u16, is_src_0: bool) -> Result<VectorInputOperand, RDNA2DecodeError> {
    Ok(match src {
        256..=511 => VectorInputOperand::Base(ScalarInputOperand::ScalarValueRef(
            ScalarValueRef::GeneralPurposeRegister(src as u64 - 256),
        )),
        233 => {
            if is_src_0 {
                VectorInputOperand::DPP8
            } else {
                return Err(RDNA2DecodeError::BadValue(
                    "Vector input DPP8 when not src0",
                    src as u64,
                ));
            }
        }
        234 => {
            if is_src_0 {
                VectorInputOperand::DPP8FI
            } else {
                return Err(RDNA2DecodeError::BadValue(
                    "Vector input DPP8FI when not src0",
                    src as u64,
                ));
            }
        }
        249 => {
            if is_src_0 {
                VectorInputOperand::SDWA
            } else {
                return Err(RDNA2DecodeError::BadValue(
                    "Vector input SDWA when not src0",
                    src as u64,
                ));
            }
        }
        250 => {
            if is_src_0 {
                VectorInputOperand::DPP16
            } else {
                return Err(RDNA2DecodeError::BadValue(
                    "Vector input DPP16 when not src0",
                    src as u64,
                ));
            }
        }
        254 => VectorInputOperand::LDSDirect,
        _ => VectorInputOperand::Base(decode_scalar_src(
            src.try_into().expect("16-bit src where 256..511 are "),
        )?),
    })
}

/// Based on Table 10/page 32
pub fn decode_scalar_src(src: u8) -> Result<ScalarInputOperand, RDNA2DecodeError> {
    use ScalarValueRef::*;
    Ok(ScalarInputOperand::ScalarValueRef(match src {
        0..=105 => GeneralPurposeGlobalRegister(src as u64 - 0),
        106 => SpecialReg {
            name: "VCC_LO",
            idx: 0,
        },
        107 => SpecialReg {
            name: "VCC_HI",
            idx: 0,
        },
        108..=123 => SpecialReg {
            name: "TTMP",
            idx: (src as u64 - 108),
        },
        124 => SpecialReg { name: "M0", idx: 0 },
        125 => SpecialReg {
            name: "NULL",
            idx: 0,
        },
        126 => SpecialReg {
            name: "EXEC_LO",
            idx: 0,
        },
        127 => SpecialReg {
            name: "EXEC_HI",
            idx: 0,
        },
        128 => Literal(0),
        129..=192 => Literal(((src as u64 as i64) - 128) as u64),
        193..=208 => Literal((192 - (src as u64 as i64)) as u64),
        235 => SpecialReg {
            name: "SHARED_BASE",
            idx: 0,
        },
        236 => SpecialReg {
            name: "SHARED_LIMIT",
            idx: 0,
        },
        237 => SpecialReg {
            name: "PRIVATE_BASE",
            idx: 0,
        },
        238 => SpecialReg {
            name: "PRIVATE_LIMIT",
            idx: 0,
        },
        239 => SpecialReg {
            name: "POPS_EXITING_WAVE_ID",
            idx: 0,
        },
        240 => Literal(0.5_f64.to_bits()),
        241 => Literal((-0.5_f64).to_bits()),
        242 => Literal(1.0_f64.to_bits()),
        243 => Literal((-1.0_f64).to_bits()),
        244 => Literal(2.0_f64.to_bits()),
        245 => Literal((-2.0_f64).to_bits()),
        246 => Literal(4.0_f64.to_bits()),
        247 => Literal((-4.0_f64).to_bits()),
        248 => Literal((1.0 / (2.0 * std::f64::consts::PI)).to_bits()),
        251 => SpecialReg {
            name: "VCCZ",
            idx: 0,
        },
        252 => SpecialReg {
            name: "EXECZ",
            idx: 0,
        },
        253 => SpecialReg {
            name: "SCC",
            idx: 0,
        },
        255 => return Ok(ScalarInputOperand::Extra32BitConstant),
        209..=234 | 249..=250 | 254 => {
            return Err(RDNA2DecodeError::BadValue(
                "scalar src (8-bit) RESERVED",
                src as u64,
            ))
        }
    }))
}
