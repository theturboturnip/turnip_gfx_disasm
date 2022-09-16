#![allow(non_camel_case_types)]
use bitutils::bits;

use super::{
    opcodes::{
        decode_opcode, VOP1_Opcode, VOP2_Opcode, VOP3A_Opcode, VOP3B_Opcode, VOP3P_Opcode,
        VOPC_Opcode,
    },
    utils::extract_u32,
    Decodable, RDNA2DecodeError,
};

/// TODO
#[derive(Debug, Clone, Copy)]
pub struct SDWA(u32);
/// TODO
#[derive(Debug, Clone, Copy)]
pub struct SDWAB(u32);
/// TODO
#[derive(Debug, Clone, Copy)]
pub struct DPP16(u32);
/// TODO
#[derive(Debug, Clone, Copy)]
pub struct DPP8(u32);

#[derive(Debug, Clone, Copy)]
pub enum VOP2_Extra {
    SDWA(SDWA),
    DPP8(DPP8),
    DPP16(DPP16),
}

#[derive(Debug, Clone, Copy)]
pub enum VOP1_Extra {
    SDWA(SDWA),
    DPP8(DPP8),
    DPP16(DPP16),
}

#[derive(Debug, Clone, Copy)]
pub enum VOPC_Extra {
    SDWAB(SDWAB),
    DPP8(DPP8),
    DPP16(DPP16),
}

#[derive(Debug, Clone, Copy)]
pub enum VOP {
    VOP2 {
        OP: VOP2_Opcode,
        VDST: u8,
        VSRC1: u8,
        SRC0: u16,
        extra: Option<VOP2_Extra>,
    },
    VOP1 {
        OP: VOP1_Opcode,
        VDST: u8,
        SRC0: u16,
        extra: Option<VOP1_Extra>,
    },
    VOPC {
        OP: VOPC_Opcode,
        VSRC1: u8,
        SRC0: u16,
        extra: Option<VOPC_Extra>,
    },
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
            let SRC0 = bits!(instr, 0:8) as u16;
            if bits!(instr, 25:31) == 0b0111110 {
                // VOPC
                let (length, extra) = match SRC0 {
                    // 233 = DPP8, 234 = DPP8FI?
                    233 | 234 => (8, Some(VOPC_Extra::DPP8(DPP8(extract_u32(&data[4..])?)))),
                    // 250 = DPP16
                    250 => (8, Some(VOPC_Extra::DPP16(DPP16(extract_u32(&data[4..])?)))),
                    // 249 = SDWA
                    249 => (8, Some(VOPC_Extra::SDWAB(SDWAB(extract_u32(&data[4..])?)))),
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOPC {
                        OP: decode_opcode(bits!(instr, 17:24))?,
                        VSRC1: bits!(instr, 9:16) as u8,
                        SRC0: SRC0,
                        extra: extra,
                    },
                ))
            } else if bits!(instr, 25:31) == 0b0111111 {
                // VOP1
                let (length, extra) = match SRC0 {
                    // 233 = DPP8, 234 = DPP8FI?
                    233 | 234 => (8, Some(VOP1_Extra::DPP8(DPP8(extract_u32(&data[4..])?)))),
                    // 250 = DPP16
                    250 => (8, Some(VOP1_Extra::DPP16(DPP16(extract_u32(&data[4..])?)))),
                    // 249 = SDWA
                    249 => (8, Some(VOP1_Extra::SDWA(SDWA(extract_u32(&data[4..])?)))),
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOP1 {
                        OP: decode_opcode(bits!(instr, 9:16))?,
                        VDST: bits!(instr, 17:24) as u8,
                        SRC0: SRC0,
                        extra: extra,
                    },
                ))
            } else {
                // VOP2
                let (length, extra) = match SRC0 {
                    // 233 = DPP8, 234 = DPP8FI?
                    233 | 234 => (8, Some(VOP2_Extra::DPP8(DPP8(extract_u32(&data[4..])?)))),
                    // 250 = DPP16
                    250 => (8, Some(VOP2_Extra::DPP16(DPP16(extract_u32(&data[4..])?)))),
                    // 249 = SDWA
                    249 => (8, Some(VOP2_Extra::SDWA(SDWA(extract_u32(&data[4..])?)))),
                    // All others = normal
                    _ => (4, None),
                };
                Ok((
                    &data[length..],
                    Self::VOP2 {
                        OP: decode_opcode(bits!(instr, 25:30))?,
                        VSRC1: bits!(instr, 9:16) as u8,
                        VDST: bits!(instr, 17:24) as u8,
                        SRC0: SRC0,
                        extra: extra,
                    },
                ))
            }
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
        SRC2: u16,
        SRC1: u16,
        SRC0: u16,
    },
    VOP3B {
        OP: VOP3B_Opcode,
        CLMP: bool,
        SDST: u8,
        VDST: u8,
        NEG: u8,
        OMOD: u8,
        SRC2: u16,
        SRC1: u16,
        SRC0: u16,
    },
    VOP3P {
        OP: VOP3P_Opcode,
        CLMP: bool,
        OP_SEL: u8,
        NEG_HI: u8,
        VDST: u8,
        NEG: u8,
        OP_SEL_HI: u8,
        SRC2: u16,
        SRC1: u16,
        SRC0: u16,
    },
}
impl Decodable for VOP3 {
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), super::RDNA2DecodeError> {
        // Always 64bits
        let instr = extract_u32(data)?;
        let instr_top = extract_u32(&data[4..])?;

        match bits!(instr, 26:31) {
            0b110101 => {
                // VOP3A or VOP3B

                let opcode = bits!(instr, 16:25);

                // Try decoding as VOP3A, then if that doesn't work try VOP3B
                if let Ok(OP) = decode_opcode::<VOP3A_Opcode>(opcode) {
                    Ok((
                        &data[8..],
                        Self::VOP3A {
                            OP,
                            CLMP: bits!(instr, 15:15) != 0,
                            OP_SEL: bits!(instr, 11:14) as u8,
                            ABS: bits!(instr, 8:10) as u8,
                            VDST: bits!(instr, 0:7) as u8,
                            NEG: bits!(instr_top, 29:31) as u8,
                            OMOD: bits!(instr_top, 27:28) as u8,
                            SRC2: bits!(instr_top, 18:26) as u16,
                            SRC1: bits!(instr_top, 9:17) as u16,
                            SRC0: bits!(instr_top, 0:8) as u16,
                        },
                    ))
                } else {
                    let OP = decode_opcode(opcode)?;

                    Ok((
                        &data[8..],
                        Self::VOP3B {
                            OP,
                            CLMP: bits!(instr, 15:15) != 0,
                            SDST: bits!(instr, 8:14) as u8,
                            VDST: bits!(instr, 0:7) as u8,
                            NEG: bits!(instr_top, 29:31) as u8,
                            OMOD: bits!(instr_top, 27:28) as u8,
                            SRC2: bits!(instr_top, 18:26) as u16,
                            SRC1: bits!(instr_top, 9:17) as u16,
                            SRC0: bits!(instr_top, 0:8) as u16,
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
                    &data[8..],
                    Self::VOP3P {
                        OP: decode_opcode(opcode)?,
                        CLMP: bits!(instr, 15:15) != 0,
                        OP_SEL: bits!(instr, 11:13) as u8,
                        NEG_HI: bits!(instr, 8:10) as u8,
                        VDST: bits!(instr, 0:7) as u8,
                        NEG: bits!(instr_top, 29:31) as u8,
                        OP_SEL_HI,
                        SRC2: bits!(instr_top, 18:26) as u16,
                        SRC1: bits!(instr_top, 9:17) as u16,
                        SRC0: bits!(instr_top, 0:8) as u16,
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
