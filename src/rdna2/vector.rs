use bitutils::bits;

use super::{
    opcodes::{
        decode_opcode, VOP1_Opcode, VOP2_Opcode, VOP3A_Opcode, VOP3B_Opcode, VOP3P_Opcode,
        VOPC_Opcode,
    },
    utils::extract_u32,
    Decodable, DecodeError,
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
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), DecodeError> {
        // Read first 4 bytes, decide if we have an extra 32-bit literal constant
        let instr = extract_u32(data)?;

        if bits!(instr, 31:31) != 0 {
            Err(DecodeError::BadValue(
                "vector ALU major opcode",
                instr.into(),
            ))
        } else {
            let SRC0 = bits!(instr, 8:0) as u16;
            if bits!(instr, 31:25) == 0b0111110 {
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
                        OP: decode_opcode(bits!(instr, 24:17) as u8)?,
                        VSRC1: bits!(instr, 16:9) as u8,
                        SRC0: SRC0,
                        extra: extra,
                    },
                ))
            } else if bits!(instr, 31:25) == 0b0111111 {
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
                        OP: decode_opcode(bits!(instr, 16:9) as u8)?,
                        VDST: bits!(instr, 24:17) as u8,
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
                        OP: decode_opcode(bits!(instr, 30:25) as u8)?,
                        VSRC1: bits!(instr, 16:9) as u8,
                        VDST: bits!(instr, 24:17) as u8,
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
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), super::DecodeError> {
        todo!()
    }
}
