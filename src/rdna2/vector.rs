use super::{
    opcodes::{VOP1_Opcode, VOP2_Opcode, VOP3A_Opcode, VOP3B_Opcode, VOP3P_Opcode, VOPC_Opcode},
    Decodable,
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
    fn decode_consuming(data: &[u8]) -> Result<(&[u8], Self), super::DecodeError> {
        todo!()
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
