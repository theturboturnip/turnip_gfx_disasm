#![allow(non_camel_case_types)]

use bitutils::bits;
use num_traits::FromPrimitive;

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
fn convert_to_opcode<T: FromPrimitive>(opcode: u8) -> Result<T, DecodeError> {
    T::from_u8(opcode)
        .ok_or_else(|| DecodeError::BadValue(std::any::type_name::<T>(), opcode.into()))
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
                    OP: convert_to_opcode(bits!(instr, 22:16) as u8)?,
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
                    OP: convert_to_opcode(bits!(instr, 22:16) as u8)?,
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
                    OP: convert_to_opcode(bits!(instr, 15:8) as u8)?,
                    SDST: bits!(instr, 22:16) as u8,
                    SSRC0,
                    extra_literal,
                },
            ))
        } else if bits!(instr, 31:28) == 0b1011 {
            Ok((
                &data[4..],
                Self::SOPK {
                    OP: convert_to_opcode(bits!(instr, 27:23) as u8)?,
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
                    OP: convert_to_opcode(bits!(instr, 29:23) as u8)?,
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

#[derive(Debug, Clone, Copy, FromPrimitive)]
pub enum SOP2_Opcode {
    S_ADD_U32 = 0,
    S_SUB_U32 = 1,
    S_ADD_I32 = 2,
    S_SUB_I32 = 3,
    S_ADDC_U32 = 4,
    S_SUBB_U32 = 5,
    S_MIN_I32 = 6,
    S_MIN_U32 = 7,
    S_MAX_I32 = 8,
    S_MAX_U32 = 9,
    S_CSELECT_B32 = 10,
    S_CSELECT_B64 = 11,
    S_AND_B32 = 14,
    S_AND_B64 = 15,
    S_OR_B32 = 16,
    S_OR_B64 = 17,
    S_XOR_B32 = 18,
    S_XOR_B64 = 19,
    S_ANDN2_B32 = 20,
    S_ANDN2_B64 = 21,
    S_ORN2_B32 = 22,
    S_ORN2_B64 = 23,
    S_NAND_B32 = 24,
    S_NAND_B64 = 25,
    S_NOR_B32 = 26,
    S_NOR_B64 = 27,
    S_XNOR_B32 = 28,
    S_XNOR_B64 = 29,
    S_LSHL_B32 = 30,
    S_LSHL_B64 = 31,
    S_LSHR_B32 = 32,
    S_LSHR_B64 = 33,
    S_ASHR_I32 = 34,
    S_ASHR_I64 = 35,
    S_BFM_B32 = 36,
    S_BFM_B64 = 37,
    S_MUL_I32 = 38,
    S_BFE_U32 = 39,
    S_BFE_I32 = 40,
    S_BFE_U64 = 41,
    S_BFE_I64 = 42,
    S_ABSDIFF_I32 = 44,
    S_LSHL1_ADD_U32 = 46,
    S_LSHL2_ADD_U32 = 47,
    S_LSHL3_ADD_U32 = 48,
    S_LSHL4_ADD_U32 = 49,
    S_PACK_LL_B32_B16 = 50,
    S_PACK_LH_B32_B16 = 51,
    S_PACK_HH_B32_B16 = 52,
    S_MUL_HI_U32 = 53,
    S_MUL_HI_I32 = 54,
}

#[derive(Debug, Clone, Copy, FromPrimitive)]
pub enum SOPK_Opcode {
    S_MOVK_I32 = 0,
    S_VERSION = 1,
    S_CMOVK_I32 = 2,
    S_CMPK_EQ_I32 = 3,
    S_CMPK_LG_I32 = 4,
    S_CMPK_GT_I32 = 5,
    S_CMPK_GE_I32 = 6,
    S_CMPK_LT_I32 = 7,
    S_CMPK_LE_I32 = 8,
    S_CMPK_EQ_U32 = 9,
    S_CMPK_LG_U32 = 10,
    S_CMPK_GT_U32 = 11,
    S_CMPK_GE_U32 = 12,
    S_CMPK_LT_U32 = 13,
    S_CMPK_LE_U32 = 14,
    S_ADDK_I32 = 15,
    S_MULK_I32 = 16,
    S_GETREG_B32 = 18,
    S_SETREG_B32 = 19,
    S_SETREG_IMM32_B32 = 21,
    S_CALL_B64 = 22,
    S_WAITCNT_VSCNT = 23,
    S_WAITCNT_VMCNT = 24,
    S_WAITCNT_EXPCNT = 25,
    S_WAITCNT_LGKMCNT = 26,
    S_SUBVECTOR_LOOP_BEGIN = 27,
    S_SUBVECTOR_LOOP_END = 28,
}

#[derive(Debug, Clone, Copy, FromPrimitive)]
pub enum SOP1_Opcode {
    S_MOV_B32 = 3,
    S_MOV_B64 = 4,
    S_CMOV_B32 = 5,
    S_CMOV_B64 = 6,
    S_NOT_B32 = 7,
    S_NOT_B64 = 8,
    S_WQM_B32 = 9,
    S_WQM_B64 = 10,
    S_BREV_B32 = 11,
    S_BREV_B64 = 12,
    S_BCNT0_I32_B32 = 13,
    S_BCNT0_I32_B64 = 14,
    S_BCNT1_I32_B32 = 15,
    S_BCNT1_I32_B64 = 16,
    S_FF0_I32_B32 = 17,
    S_FF0_I32_B64 = 18,
    S_FF1_I32_B32 = 19,
    S_FF1_I32_B64 = 20,
    S_FLBIT_I32_B32 = 21,
    S_FLBIT_I32_B64 = 22,
    S_FLBIT_I32 = 23,
    S_FLBIT_I32_I64 = 24,
    S_SEXT_I32_I8 = 25,
    S_SEXT_I32_I16 = 26,
    S_BITSET0_B32 = 27,
    S_BITSET0_B64 = 28,
    S_BITSET1_B32 = 29,
    S_BITSET1_B64 = 30,
    S_GETPC_B64 = 31,
    S_SETPC_B64 = 32,
    S_SWAPPC_B64 = 33,
    S_RFE_B64 = 34,
    S_AND_SAVEEXEC_B64 = 36,
    S_OR_SAVEEXEC_B64 = 37,
    S_XOR_SAVEEXEC_B64 = 38,
    S_ANDN2_SAVEEXEC_B64 = 39,
    S_ORN2_SAVEEXEC_B64 = 40,
    S_NAND_SAVEEXEC_B64 = 41,
    S_NOR_SAVEEXEC_B64 = 42,
    S_XNOR_SAVEEXEC_B64 = 43,
    S_QUADMASK_B32 = 44,
    S_QUADMASK_B64 = 45,
    S_MOVRELS_B32 = 46,
    S_MOVRELS_B64 = 47,
    S_MOVRELD_B32 = 48,
    S_MOVRELD_B64 = 49,
    S_ABS_I32 = 52,
    S_ANDN1_SAVEEXEC_B64 = 55,
    S_ORN1_SAVEEXEC_B64 = 56,
    S_ANDN1_WREXEC_B64 = 57,
    S_ANDN2_WREXEC_B64 = 58,
    S_BITREPLICATE_B64_B32 = 59,
    S_AND_SAVEEXEC_B32 = 60,
    S_OR_SAVEEXEC_B32 = 61,
    S_XOR_SAVEEXEC_B32 = 62,
    S_ANDN2_SAVEEXEC_B32 = 63,
    S_ORN2_SAVEEXEC_B32 = 64,
    S_NAND_SAVEEXEC_B32 = 65,
    S_NOR_SAVEEXEC_B32 = 66,
    S_XNOR_SAVEEXEC_B32 = 67,
    S_ANDN1_SAVEEXEC_B32 = 68,
    S_ORN1_SAVEEXEC_B32 = 69,
    S_ANDN1_WREXEC_B32 = 70,
    S_ANDN2_WREXEC_B32 = 71,
    S_MOVRELSD_2_B32 = 73,
}

#[derive(Debug, Clone, Copy, FromPrimitive)]
pub enum SOPC_Opcode {
    S_CMP_EQ_I32 = 0,
    S_CMP_LG_I32 = 1,
    S_CMP_GT_I32 = 2,
    S_CMP_GE_I32 = 3,
    S_CMP_LT_I32 = 4,
    S_CMP_LE_I32 = 5,
    S_CMP_EQ_U32 = 6,
    S_CMP_LG_U32 = 7,
    S_CMP_GT_U32 = 8,
    S_CMP_GE_U32 = 9,
    S_CMP_LT_U32 = 10,
    S_CMP_LE_U32 = 11,
    S_BITCMP0_B32 = 12,
    S_BITCMP1_B32 = 13,
    S_BITCMP0_B64 = 14,
    S_BITCMP1_B64 = 15,
    S_CMP_EQ_U64 = 18,
    S_CMP_LG_U64 = 19,
}

#[derive(Debug, Clone, Copy, FromPrimitive)]
pub enum SOPP_Opcode {
    S_NOP = 0,
    S_ENDPGM = 1,
    S_BRANCH = 2,
    S_WAKEUP = 3,
    S_CBRANCH_SCC0 = 4,
    S_CBRANCH_SCC1 = 5,
    S_CBRANCH_VCCZ = 6,
    S_CBRANCH_VCCNZ = 7,
    S_CBRANCH_EXECZ = 8,
    S_CBRANCH_EXECNZ = 9,
    S_BARRIER = 10,
    S_SETKILL = 11,
    S_WAITCNT = 12,
    S_SETHALT = 13,
    S_SLEEP = 14,
    S_SETPRIO = 15,
    S_SENDMSG = 16,
    S_SENDMSGHALT = 17,
    S_TRAP = 18,
    S_ICACHE_INV = 19,
    S_INCPERFLEVEL = 20,
    S_DECPERFLEVEL = 21,
    S_TTRACEDATA = 22,
    S_CBRANCH_CDBGSYS = 23,
    S_CBRANCH_CDBGUSER = 24,
    S_CBRANCH_CDBGSYS_OR_USER = 25,
    S_CBRANCH_CDBGSYS_AND_USER = 26,
    S_ENDPGM_SAVED = 27,
    S_ENDPGM_ORDERED_PS_DONE = 30,
    S_CODE_END = 31,
    S_INST_PREFETCH = 32,
    S_CLAUSE = 33,
    S_WAITCNT_DEPCTR = 35,
    S_ROUND_MODE = 36,
    S_DENORM_MODE = 37,
    S_TTRACEDATA_IMM = 40,
}
