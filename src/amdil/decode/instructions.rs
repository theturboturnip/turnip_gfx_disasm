use bitutils::bits;

use crate::amdil::{
    decode::decode_raw_token,
    enums::{ILImportUsage, ILInterpMode, ILOpCode},
    tokens::{IL_Opcode, IL_Src},
};

use super::{decode_token, AMDILDecodable, AMDILDecodeError, AMDILDest, AMDILSource};

#[derive(Debug)]
pub enum Instruction {
    Prefix {
        precise: [bool; 4],
        instruction: Box<Instruction>,
    },
    FlowControl(FlowControl),
    Declaration(Declaration),
}

#[derive(Debug)]
pub enum FlowControl {
    End,
}
#[derive(Debug)]
pub enum Declaration {
    UserData,
    GlobalFlags(u16),
    NonLiteralConstantBuffer {
        cb_idx: u8,
        len: u16,
    },
    LiteralConstantBuffer {
        icb_idx: u8,
        data: Vec<u32>,
    },
    Input {
        usage: ILImportUsage,
        interp: ILInterpMode,
        dst: AMDILDest,
    },
    Output {
        usage: ILImportUsage,
        dst: AMDILDest,
    },
    Literal {
        src: AMDILSource,
        bits: [u32; 4],
    },
}

impl AMDILDecodable for Instruction {
    fn from_stream(data: &[u8]) -> Result<(&[u8], Self), AMDILDecodeError> {
        let (data, opcode) = decode_token::<IL_Opcode>(data)?;

        use ILOpCode::*;
        match opcode.code {
            IL_OP_PREFIX => {
                let (data, prefixed_instr) = Instruction::from_stream(data)?;
                Ok((
                    data,
                    Self::Prefix {
                        precise: [
                            bits!(opcode.control, 0:0) == 1,
                            bits!(opcode.control, 1:1) == 1,
                            bits!(opcode.control, 2:2) == 1,
                            bits!(opcode.control, 3:3) == 1,
                        ],
                        instruction: Box::new(prefixed_instr),
                    },
                ))
            }
            // Flow Control
            IL_OP_END => Ok((data, Self::FlowControl(FlowControl::End))),

            // Declaration/Init
            IL_DCL_CONST_BUFFER => {
                if opcode.pri_modifier_present {
                    let icb_idx = todo!("Where is the ICB number defined? there's a raw 32-bit length where the Src would be :(");
                    let (mut data, len) = decode_raw_token(data)?;
                    let mut cb_data = vec![];
                    for i in 0..len {
                        let (new_data, cb_item) = decode_raw_token(data)?;
                        data = new_data;
                        cb_data.push(cb_item);
                    }
                    Ok((
                        data,
                        Self::Declaration(Declaration::LiteralConstantBuffer {
                            icb_idx,
                            data: cb_data,
                        }),
                    ))
                } else {
                    let (data, src) = decode_token::<IL_Src>(data)?;
                    todo!(
                        "Look at this IL_Src and figure out where m and n are defined {:?}",
                        src
                    )
                }
            }
            IL_DCL_GLOBAL_FLAGS => {
                todo!(
                    "Look at this IL_OpCode and figure out where the flags are {:?}",
                    opcode
                )
            }

            _ => panic!("unhandled instruction opcode: {:?}", opcode),
        }
    }
}
