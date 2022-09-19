use std::convert::TryInto;

use num_traits::FromPrimitive;

use self::instructions::Instruction;

use super::tokens::{
    IL_Dst, IL_Dst_Mod, IL_Lang, IL_Opcode, IL_Rel_Addr, IL_Src, IL_Src_Mod, IL_Version,
    RelativeAddressing,
};

pub mod instructions;

#[derive(Debug)]
pub enum AMDILDecodeError {
    BadValue(&'static str, u64),
    NotEnoughData,
    MajorVersionMismatch { expected: u64, actual: u64 },
}

pub fn decode_enum<T: FromPrimitive>(bits: u32) -> Result<T, AMDILDecodeError> {
    T::from_u32(bits)
        .ok_or_else(|| AMDILDecodeError::BadValue(std::any::type_name::<T>(), bits.into()))
}

fn decode_raw_token<'a>(data: &'a [u8]) -> Result<(&'a [u8], u32), AMDILDecodeError> {
    if data.len() < 4 {
        Err(AMDILDecodeError::NotEnoughData)
    } else {
        // little-endian u32
        let val = data[0] as u32
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24);
        Ok((&data[4..], val))
    }
}

fn decode_token<'a, T: TryFrom<u32, Error = AMDILDecodeError>>(
    data: &'a [u8],
) -> Result<(&'a [u8], T), AMDILDecodeError> {
    let (data, raw_tok) = decode_raw_token(data)?;
    Ok((data, raw_tok.try_into()?))
}

trait AMDILDecodable: Sized {
    fn from_stream(data: &[u8]) -> Result<(&[u8], Self), AMDILDecodeError>;
}

pub struct AMDILProgram {
    lang: IL_Lang,
    version: IL_Version,
    ops: Vec<Instruction>,
}
impl AMDILDecodable for AMDILProgram {
    fn from_stream(data: &[u8]) -> Result<(&[u8], Self), AMDILDecodeError> {
        let (data, lang) = decode_token(data)?;
        let (data, version) = decode_token(data)?;

        let mut ops = vec![];
        let mut data = data;
        loop {
            if data.len() == 0 {
                break;
            }
            let (new_data, op) = Instruction::from_stream(data)?;
            dbg!(&op);
            data = new_data;
            ops.push(op);
        }

        Ok((data, Self { lang, version, ops }))
    }
}

pub fn decode_amdil_program(data: &[u8]) -> Result<AMDILProgram, AMDILDecodeError> {
    let (_, program) = AMDILProgram::from_stream(data)?;
    Ok(program)
}

// struct AMDILOperation {
//     opcode: IL_Opcode,
//     dest: Option<AMDILDest>,
//     srcs: Vec<AMDILSource>,
//     /// If this is set, self.opcode.code == IL_Prefix_OpCode, and `self.opcode.control` influences operation precision of prefixed_operation
//     prefixed_operation: Option<Box<AMDILOperation>>,
//     extra_tokens: Vec<u32>,
// }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AMDILDest {
    base: IL_Dst,
    modifier: Option<IL_Dst_Mod>,
    rel_addr: Option<IL_Rel_Addr>,
    immediate: Option<u32>,
    extended: Option<u32>,
    next_dimension: Option<Box<AMDILDest>>,
}
impl AMDILDecodable for AMDILDest {
    fn from_stream(data: &[u8]) -> Result<(&[u8], Self), AMDILDecodeError> {
        let (data, base) = decode_token::<IL_Dst>(data)?;

        let (data, modifier) = if base.modifier_present {
            let (data, modifier) = decode_token(data)?;
            (data, Some(modifier))
        } else {
            (data, None)
        };

        // For Src, when modifiers and rel_addr are used together, modifier comes first
        // Assume that holds for Dst too

        let (data, rel_addr) = if base.relative_address == RelativeAddressing::Relative {
            let (data, rel_addr) = decode_token(data)?;
            (data, Some(rel_addr))
        } else {
            (data, None)
        };

        // I *think* this should only be true iff:
        //  - We are the first token in the dimension-chain, and relative_address == RelativeAddressing::RegisterRelative
        //  - We are a subsequent token in the dimension-chain, and there are more dimensions left
        // i.e. you could have the src tokens (relative_address = RegisterRelative, register = 4), (dimension = 1, register = 3), (dimension = 0, register = 5)
        // for x[4][3][5]
        // where x is determined by base.register_type
        let (data, next_dimension) = if base.dimension {
            println!(
                "WARNING - IL_Dst {:?} has an extra dimension - unsure how this works, things might break",
                base
            );
            let (data, next_dimension) = AMDILDest::from_stream(data)?;
            (data, Some(Box::new(next_dimension)))
        } else {
            (data, None)
        };

        // In the immediate_present documentation:
        // "A 32-bit value containing the immediate value follows this token, modifier
        // tokens, and src tokens used in register relative addressing."
        let (data, immediate) = if base.immediate_present {
            let (data, immediate) = decode_raw_token(data)?;
            (data, Some(immediate))
        } else {
            (data, None)
        };

        let (data, extended) = if base.extended {
            println!(
                "WARNING - IL_Dst {:?} is extended - unsure how this works, things might break",
                base
            );
            let (data, extended) = decode_raw_token(data)?;
            (data, Some(extended))
        } else {
            (data, None)
        };

        Ok((
            data,
            Self {
                base,
                modifier,
                rel_addr,
                immediate,
                extended,
                next_dimension,
            },
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AMDILSource {
    base: IL_Src,
    modifier: Option<IL_Src_Mod>,
    rel_addr: Option<IL_Rel_Addr>,
    immediate: Option<u32>,
    extended: Option<u32>,
    next_dimension: Option<Box<AMDILSource>>,
}
impl AMDILDecodable for AMDILSource {
    fn from_stream(data: &[u8]) -> Result<(&[u8], Self), AMDILDecodeError> {
        let (data, base) = decode_token::<IL_Src>(data)?;

        let (data, modifier) = if base.modifier_present {
            let (data, modifier) = decode_token(data)?;
            (data, Some(modifier))
        } else {
            (data, None)
        };

        // For Src, when modifiers and rel_addr are used together, modifier comes first

        let (data, rel_addr) = if base.relative_address == RelativeAddressing::Relative {
            let (data, rel_addr) = decode_token(data)?;
            (data, Some(rel_addr))
        } else {
            (data, None)
        };

        // I *think* this should only be true iff:
        //  - We are the first token in the dimension-chain, and relative_address == RelativeAddressing::RegisterRelative
        //  - We are a subsequent token in the dimension-chain, and there are more dimensions left
        // i.e. you could have the src tokens (relative_address = RegisterRelative, register = 4), (dimension = 1, register = 3), (dimension = 0, register = 5)
        // for x[4][3][5]
        // where x is determined by base.register_type
        let (data, next_dimension) = if base.dimension {
            println!(
                "WARNING - IL_Src {:?} has an extra dimension - unsure how this works, things might break",
                base
            );
            let (data, next_dimension) = AMDILSource::from_stream(data)?;
            (data, Some(Box::new(next_dimension)))
        } else {
            (data, None)
        };

        // In the immediate_present documentation:
        // "A 32-bit value containing the immediate value follows this token, modifier
        // tokens, and src tokens used in register relative addressing."
        let (data, immediate) = if base.immediate_present {
            let (data, immediate) = decode_raw_token(data)?;
            (data, Some(immediate))
        } else {
            (data, None)
        };

        let (data, extended) = if base.extended {
            println!(
                "WARNING - IL_Src {:?} is extended - unsure how this works, things might break",
                base
            );
            let (data, extended) = decode_raw_token(data)?;
            (data, Some(extended))
        } else {
            (data, None)
        };

        Ok((
            data,
            Self {
                base,
                modifier,
                rel_addr,
                immediate,
                extended,
                next_dimension,
            },
        ))
    }
}
