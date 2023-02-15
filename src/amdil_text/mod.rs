use std::marker::PhantomData;

use nom::Finish;

use crate::{Decoder, Program};

mod decode;
mod grammar;
pub mod vm;
pub use vm::AMDILAction;

use self::{decode::AMDILTextDecodeError, grammar::AMDILTextParseError, vm::AMDILAbstractVM};

/// The type returned by [AMDILDecoder] holding the instructions for a given AMDIL program
pub type AMDILProgram = Vec<AMDILAction>;
impl Program<AMDILAbstractVM> for AMDILProgram {
    fn actions(&self) -> &Vec<AMDILAction> {
        self
    }
}

/// Combined error type for [AMDILTextParseError] and [AMDILTextDecodeError]
#[derive(Debug, Clone)]
pub enum AMDILDecodeError {
    ParseNomError(String, nom::error::ErrorKind),
    ParseIntError(std::num::ParseIntError),
    ParseBadVectorComponent(char),

    DecodedBadValue(&'static str, grammar::Instruction),
    GenericDecodeError(String),
}
impl<'a> From<AMDILTextParseError<&'a str>> for AMDILDecodeError {
    fn from(err: AMDILTextParseError<&'a str>) -> Self {
        match err {
            AMDILTextParseError::Nom(msg, kind) => {
                AMDILDecodeError::ParseNomError(msg.to_owned(), kind)
            }
            AMDILTextParseError::ParseIntError(int_err) => AMDILDecodeError::ParseIntError(int_err),
            AMDILTextParseError::BadVectorComponent(comp) => {
                AMDILDecodeError::ParseBadVectorComponent(comp)
            }
        }
    }
}
impl From<AMDILTextDecodeError> for AMDILDecodeError {
    fn from(err: AMDILTextDecodeError) -> Self {
        match err {
            AMDILTextDecodeError::BadValue(msg, instruction) => {
                AMDILDecodeError::DecodedBadValue(msg, instruction)
            }
            AMDILTextDecodeError::Generic(msg) => AMDILDecodeError::GenericDecodeError(msg),
        }
    }
}

/// Decoder for AMDIL text disassembly
pub struct AMDILDecoder<'a> {
    _lifetime: PhantomData<&'a ()>, // NOTE: there's no generic type here!
}
impl<'a> AMDILDecoder<'a> {
    pub fn new() -> AMDILDecoder<'a> {
        AMDILDecoder {
            _lifetime: PhantomData::default(),
        }
    }
}
impl<'a> Decoder<AMDILAbstractVM> for AMDILDecoder<'a> {
    type Input = &'a str;
    type Program = AMDILProgram;
    type Err = AMDILDecodeError;

    fn decode(&self, data: Self::Input) -> Result<AMDILProgram, AMDILDecodeError> {
        // Parse
        let (_, g_instrs) = grammar::parse_lines(data).finish()?;

        // Decode
        let instrs = g_instrs
            .into_iter()
            .map(decode::decode_instruction)
            .collect::<Result<Vec<AMDILAction>, _>>()?;

        // Return
        Ok(instrs)
    }
}
