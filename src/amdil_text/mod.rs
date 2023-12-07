use std::marker::PhantomData;

use nom::Finish;

use crate::{Decoder, Program, abstract_machine::VMName, Action};

mod decode;
mod grammar;
pub mod vm;

use self::{decode::{AMDILTextDecodeError, Instruction}, grammar::AMDILTextParseError, vm::{AMDILAbstractVM, AMDILRegister}};

/// The type returned by [AMDILDecoder] holding the instructions for a given AMDIL program
pub struct AMDILProgram {
    io_registers: Vec<AMDILRegister>,
    actions: Vec<Action<AMDILAbstractVM>>
}
impl Program<AMDILAbstractVM> for AMDILProgram {
    fn io_declarations(&self) -> &Vec<<AMDILAbstractVM as crate::AbstractVM>::Register> {
        &self.io_registers
    }
    fn actions(&self) -> &Vec<Action<AMDILAbstractVM>> {
        &self.actions
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
        let mut actions = vec![];
        let mut io_registers = vec![];
        for g_i in g_instrs {
            let i = decode::decode_instruction(g_i)?;
            i.push_actions(&mut actions);
            match i {
                Instruction::Decl(decl) => {
                    match decl.get_decl().filter(|r| r.is_pure_input() || r.is_output()) {
                        Some(io_reg) => io_registers.push(io_reg),
                        None => {}
                    }
                },
                _ => {}
            }
        }

        // Return
        Ok(AMDILProgram {
            io_registers,
            actions,
        })
    }
}
