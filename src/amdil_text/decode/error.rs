use std::num::ParseIntError;

use nom::{error::{ParseError, ErrorKind}, IResult};
use thiserror::Error;

use super::grammar::CtrlSpec;

pub type NomGrammarResult<'a, T> = IResult<&'a str, T, GrammarError<&'a str>>;

#[derive(Debug, Error)]
pub enum GrammarError<I> {
    #[error("Internal Nom error: {1:?}")]
    Nom(I, ErrorKind),
    #[error("AMDIL semantic error: {0}")]
    AMDIL(#[from] AMDILError)
}
impl<I> ParseError<I> for GrammarError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        GrammarError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(Debug, Error)]
pub struct AMDILErrorContext {
    line: String,
    err: AMDILError,
}
impl AMDILErrorContext {
    pub fn new(line: &str, err: AMDILError) -> Self {
        Self {
            line: line.to_owned(),
            err
        }
    }
}
impl std::fmt::Display for AMDILErrorContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error {} in line '{}'", self.err, self.line)
    }
}

#[derive(Debug, Error)]
pub enum AMDILError {
    #[error("Error parsing integer: {0}")]
    ParseIntError(ParseIntError),
    #[error("Bad vector component '{0}'")]
    BadVectorComponent(char),
    #[error("Bad source modifier '{0}'")]
    BadSrcModifier(String),
    #[error("{0}")]
    Generic(String),
    #[error("Instruction '{0}' had problem: {1}")]
    InstructionError(&'static str, String),
    #[error("Unknown instruction '{0}' with controls '{1:?}'")]
    UnkInstruction(String, Vec<CtrlSpec>),
}
impl<I> From<nom::Err<GrammarError<I>>> for AMDILError {
    fn from(value: nom::Err<GrammarError<I>>) -> Self {
        match value {
            nom::Err::Incomplete(_) => panic!("AMDILError is not intended to be used with streaming"),
            nom::Err::Error(e) | nom::Err::Failure(e) => match e {
                GrammarError::Nom(state, err) => AMDILError::Generic(format!("Nom error {err:?}")),
                GrammarError::AMDIL(a) => a,
            }
        }
    }
}
impl<I> From<AMDILError> for nom::Err<GrammarError<I>> {
    fn from(value: AMDILError) -> Self {
        nom::Err::Error(GrammarError::AMDIL(value))
    }
}