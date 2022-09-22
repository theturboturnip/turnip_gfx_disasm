use crate::Outcome;

use crate::abstract_machine::{AbstractVM, DataRef, ScalarBasedAbstractVM};

#[derive(Debug)]
pub enum RDNA2AbstractVM {}
impl AbstractVM for RDNA2AbstractVM {
    type TScalarDataRef = RDNA2DataRef;
}
impl ScalarBasedAbstractVM for RDNA2AbstractVM {}
pub type RDNA2Outcome = Outcome<RDNA2AbstractVM>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RDNA2Output {
    VertPosition {
        idx: u64,
        vector_comp: usize,
    },
    VertParameter {
        idx: u64,
        vector_comp: usize,
    },
    FragColor {
        idx: u64,
        vector_comp: usize,
    },
    Other {
        name: &'static str,
        idx: u64,
        vector_comp: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RDNA2DataRef {
    /// 64-bit scalar per-executor-group register
    GeneralPurposeGlobalRegister(u64),
    /// 64-bit scalar per-executor register
    GeneralPurposeRegister(u64),
    /// Value from memory (TODO: Add refs to actual included values and make copyable?)
    Memory(),
    /// 64-bit literal value
    Literal(u64),
    /// Register with special meaning
    SpecialReg { name: &'static str, idx: u64 },
    /// Refers to a specific component of a fragment shader's vector output.
    ///
    /// Fragment shaders are a special case because they usually write to vectors
    /// e.g. output color, so we have a component index here.
    Output(RDNA2Output),
}
impl DataRef for RDNA2DataRef {
    fn is_pure_input(&self) -> bool {
        match self {
            RDNA2DataRef::Literal(..) => true,
            _ => false,
        }
    }
}
