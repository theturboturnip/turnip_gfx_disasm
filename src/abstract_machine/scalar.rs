use crate::Dependency;

use super::{DataRef, DataRefFilter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarOutput {
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

/// TODO more values here - special registers? function arguments?
///
/// TODO type information?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarDataRef {
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
    Output(ScalarOutput),
}
impl DataRef for ScalarDataRef {}

pub struct BasicScalarDataRefFilter {}
impl BasicScalarDataRefFilter {
    pub fn new() -> Self {
        Self {}
    }
}
impl DataRefFilter<ScalarDataRef> for BasicScalarDataRefFilter {
    fn is_pure_input(&self, v: &ScalarDataRef) -> bool {
        match v {
            ScalarDataRef::Literal(..) => true,
            _ => false,
        }
    }
}

pub type ScalarDependency = Dependency<ScalarDataRef>;