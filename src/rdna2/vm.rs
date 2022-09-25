use crate::{ScalarAction, ScalarOutcome};

use crate::abstract_machine::{ScalarAbstractVM, VMDataRef, VMElementRef, VMRef};

pub type RDNA2Action = Box<dyn ScalarAction<RDNA2AbstractVM>>;

#[derive(Debug, Clone)]
pub enum RDNA2AbstractVM {}
impl ScalarAbstractVM for RDNA2AbstractVM {
    type Action = RDNA2Action;
    type TScalarDataRef = RDNA2DataRef;
    type TElementDataRef = RDNA2DataRef;
}
pub type RDNA2Outcome = ScalarOutcome<RDNA2AbstractVM>;

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
impl VMRef for RDNA2DataRef {
    fn is_pure_input(&self) -> bool {
        match self {
            RDNA2DataRef::Literal(..) => true,
            _ => false,
        }
    }
}
impl VMDataRef for RDNA2DataRef {}
impl VMElementRef<RDNA2DataRef> for RDNA2DataRef {
    fn decompose(&self) -> Vec<RDNA2DataRef> {
        vec![self.clone()]
    }
}
