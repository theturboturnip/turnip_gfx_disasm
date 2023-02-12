use std::{cell::RefCell, rc::Rc};

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent};

use self::{
    syntax::{UnconcreteOpResult, UnconcreteOpTarget},
    types::HLSLType,
};

pub mod compat;
mod display;
pub mod syntax;
pub mod types;

/// An unswizzled vector available to operations in the HLSL virtual machine.
/// See [HLSLVariableInfo].
pub type HLSLVariable = Rc<RefCell<HLSLVariableInfo>>;

/// Metadata of [HLSLVariable] i.e. an unswizzled vector available to operations in the HLSL virtual machine
#[derive(Debug)]
pub struct HLSLVariableInfo {
    pub vector_name: HLSLVectorName,
    pub kind: HLSLType,
    pub n_components: u8,
}

/// The name of an unswizzled vector in the HLSL virtual machine
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HLSLVectorName {
    GenericRegister(u64),
    ShaderInput(String),
    ShaderOutput(String),
    Literal([u64; 4]),
    // TODO read/write permissions for ArrayElement?
    ArrayElement { of: Box<Self>, idx: u64 },
}

/// A reference to a single scalar in the HLSL virtual machine
pub type HLSLScalarDataRef = (HLSLVariable, VectorComponent);
/// A reference to a swizzled vector in the HLSL virtual machine
pub type HLSLVectorDataRef = (HLSLVariable, MaskedSwizzle);
impl UnconcreteOpTarget for HLSLVectorDataRef {}

/// The outcome of an action in the HLSL virtual machine
#[derive(Debug, Clone)]
pub enum HLSLOutcome {
    /// State that a new variable exists without setting its value
    Declaration { new_var: HLSLVariable },
    /// State that a new variable exists and has a given value taken directly from other variables
    Definition {
        new_var: HLSLVariable,
        components: Vec<HLSLScalarDataRef>,
    },
    /// State that the output of an operation has been assigned to some components of a variable
    Operation {
        op: UnconcreteOpResult<HLSLVectorDataRef>,
        // Mapping of each individual scalar output to each individual scalar input
        scalar_deps: Vec<(HLSLScalarDataRef, Vec<HLSLScalarDataRef>)>,
    },
    /// State that the program flow may end early due to some vector components
    EarlyOut { inputs: Vec<HLSLScalarDataRef> },
}
