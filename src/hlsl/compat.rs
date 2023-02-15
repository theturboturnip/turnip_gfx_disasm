use crate::{
    abstract_machine::{
        vector::{MaskedSwizzle, VECTOR_COMPONENTS},
        AbstractVM, TypedVMRef, VMScalarDataRef, VMVectorNameRef,
    },
    Action,
};

use super::{
    syntax::{UnconcreteOpResult, UnconcreteOpTarget},
    types::HLSLType,
};

pub trait HLSLCompatibleVectorNameRef: VMVectorNameRef {
    fn as_hlsl(&self) -> HLSLNameRefType;
}

/// An HLSL-compatible specification for a reference to a swizzled vector
#[derive(Debug, Clone)]
pub struct HLSLDataRefSpec<TName: VMVectorNameRef> {
    pub vm_name_ref: TName,
    pub name_ref_type: HLSLNameRefType,
    pub swizzle: MaskedSwizzle,
    pub kind: HLSLType,
}
impl<TName: VMVectorNameRef> UnconcreteOpTarget for HLSLDataRefSpec<TName> {}

/// The type of vector name an HLSL-compatible value will have
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HLSLNameRefType {
    GenericRegister,
    Texture(u64),
    ShaderInput(String),
    ShaderOutput(String),
    Literal([u64; 4]),
    ArrayElement { of: Box<Self>, idx: u64 },
}

/// An HLSL-compatible specification for a declaration of a new vector variable
#[derive(Debug, Clone)]
pub struct HLSLDeclarationSpec<TName: VMVectorNameRef> {
    pub vm_name_ref: TName,
    pub decl_type: HLSLDeclarationSpecType,
    pub n_components: u8,
    pub kind: HLSLType,
    pub name: String,
}
/// The type of vector name an HLSL-compatible declaration will have
#[derive(Debug, Clone)]
pub enum HLSLDeclarationSpecType {
    GenericRegister,
    Texture(u64),
    ShaderInput(String),
    ShaderOutput(String),
    // TODO re-enable array declarations once we re-work the HLSLCompatibleOutcome::Declaration enum, which is currently very single-variable-specific.
    // Specifically, the declaration spec only has one base-name-ref, not one for each variable.
    // Array { of: Box<Self>, len: u64 },
}

/// Helper trait for HLSL-compatible specifications of references to vectors
///
/// TODO make private
pub trait ExpandsIntoHLSLComponents {
    type TName: VMVectorNameRef;

    /// Expand the vector reference into constituent scalar references.
    ///
    /// TODO refactor this into something that just returns the constituent components?
    fn expand(&self) -> Vec<VMScalarDataRef<Self::TName>>;
}
impl<TName: VMVectorNameRef> ExpandsIntoHLSLComponents for HLSLDataRefSpec<TName> {
    type TName = TName;
    fn expand(&self) -> Vec<VMScalarDataRef<TName>> {
        self.swizzle
            .0
            .iter()
            .filter_map(|comp| comp.map(|comp| (self.vm_name_ref.clone(), comp)))
            .collect()
    }
}
impl<TName: VMVectorNameRef> ExpandsIntoHLSLComponents for HLSLDeclarationSpec<TName> {
    type TName = TName;
    fn expand(&self) -> Vec<VMScalarDataRef<TName>> {
        (0..self.n_components)
            .into_iter()
            .map(|i| (self.vm_name_ref.clone(), VECTOR_COMPONENTS[i as usize]))
            .collect()
    }
}

/// Trait for abstract VMs that are capable of translation to HLSL.
///
/// Allows VMs to define actions in terms of "elements" i.e. the basic unit that it specifically operates on.
pub trait HLSLCompatibleAbstractVM: AbstractVM {}

/// An action that can be represented as a set of HLSL-compatible outcomes.
///
/// See [HLSLCompatibleOutcome]
pub trait HLSLCompatibleAction<TVM: HLSLCompatibleAbstractVM>: Action<TVM> {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<TVM>>;
}

/// An outcome of an action that can be translated to an HLSL outcome by a [crate::abstract_machine::analysis::variable::VariableAbstractMachine].
#[derive(Debug, Clone)]
pub enum HLSLCompatibleOutcome<TVM: HLSLCompatibleAbstractVM> {
    /// Declare that some named element exists, and optionally has a known literal value.
    Declaration {
        declspec: HLSLDeclarationSpec<TVM::TVectorNameRef>,
        literal_value: Option<TypedVMRef<[u64; 4]>>,
    },

    /// Declare that an output element has a new value, based on many input scalars.
    Operation {
        // The actual operation, including the input scalars
        op: UnconcreteOpResult<HLSLDataRefSpec<TVM::TVectorNameRef>>,
        // How each of the output scalars are related to the input scalars
        component_deps: Vec<(
            TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>,
            Vec<TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>>,
        )>,
    },

    /// Declare that program flow may end early due to a set of input scalars.
    EarlyOut {
        inputs: Vec<TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>>,
    },
}
