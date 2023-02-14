use crate::{
    abstract_machine::{
        vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
        ScalarAbstractVM, TypedVMRef, VMDataRef, VMNameRef, VMRef,
    },
    ScalarAction,
};

use super::{
    syntax::{UnconcreteOpResult, UnconcreteOpTarget},
    types::HLSLType,
};

/// An HLSL-compatible specification for a reference to a swizzled vector
#[derive(Debug, Clone)]
pub struct HLSLDataRefSpec<TName: VMNameRef> {
    pub vm_name_ref: TName,
    pub name_ref_type: HLSLNameRefType,
    pub swizzle: MaskedSwizzle,
    pub kind: HLSLType,
}
impl<TName: VMNameRef> UnconcreteOpTarget for HLSLDataRefSpec<TName> {}

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
pub struct HLSLDeclarationSpec<TName: VMNameRef> {
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
    type TName: VMNameRef;

    /// Expand the vector reference into constituent scalar references.
    ///
    /// TODO refactor this into something that just returns the constituent components?
    fn expand(&self) -> Vec<HLSLCompatibleScalarRef<Self::TName>>;
}
impl<TName: VMNameRef> ExpandsIntoHLSLComponents for HLSLDataRefSpec<TName> {
    type TName = TName;
    fn expand(&self) -> Vec<HLSLCompatibleScalarRef<TName>> {
        self.swizzle
            .0
            .iter()
            .filter_map(|comp| {
                comp.map(|comp| HLSLCompatibleScalarRef::new(self.vm_name_ref.clone(), comp))
            })
            .collect()
    }
}
impl<TName: VMNameRef> ExpandsIntoHLSLComponents for HLSLDeclarationSpec<TName> {
    type TName = TName;
    fn expand(&self) -> Vec<HLSLCompatibleScalarRef<TName>> {
        (0..self.n_components)
            .into_iter()
            .map(|i| {
                HLSLCompatibleScalarRef::new(
                    self.vm_name_ref.clone(),
                    VECTOR_COMPONENTS[i as usize],
                )
            })
            .collect()
    }
}

/// An HLSL-compatible reference to a scalar (a single component of a VM's vector)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HLSLCompatibleScalarRef<T: VMNameRef> {
    pub vm_name_ref: T,
    pub comp: VectorComponent,
}
impl<T: VMNameRef> HLSLCompatibleScalarRef<T> {
    /// Make a new [HLSLCompatibleScalarRef]
    pub fn new(vm_name_ref: T, comp: VectorComponent) -> Self {
        Self { vm_name_ref, comp }
    }
    /// Make a new [HLSLCompatibleScalarRef], taking X as the component.
    /// Useful for scalar-based VMs.
    pub fn new_scalar(vm_name_ref: T) -> Self {
        Self {
            vm_name_ref,
            comp: VectorComponent::X,
        }
    }
}
impl<T: VMNameRef> VMRef for HLSLCompatibleScalarRef<T> {
    fn is_pure_input(&self) -> bool {
        self.vm_name_ref.is_pure_input()
    }
}
impl<T: VMNameRef> VMDataRef for HLSLCompatibleScalarRef<T> {}
impl<T: VMNameRef> Into<HLSLCompatibleScalarRef<T>> for (T, VectorComponent) {
    fn into(self) -> HLSLCompatibleScalarRef<T> {
        HLSLCompatibleScalarRef {
            vm_name_ref: self.0,
            comp: self.1,
        }
    }
}

/// Trait for abstract VMs that are capable of translation to HLSL.
///
/// Allows VMs to define actions in terms of "elements" i.e. the basic unit that it specifically operates on.
pub trait HLSLCompatibleAbstractVM: ScalarAbstractVM {
    /// Element = the unit that abstract VM instructions operate on.
    ///
    /// e.g. for DXBC and AMDIL instructions operate on vectors => TElementDataRef = a VectorDataRef.
    /// Must be convertible to an HLSL-esque representation
    type TElementNameRef: VMNameRef;
}

/// An action that can be represented as a set of HLSL-compatible outcomes.
///
/// See [HLSLCompatibleOutcome]
pub trait HLSLCompatibleAction<TVM: HLSLCompatibleAbstractVM>: ScalarAction<TVM> {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<TVM>>;
}

/// An outcome of an action that can be translated to an HLSL outcome by a [crate::abstract_machine::analysis::variable::VariableAbstractMachine].
#[derive(Debug, Clone)]
pub enum HLSLCompatibleOutcome<TVM: HLSLCompatibleAbstractVM> {
    /// Declare that some named element exists, and optionally has a known literal value.
    Declaration {
        declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
        literal_value: Option<TypedVMRef<[u64; 4]>>,
    },

    /// Declare that an output element has a new value, based on many input scalars.
    Operation {
        // The actual operation, including the input scalars
        op: UnconcreteOpResult<HLSLDataRefSpec<TVM::TElementNameRef>>,
        // How each of the output scalars are related to the input scalars
        component_deps: Vec<(
            TypedVMRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>,
            Vec<TypedVMRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>>,
        )>,
    },

    /// Declare that program flow may end early due to a set of input scalars.
    EarlyOut {
        inputs: Vec<TypedVMRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>>,
    },
}

mod display {
    use std::fmt::{Display, Formatter, Result};

    use crate::VMNameRef;

    use super::HLSLCompatibleScalarRef;

    impl<T> Display for HLSLCompatibleScalarRef<T>
    where
        T: VMNameRef,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result {
            write!(f, "ref {:?}.{:?}", self.vm_name_ref, self.comp)
        }
    }
}
