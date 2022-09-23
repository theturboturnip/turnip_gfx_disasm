use std::{cell::RefCell, rc::Rc};

use super::{
    vector::{MaskedSwizzle, VectorComponent},
    DataKind,
};

pub mod compat {
    use crate::{
        abstract_machine::{
            vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
            DataKind, ScalarAbstractVM, TypedVMRef, VMDataRef, VMNameRef, VMRef,
        },
        ScalarAction,
    };

    /// An HLSL-compatible specification for a reference to a swizzled vector
    #[derive(Debug, Clone)]
    pub struct HLSLDataRefSpec<TName: VMNameRef> {
        pub vm_name_ref: TName,
        pub name_ref_type: HLSLNameRefType,
        pub swizzle: MaskedSwizzle,
        pub kind: DataKind,
    }
    /// The type of vector name an HLSL-compatible value will have
    #[derive(Debug, Clone)]
    pub enum HLSLNameRefType {
        GenericRegister,
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
        pub kind: DataKind,
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
        // Declare that some named element exists, and optionally has a known literal value.
        Declaration {
            declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
            literal_value: Option<TypedVMRef<[u64; 4]>>,
        },

        // Declare that an output element has a new value, based on many input scalars.
        Operation {
            opname: String,
            output_dataspec: HLSLDataRefSpec<TVM::TElementNameRef>,
            input_dataspecs: Vec<HLSLDataRefSpec<TVM::TElementNameRef>>,
            component_deps: Vec<(
                TypedVMRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>,
                Vec<TypedVMRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>>,
            )>,
        },
    }
}

/// An unswizzled vector available to operations in the HLSL virtual machine.
/// See [HLSLVariableInfo].
pub type HLSLVariable = Rc<RefCell<HLSLVariableInfo>>;

/// Metadata of [HLSLVariable] i.e. an unswizzled vector available to operations in the HLSL virtual machine
#[derive(Debug)]
pub struct HLSLVariableInfo {
    pub vector_name: HLSLVectorName,
    pub kind: DataKind,
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
        output_dataref: HLSLVectorDataRef,
        opname: String,
        input_datarefs: Vec<HLSLVectorDataRef>,
        // Mapping of each individual scalar output to each individual scalar input
        scalar_deps: Vec<(HLSLScalarDataRef, Vec<HLSLScalarDataRef>)>,
    },
}

impl std::fmt::Display for HLSLVectorName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO have a formatter that takes type into account
        match self {
            Self::GenericRegister(id) => write!(f, "var{:0>3}", id),
            Self::Literal(data) => write!(
                f,
                "(0x{:x}, 0x{:x}, 0x{:x}, 0x{:x})",
                data[0], data[1], data[2], data[3]
            ),
            Self::ShaderInput(name) | Self::ShaderOutput(name) => write!(f, "{}", name),
            Self::ArrayElement { of: elem, idx } => write!(f, "{}[{}]", elem, idx),
        }
    }
}

impl std::fmt::Display for HLSLOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Declaration { new_var } => {
                let var = new_var.borrow();
                write!(f, "{:?}{} {};", var.kind, var.n_components, var.vector_name)
            }
            Self::Definition {
                new_var,
                components,
            } => {
                {
                    let var = new_var.borrow();
                    write!(
                        f,
                        "{:?}{} {} = {:?}{}(",
                        var.kind, var.n_components, var.vector_name, var.kind, var.n_components
                    )?;
                }
                for (refed_var, comp) in components {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}.{:?}, ", referenced_var.vector_name, *comp)?;
                }
                write!(f, ");")
            }
            Self::Operation {
                output_dataref,
                opname,
                input_datarefs,
                ..
            } => {
                {
                    let output_var = output_dataref.0.borrow();
                    write!(
                        f,
                        "{}{} = {}(",
                        output_var.vector_name, output_dataref.1, opname
                    )?;
                }
                for (refed_var, swizz) in input_datarefs {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}{}, ", referenced_var.vector_name, swizz)?;
                }
                write!(f, ");")
            }
        }
    }
}
