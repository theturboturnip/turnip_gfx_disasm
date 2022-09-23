use std::{cell::RefCell, rc::Rc};

use super::{
    vector::{MaskedSwizzle, VectorComponent},
    DataKind,
};

pub mod compat {
    use crate::abstract_machine::{
        vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
        DataKind, DataRef, ScalarAbstractVM, TypedRef,
    };

    pub trait ExpandsIntoHLSLComponents {
        type TName;
        fn expand(&self) -> Vec<HLSLCompatibleScalarRef<Self::TName>>;
    }

    #[derive(Debug, Clone)]
    pub struct HLSLDataRefSpec<TName: HLSLCompatibleNameRef> {
        pub vm_data_ref: (TName, MaskedSwizzle),
        pub ref_type: HLSLDataRefType,
        pub kind: DataKind,
    }
    // TODO move out of compat, rename to HLSLNameRef
    #[derive(Debug, Clone)]
    pub enum HLSLDataRefType {
        GenericRegister,
        ShaderInput(String),
        ShaderOutput(String),
        Literal([u64; 4]),
        ArrayElement { of: Box<Self>, idx: u64 },
    }
    impl<TName: HLSLCompatibleNameRef> ExpandsIntoHLSLComponents for HLSLDataRefSpec<TName> {
        type TName = TName;
        fn expand(&self) -> Vec<HLSLCompatibleScalarRef<TName>> {
            self.vm_data_ref
                .1
                 .0
                .iter()
                .filter_map(|comp| comp.map(|comp| (self.vm_data_ref.0.clone(), comp)))
                .collect()
        }
    }

    #[derive(Debug, Clone)]
    pub struct HLSLDeclarationSpec<TName: HLSLCompatibleNameRef> {
        pub vm_name_ref: TName,
        pub decl_type: HLSLDeclarationSpecType,
        pub n_components: u8,
        pub kind: DataKind,
        pub name: String,
    }
    #[derive(Debug, Clone)]
    pub enum HLSLDeclarationSpecType {
        GenericRegister,
        ShaderInput(String),
        ShaderOutput(String),
        // TODO re-enable array declarations once we re-work the HLSLCompatibleOutcome::Declaration enum, which is currently very single-variable-specific.
        // Specifically, the declaration spec only has one base-name-ref, not one for each variable.
        // Array { of: Box<Self>, len: u64 },
    }
    impl<TName: HLSLCompatibleNameRef> ExpandsIntoHLSLComponents for HLSLDeclarationSpec<TName> {
        type TName = TName;
        fn expand(&self) -> Vec<HLSLCompatibleScalarRef<TName>> {
            (0..self.n_components)
                .into_iter()
                .map(|i| (self.vm_name_ref.clone(), VECTOR_COMPONENTS[i as usize]))
                .collect()
        }
    }

    pub trait HLSLCompatibleNameRef: DataRef {}

    pub type HLSLCompatibleScalarRef<T> = (T, VectorComponent);
    impl<T: HLSLCompatibleNameRef> DataRef for HLSLCompatibleScalarRef<T> {
        fn is_pure_input(&self) -> bool {
            self.0.is_pure_input()
        }
    }

    /// Trait for abstract VMs that are capable of translation to HLSL.
    ///
    /// Allows VMs to define actions in terms of "elements" i.e. the basic unit that it specifically operates on.
    pub trait HLSLCompatibleAbstractVM: std::fmt::Debug {
        /// Element = the unit that abstract VM instructions operate on.
        ///
        /// e.g. for DXBC and AMDIL instructions operate on vectors => TElementDataRef = a VectorDataRef.
        /// Must be convertible to an HLSL-esque representation
        type TElementNameRef: HLSLCompatibleNameRef;
    }
    impl<T> ScalarAbstractVM for T
    where
        T: HLSLCompatibleAbstractVM,
    {
        type TScalarDataRef = HLSLCompatibleScalarRef<T::TElementNameRef>;
    }

    pub trait HLSLCompatibleAction<TVM: HLSLCompatibleAbstractVM> {
        fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<TVM>>;
    }

    #[derive(Debug, Clone)]
    pub enum HLSLCompatibleOutcome<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> {
        // Declare that some named element exists, and optionally has a known literal value.
        Declaration {
            declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
            literal_value: Option<TypedRef<[u64; 4]>>,
        },

        // Declare that an output element has a new value, based on many input scalars.
        Operation {
            opname: String,
            output_dataspec: HLSLDataRefSpec<TVM::TElementNameRef>,
            input_dataspecs: Vec<HLSLDataRefSpec<TVM::TElementNameRef>>,
            component_deps: Vec<(
                TypedRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>,
                Vec<TypedRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>>,
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
