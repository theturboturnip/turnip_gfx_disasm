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
        pub base_name_ref: (TName, MaskedSwizzle),
        pub ref_type: HLSLDataRefType,
        pub kind: DataKind,
    }
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
            self.base_name_ref
                .1
                 .0
                .iter()
                .filter_map(|comp| comp.map(|comp| (self.base_name_ref.0.clone(), comp)))
                .collect()
        }
    }

    #[derive(Debug, Clone)]
    pub struct HLSLDeclarationSpec<TName: HLSLCompatibleNameRef> {
        pub base_name_ref: TName,
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
                .map(|i| (self.base_name_ref.clone(), VECTOR_COMPONENTS[i as usize]))
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
            name: HLSLDeclarationSpec<TVM::TElementNameRef>,
            literal_value: Option<TypedRef<[u64; 4]>>,
        },

        // Declare that an output element has a new value, based on many input scalars.
        Operation {
            opname: String,
            output_elem: HLSLDataRefSpec<TVM::TElementNameRef>,
            input_elems: Vec<HLSLDataRefSpec<TVM::TElementNameRef>>,
            component_deps: Vec<(
                TypedRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>,
                Vec<TypedRef<HLSLCompatibleScalarRef<TVM::TElementNameRef>>>,
            )>,
        },
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HLSLVectorRef {
    GenericRegister(u64),
    ShaderInput(String),
    ShaderOutput(String),
    Literal([u64; 4]),
    // TODO read/write permissions for ArrayElement?
    ArrayElement { of: Box<Self>, idx: u64 },
}

pub type HLSLScalarElementRef = (HLSLVariable, VectorComponent);
pub type HLSLElementRef = (HLSLVariable, MaskedSwizzle);

pub type HLSLVariable = Rc<RefCell<HLSLVariableInfo>>;

/// Single, unnamed unit of value with a specified kind
///
/// TODO store information on whether this is "important" i.e. a shader input or output
#[derive(Debug)]
pub struct HLSLVariableInfo {
    pub vector_ref: HLSLVectorRef,
    pub kind: DataKind,
    pub n_components: u8,
}

#[derive(Debug, Clone)]
pub enum HLSLOutcome {
    // State that a new variable exists without setting its value
    Declaration {
        new_var: HLSLVariable,
    },
    // State that a new variable exists and has a given value taken directly from other variables
    Definition {
        new_var: HLSLVariable,
        components: Vec<HLSLScalarElementRef>,
    },
    // State that the output of an operation has been assigned to some components of a variable
    Operation {
        output: HLSLElementRef,
        op: String,
        inputs: Vec<HLSLElementRef>,
        // Mapping of each individual scalar output to each individual scalar input
        scalar_deps: Vec<(HLSLScalarElementRef, Vec<HLSLScalarElementRef>)>,
    },
}

impl std::fmt::Display for HLSLVectorRef {
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
                write!(f, "{:?}{} {};", var.kind, var.n_components, var.vector_ref)
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
                        var.kind, var.n_components, var.vector_ref, var.kind, var.n_components
                    )?;
                }
                for (refed_var, comp) in components {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}.{:?}, ", referenced_var.vector_ref, *comp)?;
                }
                write!(f, ");")
            }
            Self::Operation {
                output, op, inputs, ..
            } => {
                {
                    let output_var = output.0.borrow();
                    write!(f, "{}{} = {}(", output_var.vector_ref, output.1, op)?;
                }
                for (refed_var, swizz) in inputs {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}{}, ", referenced_var.vector_ref, swizz)?;
                }
                write!(f, ");")
            }
        }
    }
}
