//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS};
use crate::abstract_machine::DataWidth;
use crate::{Action, Outcome};

use crate::abstract_machine::{
    analysis::variable::VariableCapableAbstractVM, AbstractVM, DataKind, DataRef,
    ElementAbstractVM, ElementAction, ElementDataRef, ElementOutcome, ScalarBasedAbstractVM,
    TypedRef,
};

// TODO rename to VectorAbstractVM
#[derive(Debug)]
pub enum AMDILAbstractVM {}
impl AbstractVM for AMDILAbstractVM {
    type TScalarDataRef = (AMDILNameRef, VectorComponent);
}
impl ScalarBasedAbstractVM for AMDILAbstractVM {}
impl ElementAbstractVM for AMDILAbstractVM {
    type TElementDataRef = AMDILDataRef;

    fn expand_element(elem: &Self::TElementDataRef) -> Vec<Self::TScalarDataRef> {
        elem.swizzle
            .0
            .iter()
            .filter_map(|comp| comp.map(|comp| (elem.name.clone(), comp)))
            .collect()
    }
}
impl VariableCapableAbstractVM for AMDILAbstractVM {
    fn variable_info(elem: &Self::TElementDataRef, unique_id: u64) -> (String, u8) {
        let name = match &elem.name {
            AMDILNameRef::NamedRegister(_) => format!("variable{unique_id:0>3}"),
            AMDILNameRef::Literal(data) => format!(
                "({:x}, {:x}, {:x}, {:x})",
                data[0], data[1], data[2], data[3]
            ),
            AMDILNameRef::NamedLiteral(name, ..) => name.clone(),
            AMDILNameRef::NamedBuffer { name, idx } => format!("{name}[{idx}]"),
            AMDILNameRef::NamedInputRegister(name, ..) => name.clone(),
            AMDILNameRef::NamedOutputRegister(name, ..) => name.clone(),
        };
        (
            name,
            elem.swizzle.0.iter().filter(|c| c.is_some()).count() as u8,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILNameRef {
    NamedRegister(String),
    Literal([u64; 4]),
    NamedLiteral(String),
    NamedBuffer { name: String, idx: u64 },
    NamedInputRegister(String),
    NamedOutputRegister(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AMDILDataRef {
    pub name: AMDILNameRef,
    pub swizzle: MaskedSwizzle,
}
impl AMDILDataRef {
    pub fn named_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::NamedRegister(name),
            swizzle,
        }
    }
    pub fn literal(data: [u64; 4], swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::Literal(data),
            swizzle,
        }
    }
    pub fn named_literal(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::NamedLiteral(name),
            swizzle,
        }
    }
    pub fn named_buffer(name: String, idx: u64, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::NamedBuffer { name, idx },
            swizzle,
        }
    }
    pub fn named_input_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::NamedInputRegister(name),
            swizzle,
        }
    }
    pub fn named_output_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: AMDILNameRef::NamedOutputRegister(name),
            swizzle,
        }
    }
}

impl DataRef for AMDILNameRef {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Literal(..) => true,
            Self::NamedLiteral(..) => true,
            Self::NamedInputRegister(..) => true,
            // TODO consider concept of i/o buffers
            Self::NamedBuffer { .. } => true,
            _ => false,
        }
    }
}
impl DataRef for AMDILDataRef {
    fn is_pure_input(&self) -> bool {
        self.name.is_pure_input()
    }
}
impl ElementDataRef for AMDILDataRef {}
impl DataRef for (AMDILNameRef, VectorComponent) {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILDeclaration {
    NamedLiteral(String, [u64; 4]),
    NamedBuffer {
        name: String,
        len: u64,
    },
    NamedInputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
    NamedOutputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
}
impl Action<AMDILAbstractVM> for AMDILDeclaration {
    fn outcomes(&self) -> Vec<crate::Outcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::NamedLiteral(name, value) => VECTOR_COMPONENTS
                .iter()
                .map(|comp| Outcome::Declaration {
                    name: (AMDILNameRef::NamedLiteral(name.clone()), *comp),
                    value: Some(TypedRef {
                        data: (AMDILNameRef::Literal(*value), *comp),
                        kind: DataKind::Untyped,
                        width: DataWidth::E32,
                    }),
                })
                .collect(),
            AMDILDeclaration::NamedBuffer { .. } => {
                // TODO Declare all of the values in the array?
                vec![]
            }
            // VECTOR_COMPONENTS.iter().map(|comp| Outcome::Declaration { name: VectorNameRef::NamedBuffer { name: (), idx: () }, value: () })
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            }
            | AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => VECTOR_COMPONENTS
                .iter()
                .take(*len as usize)
                .map(|comp| Outcome::Declaration {
                    // TODO THIS IS WRONG FOR OUTPUT REGISTERS
                    name: (AMDILNameRef::NamedInputRegister(name.clone()), *comp),
                    value: None,
                })
                .collect(),
        }
    }
}
impl ElementAction<AMDILAbstractVM> for AMDILDeclaration {
    fn per_element_outcomes(&self) -> Vec<ElementOutcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::NamedLiteral(name, value) => vec![ElementOutcome::Declaration {
                name: AMDILDataRef::named_literal(name.clone(), MaskedSwizzle::identity(4)),
                value: Some(TypedRef {
                    data: AMDILDataRef::literal(*value, MaskedSwizzle::identity(4)),
                    kind: DataKind::Hole,
                    width: DataWidth::E32,
                }),
            }],
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => vec![ElementOutcome::Declaration {
                name: AMDILDataRef::named_input_register(
                    name.clone(),
                    MaskedSwizzle::identity(*len as usize),
                ),
                value: None,
            }],
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => vec![ElementOutcome::Declaration {
                name: AMDILDataRef::named_output_register(
                    name.clone(),
                    MaskedSwizzle::identity(*len as usize),
                ),
                value: None,
            }],
            _ => {
                vec![]
            }
        }
    }
}
