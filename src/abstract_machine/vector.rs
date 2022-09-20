//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use std::ops::Index;

use crate::{Action, Outcome};

use super::{AbstractVM, DataRef, ScalarBasedAbstractVM, TypedRef};

#[derive(Debug)]
pub enum Vector2ScalarAbstractVM {}
impl AbstractVM for Vector2ScalarAbstractVM {
    type TDataRef = (VectorNameRef, VectorComponent);
}
impl ScalarBasedAbstractVM for Vector2ScalarAbstractVM {}

#[derive(Debug)]
pub enum VectorAbstractVM {}
impl AbstractVM for VectorAbstractVM {
    type TDataRef = VectorDataRef;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VectorComponent {
    X,
    Y,
    Z,
    W,
}
const VECTOR_COMPONENTS: [VectorComponent; 4] = [
    VectorComponent::X,
    VectorComponent::Y,
    VectorComponent::Z,
    VectorComponent::W,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MaskedSwizzle(pub [Option<VectorComponent>; 4]);
impl MaskedSwizzle {
    pub fn new(x: [Option<VectorComponent>; 4]) -> Self {
        MaskedSwizzle(x)
    }
    pub fn identity(num: usize) -> Self {
        use VectorComponent::*;
        match num {
            0 => Self([None; 4]),
            1 => Self([Some(X), None, None, None]),
            2 => Self([Some(X), Some(Y), None, None]),
            3 => Self([Some(X), Some(Y), Some(Z), None]),
            4 => Self([Some(X), Some(Y), Some(Z), Some(W)]),
            _ => panic!("MaskedSwizzle with {} (> 4) components requested", num),
        }
    }
    pub fn masked_identity(x: bool, y: bool, z: bool, w: bool) -> Self {
        use VectorComponent::*;
        Self([
            if x { Some(X) } else { None },
            if y { Some(Y) } else { None },
            if z { Some(Z) } else { None },
            if w { Some(W) } else { None },
        ])
    }
    /// If the MaskedSwizzle is a simple mask of contiguous elements beginning in x
    ///
    /// i.e. x___, xy__, xyz_, or xyzw
    ///
    /// returns the length of that contiguous run. Else returns None
    pub fn as_nonzero_length(&self) -> Option<u8> {
        use VectorComponent::*;
        match self {
            Self([Some(X), None, None, None]) => Some(1),
            Self([Some(X), Some(Y), None, None]) => Some(2),
            Self([Some(X), Some(Y), Some(Z), None]) => Some(3),
            Self([Some(X), Some(Y), Some(Z), Some(W)]) => Some(4),
            _ => None,
        }
    }

    /// Return a copy of self where each element is set to `None` if the corresponding element of `mask` is `None`
    ///
    /// e.g. `xyww.masked_out(xxx_) = xyw_`
    pub fn masked_out(&self, mask: Self) -> Self {
        Self([
            mask.0[0].and(self.0[0]),
            mask.0[1].and(self.0[1]),
            mask.0[2].and(self.0[2]),
            mask.0[3].and(self.0[3]),
        ])
    }

    /// Returns a copy of self equivalent to `identity(4).masked_out(self)`
    ///
    /// e.g. transfers the mask to the copy while keeping correct component order
    pub fn copy_mask(&self) -> Self {
        Self::masked_identity(
            self.0[0].is_some(),
            self.0[1].is_some(),
            self.0[2].is_some(),
            self.0[3].is_some(),
        )
    }

    pub fn truncated(&self, len: u8) -> Self {
        Self([
            if len >= 1 { self.0[0] } else { None },
            if len >= 2 { self.0[1] } else { None },
            if len >= 3 { self.0[2] } else { None },
            if len >= 4 { self.0[3] } else { None },
        ])
    }
}
impl Index<usize> for MaskedSwizzle {
    type Output = Option<VectorComponent>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VectorNameRef {
    NamedRegister(String),
    Literal([u64; 4]),
    NamedLiteral(String),
    NamedBuffer { name: String, idx: u64 },
    NamedInputRegister(String),
    NamedOutputRegister(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VectorDataRef {
    pub name: VectorNameRef,
    pub swizzle: MaskedSwizzle,
}
impl VectorDataRef {
    pub fn named_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::NamedRegister(name),
            swizzle,
        }
    }
    pub fn literal(data: [u64; 4], swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::Literal(data),
            swizzle,
        }
    }
    pub fn named_literal(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::NamedLiteral(name),
            swizzle,
        }
    }
    pub fn named_buffer(name: String, idx: u64, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::NamedBuffer { name, idx },
            swizzle,
        }
    }
    pub fn named_input_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::NamedInputRegister(name),
            swizzle,
        }
    }
    pub fn named_output_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self {
            name: VectorNameRef::NamedOutputRegister(name),
            swizzle,
        }
    }
}

impl DataRef for VectorNameRef {
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
impl DataRef for VectorDataRef {
    fn is_pure_input(&self) -> bool {
        self.name.is_pure_input()
    }
}
impl DataRef for (VectorNameRef, VectorComponent) {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VectorDeclaration {
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
impl Action<Vector2ScalarAbstractVM> for VectorDeclaration {
    fn outcomes(&self) -> Vec<crate::Outcome<Vector2ScalarAbstractVM>> {
        match self {
            VectorDeclaration::NamedLiteral(name, value) => VECTOR_COMPONENTS
                .iter()
                .map(|comp| Outcome::Declaration {
                    name: (VectorNameRef::NamedLiteral(name.clone()), *comp),
                    value: Some(TypedRef {
                        data: (VectorNameRef::Literal(*value), *comp),
                        kind: super::DataKind::Untyped,
                        width: super::DataWidth::E32,
                    }),
                })
                .collect(),
            VectorDeclaration::NamedBuffer { .. } => {
                // TODO Declare all of the values in the array?
                vec![]
            }
            // VECTOR_COMPONENTS.iter().map(|comp| Outcome::Declaration { name: VectorNameRef::NamedBuffer { name: (), idx: () }, value: () })
            VectorDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            }
            | VectorDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => VECTOR_COMPONENTS
                .iter()
                .take(*len as usize)
                .map(|comp| Outcome::Declaration {
                    name: (VectorNameRef::NamedInputRegister(name.clone()), *comp),
                    value: None,
                })
                .collect(),
        }
    }
}
