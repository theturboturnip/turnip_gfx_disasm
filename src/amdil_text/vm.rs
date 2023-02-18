//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS};
use crate::abstract_machine::{
    AbstractVM, RefinableVMDataRef, VMDataRef, VMVectorDataRef, VMVectorNameRef,
};
use crate::hlsl::compat::HLSLCompatibleAbstractVM;
use crate::hlsl::types::{HLSLHoleTypeMask, HLSLType};
use crate::{Action, LegacyOutcome};

use crate::abstract_machine::VMRef;

pub mod hlsl;

/// The type of Action held by Programs for the [AMDILAbstractVM]
pub type AMDILAction = super::decode::Instruction;

/// Type for the AMDIL abstract VM. Implements [ScalarAbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum AMDILAbstractVM {}
impl AbstractVM for AMDILAbstractVM {
    type Action = AMDILAction;
    type TVectorNameRef = AMDILNameRef;
    type TVectorDataRef = AMDILDataRef;
    type TScalarDataRef = RefinableVMDataRef<(AMDILNameRef, VectorComponent)>;
}
impl HLSLCompatibleAbstractVM for AMDILAbstractVM {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILNameRef {
    NamedRegister(String),
    Literal([u64; 4]),
    NamedLiteral(String),
    NamedBuffer { name: String, idx: u64 },
    NamedInputRegister(String),
    NamedOutputRegister(String),
    Texture(u64),
}
impl VMRef for AMDILNameRef {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Texture(_) => true, // Assuming textures are read only
            Self::Literal(..) => true,
            Self::NamedLiteral(..) => false, // Named literals always map to real literals in AMDIL
            Self::NamedInputRegister(..) => true,
            // TODO consider concept of i/o buffers
            Self::NamedBuffer { .. } => true,
            _ => false,
        }
    }
}
impl VMVectorNameRef for AMDILNameRef {
    fn n_components(&self) -> u8 {
        match self {
            Self::Texture(_) => 1,
            _ => 4,
        }
    }

    fn base_type_mask(&self) -> HLSLType {
        match self {
            Self::Texture(_) => HLSLHoleTypeMask::TEXTURE2D.into(),
            _ => HLSLHoleTypeMask::NUMERIC.into(),
        }
    }
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
impl VMRef for AMDILDataRef {
    fn is_pure_input(&self) -> bool {
        self.name.is_pure_input()
    }
}
impl VMDataRef<AMDILNameRef> for AMDILDataRef {
    fn name(&self) -> &AMDILNameRef {
        &self.name
    }

    fn type_mask(&self) -> HLSLType {
        self.name.base_type_mask()
    }
}
impl VMVectorDataRef<AMDILNameRef> for AMDILDataRef {
    fn swizzle(&self) -> MaskedSwizzle {
        self.swizzle
    }
}
impl From<AMDILDataRef> for RefinableVMDataRef<AMDILDataRef> {
    fn from(data: AMDILDataRef) -> Self {
        Self {
            kind: data.type_mask(),
            data,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILDeclaration {
    TextureResource(u64),
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
    fn outcomes(&self) -> Vec<crate::LegacyOutcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::TextureResource(id) => vec![LegacyOutcome::Declaration {
                name: (AMDILNameRef::Texture(*id), VectorComponent::X).into(),
                value: None,
            }],
            AMDILDeclaration::NamedLiteral(name, value) => VECTOR_COMPONENTS
                .iter()
                .map(|comp| LegacyOutcome::Declaration {
                    name: (AMDILNameRef::NamedLiteral(name.clone()), *comp).into(),
                    value: Some((AMDILNameRef::Literal(*value), *comp).into()),
                })
                .collect(),
            AMDILDeclaration::NamedBuffer { .. } => {
                // TODO Declare all of the values in the array?
                vec![]
            }
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => VECTOR_COMPONENTS
                .iter()
                .take(*len as usize)
                .map(|comp| LegacyOutcome::Declaration {
                    name: (AMDILNameRef::NamedInputRegister(name.clone()), *comp).into(),
                    value: None,
                })
                .collect(),
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => VECTOR_COMPONENTS
                .iter()
                .take(*len as usize)
                .map(|comp| LegacyOutcome::Declaration {
                    name: (AMDILNameRef::NamedOutputRegister(name.clone()), *comp).into(),
                    value: None,
                })
                .collect(),
        }
    }
}
