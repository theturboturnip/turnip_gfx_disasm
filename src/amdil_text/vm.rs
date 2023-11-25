//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use crate::abstract_machine::vector::{MaskedSwizzle, ComponentOf};
use crate::abstract_machine::{
    AbstractVM, VMName, VMVector, Refinable
};
use crate::hlsl::compat::HLSLCompatibleAbstractVM;
use crate::hlsl::syntax::HLSLOperator;
use crate::hlsl::kinds::{HLSLKind, HLSLKindBitmask};
use crate::{Action, Outcome};

/// The type of Action held by Programs for the [AMDILAbstractVM]
pub type AMDILAction = super::decode::Instruction;

/// Type for the AMDIL abstract VM. Implements [AbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum AMDILAbstractVM {}
impl AbstractVM for AMDILAbstractVM {
    type Action = AMDILAction;
    type Register = AMDILRegister;
    type Scalar = ComponentOf<AMDILRegister>;
    type Vector = AMDILMaskSwizVector;

    fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
        let reg = v.register();
        v.swizzle().0.iter().filter_map(|c| match c {
            None => None,
            Some(comp) => Some(ComponentOf::new(reg.clone(), *comp))
        }).collect()
    }
}
impl HLSLCompatibleAbstractVM for AMDILAbstractVM {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILRegister {
    NamedRegister(String),
    Literal([u64; 4]),
    NamedLiteral(String),
    NamedBuffer { name: String, idx: u64 },
    NamedInputRegister(String),
    NamedOutputRegister(String),
    Texture(u64),
}
impl VMName for AMDILRegister {
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

    fn hlsl_kind(&self) -> HLSLKind {
        match self {
            Self::Texture(_) => HLSLKindBitmask::TEXTURE2D.into(),
            _ => HLSLKindBitmask::NUMERIC.into(),
        }
    }
}
impl VMVector for AMDILRegister {
    fn n_components(&self) -> usize {
        match self {
            Self::Texture(_) => 1,
            _ => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AMDILMaskSwizVector(AMDILRegister, MaskedSwizzle, HLSLKind);
impl AMDILMaskSwizVector {
    pub fn new(reg: AMDILRegister, swizzle: MaskedSwizzle) -> Self {
        let kind = reg.hlsl_kind();
        Self(reg, swizzle, kind)
    }

    pub fn register(&self) -> &AMDILRegister {
        &self.0
    }
    pub fn swizzle(&self) -> MaskedSwizzle {
        self.1
    }

    pub fn named_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::NamedRegister(name), swizzle)
    }
    pub fn literal(data: [u64; 4], swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::Literal(data), swizzle)
    }
    pub fn named_literal(name: String, swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::NamedLiteral(name), swizzle)
    }
    pub fn named_buffer(name: String, idx: u64, swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::NamedBuffer { name, idx }, swizzle)
    }
    pub fn named_input_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::NamedInputRegister(name), swizzle)
    }
    pub fn named_output_register(name: String, swizzle: MaskedSwizzle) -> Self {
        Self::new(AMDILRegister::NamedOutputRegister(name), swizzle)
    }
}
impl VMName for AMDILMaskSwizVector {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }

    fn hlsl_kind(&self) -> HLSLKind {
        self.0.hlsl_kind()
    }
}
impl VMVector for AMDILMaskSwizVector {
    fn n_components(&self) -> usize {
        self.1.num_used_components() // TODO: ???
    }
}
impl Refinable for AMDILMaskSwizVector {
    fn refine_kind(&self, hlsl_kind: HLSLKind) -> Option<Self> {
        let new_kind = self.2.intersection(hlsl_kind)?;
        Some(Self(self.0.clone(), self.1, new_kind))
    }
}
// impl VMName for AMDILDataRef {
//     fn is_pure_input(&self) -> bool {
//         self.name.is_pure_input()
//     }
// }
// impl VMDataRef<AMDILNameRef> for AMDILDataRef {
//     fn name(&self) -> &AMDILNameRef {
//         &self.name
//     }

//     fn hlsl_kind(&self) -> HLSLKind {
//         self.name.base_hlsl_kind()
//     }
// }
// impl VMVectorDataRef<AMDILNameRef> for AMDILDataRef {
//     fn swizzle(&self) -> MaskedSwizzle {
//         self.swizzle
//     }
// }
// impl VMDataRef<AMDILNameRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn name(&self) -> &AMDILNameRef {
//         &self.name.name
//     }

//     fn hlsl_kind(&self) -> HLSLKind {
//         self.kind
//     }
// }
// impl VMVectorDataRef<AMDILNameRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn swizzle(&self) -> MaskedSwizzle {
//         self.name.swizzle
//     }
// }
// impl From<AMDILDataRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn from(data: AMDILDataRef) -> Self {
//         Self {
//             kind: data.hlsl_kind(),
//             name: data,
//         }
//     }
// }

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
    fn outcomes(&self) -> Vec<Outcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::TextureResource(id) => {
                vec![Outcome::Declare(AMDILRegister::Texture(*id))]
            }
            AMDILDeclaration::NamedLiteral(name, value) => {
                let name = AMDILRegister::NamedLiteral(name.clone());
                vec![
                    Outcome::Declare(name.clone()),
                    Outcome::Assign {
                        output: AMDILMaskSwizVector::new(name, MaskedSwizzle::identity(4)),
                        op: HLSLOperator::Assign,
                        inputs: vec![
                            AMDILMaskSwizVector::literal(*value, MaskedSwizzle::identity(4))
                        ],
                    },
                ]
            }
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => vec![Outcome::Declare(AMDILRegister::NamedInputRegister(
                name.clone(),
            ))],
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => vec![Outcome::Declare(AMDILRegister::NamedOutputRegister(
                name.clone(),
            ))],
            _ => vec![],
            // TODO re-enable this
            // AMDILDeclaration::NamedBuffer { name, len } => {
            //     vec![Outcome::Declaration {
            //         name: HLSLDeclarationSpec {
            //             base_name_ref: AMDILNameRef::NamedOutputRegister(name.clone()),
            //             kind: DataKind::Hole,
            //             n_components: 4,
            //             decl_type: HLSLDeclarationSpecType::Array {
            //                 of: Box::new(HLSLDeclarationSpecType::ShaderInput(name.clone())),
            //                 len: *len,
            //             },
            //             name: name.clone(),
            //         },
            //         literal_value: None,
            //     }]
            // }
        }
    }
}
