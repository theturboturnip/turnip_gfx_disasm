use crate::{
    abstract_machine::vector::{MaskedSwizzle, VectorComponent},
    VMRef, VMVectorNameRef,
};

use self::{syntax::UnconcreteOpTarget, types::HLSLType};

pub mod compat;
pub mod display;
pub mod syntax;
pub mod types;
pub mod vm;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HLSLVector {
    pub vector_name: HLSLVectorName,
    pub kind: HLSLType,
    pub n_components: u8,
}
impl HLSLVector {
    pub fn identity_swizzle(&self) -> MaskedSwizzle {
        MaskedSwizzle::identity(self.n_components as usize)
    }
}
impl VMRef for HLSLVector {
    fn is_pure_input(&self) -> bool {
        self.vector_name.is_pure_input()
    }
}
impl VMVectorNameRef for HLSLVector {
    fn n_components(&self) -> u8 {
        self.n_components
    }

    fn base_type_mask(&self) -> HLSLType {
        self.kind
    }
}

/// The name of an unswizzled vector in the HLSL virtual machine
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HLSLVectorName {
    Texture(u64),
    GenericRegister(String),
    ShaderInput(String),
    ShaderOutput(String),
    Literal([u64; 4]),
    // TODO read/write permissions for ArrayElement?
    ArrayElement { of: Box<Self>, idx: u64 },
}
impl VMRef for HLSLVectorName {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::ShaderInput(_) | Self::Literal(_) | Self::Texture(_) => true, // assuming textures are read-only
            Self::ArrayElement { of, .. } => of.is_pure_input(),
            _ => false,
        }
    }
}

/// A reference to a single scalar in the HLSL virtual machine
pub type HLSLScalarDataRef = (HLSLVector, VectorComponent);

/// A reference to a swizzled vector in the HLSL virtual machine
pub type HLSLVectorDataRef = (HLSLVector, MaskedSwizzle);
impl UnconcreteOpTarget for HLSLVectorDataRef {}
