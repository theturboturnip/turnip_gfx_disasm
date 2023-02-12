use crate::{
    abstract_machine::{
        vector::{MaskedSwizzle, VectorComponent},
        VMElementRef,
    },
    VMDataRef, VMNameRef, VMRef,
};

use self::{syntax::UnconcreteOpTarget, types::HLSLType};

pub mod compat;
mod display;
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
impl VMRef for HLSLVectorName {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::ShaderInput(_) | Self::Literal(_) | Self::ArrayElement { .. } => true,
            _ => false,
        }
    }
}
impl VMNameRef for HLSLVectorName {}

/// A reference to a single scalar in the HLSL virtual machine
pub type HLSLScalarDataRef = (HLSLVector, VectorComponent);
impl VMRef for HLSLScalarDataRef {
    fn is_pure_input(&self) -> bool {
        self.0.vector_name.is_pure_input()
    }
}
impl VMDataRef for HLSLScalarDataRef {}

/// A reference to a swizzled vector in the HLSL virtual machine
pub type HLSLVectorDataRef = (HLSLVector, MaskedSwizzle);
impl VMRef for HLSLVectorDataRef {
    fn is_pure_input(&self) -> bool {
        self.0.vector_name.is_pure_input()
    }
}
impl VMDataRef for HLSLVectorDataRef {}
impl VMElementRef<HLSLScalarDataRef> for HLSLVectorDataRef {
    fn decompose(&self) -> Vec<HLSLScalarDataRef> {
        self.1
             .0
            .iter()
            .filter_map(|comp| match comp {
                Some(comp) => Some((self.0.clone(), *comp)),
                None => None,
            })
            .collect()
    }
}
impl UnconcreteOpTarget for HLSLVectorDataRef {}
