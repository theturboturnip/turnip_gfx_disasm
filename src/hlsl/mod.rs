use crate::abstract_machine::{vector::{VectorOf, ComponentOf}, VMName, VMVector};

use self::types::{HLSLKind, HLSLKindBitmask};

pub mod compat;
pub mod display;
pub mod syntax;
pub mod types;
pub mod vm;


/// The name of an unswizzled vector in the HLSL virtual machine
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HLSLSingleVectorName {
    Texture(u64),
    GenericRegister(String),
    ShaderInput(String),
    ShaderOutput(String),
    Literal([u64; 4]),
    // TODO read/write permissions for ArrayElement?
    ArrayElement { of: Box<Self>, idx: u64 },
}
impl VMName for HLSLSingleVectorName {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::ShaderInput(_) | Self::Literal(_) | Self::Texture(_) => true, // assuming textures are read-only
            Self::ArrayElement { of, .. } => of.is_pure_input(),
            _ => false,
        }
    }

    fn type_mask(&self) -> HLSLKind {
        // TODO store this in a value for the variable machine to shrink it?
        match self {
            HLSLSingleVectorName::Texture(_) => HLSLKindBitmask::TEXTURE2D.into(),
            HLSLSingleVectorName::GenericRegister(_) => HLSLKindBitmask::NUMERIC.into(),
            HLSLSingleVectorName::ShaderInput(_) => HLSLKindBitmask::NUMERIC.into(),
            HLSLSingleVectorName::ShaderOutput(_) => HLSLKindBitmask::NUMERIC.into(),
            HLSLSingleVectorName::Literal(_) => HLSLKindBitmask::NUMERIC.into(),
            HLSLSingleVectorName::ArrayElement { of, idx } => of.type_mask(),
        }
    }
}
impl VMVector for HLSLSingleVectorName {
    fn n_components(&self) -> usize {
        match self {
            HLSLSingleVectorName::Texture(_) => 1,
            _ => 4 // TODO how to decide?
        }
    }
}

/// A reference to a single scalar in the HLSL virtual machine
pub type HLSLScalarName = ComponentOf<HLSLSingleVectorName>;


pub type HLSLVector = VectorOf<HLSLScalarName>;

// /// A reference to a swizzled vector in the HLSL virtual machine
// pub type HLSLVectorDataRef = (HLSLVector, MaskedSwizzle);
// impl UnconcreteOpTarget for HLSLVectorDataRef {}
