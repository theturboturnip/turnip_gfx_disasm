use crate::abstract_machine::{vector::{VectorOf, VectorComponent}, VMName, VMVector, VMScalar};

use self::kinds::{HLSLKind, HLSLKindBitmask};

pub mod compat;
pub mod display;
pub mod syntax;
pub mod kinds;
pub mod vm;


/// The name of an unswizzled vector in the HLSL virtual machine
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HLSLRegister {
    Texture(u64),
    GenericRegister(String, u8),
    ShaderInput(String, u8),
    ShaderOutput(String, u8),
    // TODO read/write permissions for ArrayElement?
    // TODO this doesn't support dynamic indexing
    ArrayElement { of: Box<Self>, idx: u64 },
}
impl VMName for HLSLRegister {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::ShaderInput(..) | Self::Texture(_) => true, // assuming textures are read-only
            Self::ArrayElement { of, .. } => of.is_pure_input(),
            _ => false,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::ShaderOutput(..) => true, // assuming textures are read-only
            Self::ArrayElement { of, .. } => of.is_output(),
            _ => false,
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        match self {
            HLSLRegister::Texture(_) => HLSLKindBitmask::TEXTURE2D.into(),
            HLSLRegister::GenericRegister(..) => HLSLKindBitmask::NUMERIC.into(),
            HLSLRegister::ShaderInput(..) => HLSLKindBitmask::NUMERIC.into(),
            HLSLRegister::ShaderOutput(..) => HLSLKindBitmask::NUMERIC.into(),
            HLSLRegister::ArrayElement { of, idx: _ } => of.toplevel_kind(),
        }
    }
}
impl VMVector for HLSLRegister {
    fn n_components(&self) -> usize {
        match self {
            HLSLRegister::Texture(_) => 1,
            HLSLRegister::GenericRegister(_, n) | HLSLRegister::ShaderInput(_, n) | HLSLRegister::ShaderOutput(_, n) => *n as usize,
            HLSLRegister::ArrayElement { of, idx: _ } => of.n_components(),
        }
    }
}

/// A reference to a single scalar in the HLSL virtual machine
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HLSLScalar {
    Component(HLSLRegister, VectorComponent),
    Literal(u32),
}
impl HLSLScalar {
    pub fn new(reg: HLSLRegister, comp: VectorComponent) -> Self {
        assert!(comp.into_index() < reg.n_components());
        Self::Component(reg, comp)
    }
    pub fn lit(bits: u32) -> Self {
        Self::Literal(bits)
    }
}
impl VMName for HLSLScalar {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Component(reg, _) => reg.is_pure_input(),
            Self::Literal(_) => true,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::Component(reg, _) => reg.is_output(),
            Self::Literal(_) => false,
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        match self {
            Self::Component(reg, _) => reg.toplevel_kind(),
            Self::Literal(_) => HLSLKindBitmask::NUMERIC.into(),
        }
    }
}
impl VMScalar for HLSLScalar {}


pub type HLSLVector = VectorOf<HLSLScalar>;

// /// A reference to a swizzled vector in the HLSL virtual machine
// pub type HLSLVectorDataRef = (HLSLVector, MaskedSwizzle);
// impl UnconcreteOpTarget for HLSLVectorDataRef {}
