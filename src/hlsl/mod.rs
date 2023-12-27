use crate::{abstract_machine::{VMName, VMVector}, Action};

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

pub type HLSLAction = Action<HLSLRegister>;