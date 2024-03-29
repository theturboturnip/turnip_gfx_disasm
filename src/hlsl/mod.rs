use crate::{abstract_machine::{VMName, VMVector, expr::Reg}, Action};

use self::kinds::{HLSLKind, HLSLKindBitmask};

pub mod compat;
pub mod display;
pub mod syntax;
pub mod kinds;
pub mod vm;


/// The name of an unswizzled vector in the HLSL virtual machine
/// 
/// TODO include kind
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HLSLRegister {
    Texture2D(u64),
    Texture3D(u64),
    TextureCube(u64),
    GenericRegister(String, u8),
    ShaderInput(String, u8),
    ShaderOutput(String, u8),
    // TODO make this a more general Array of blah
    ConstBuffer { id: String, n_comps: u8, dims: Vec<u64> },
}
impl VMName for HLSLRegister {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::ShaderInput(..) | Self::Texture2D(_) | Self::Texture3D(_) | Self::TextureCube(_) => true, // assuming textures are read-only
            Self::ConstBuffer { .. } => true,
            _ => false,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::ShaderOutput(..) => true,
            Self::ConstBuffer { .. } => false,
            _ => false, // assuming textures are read-only
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        match self {
            HLSLRegister::Texture2D(_) => HLSLKind::TEXTURE2D,
            HLSLRegister::Texture3D(_) => HLSLKind::TEXTURE3D,
            HLSLRegister::TextureCube(_) => HLSLKind::TEXTURECUBE,
            HLSLRegister::GenericRegister(..) => HLSLKind::NUMERIC,
            HLSLRegister::ShaderInput(..) => HLSLKind::NUMERIC,
            HLSLRegister::ShaderOutput(..) => HLSLKind::NUMERIC,
            HLSLRegister::ConstBuffer { .. } => HLSLKind::NUMERIC,
        }
    }
}
impl VMVector for HLSLRegister {}
impl Reg for HLSLRegister {
    fn n_components(&self) -> usize {
        match self {
            HLSLRegister::Texture2D(_) | HLSLRegister::Texture3D(_) | HLSLRegister::TextureCube(_) => 1,
            HLSLRegister::GenericRegister(_, n) | HLSLRegister::ShaderInput(_, n) | HLSLRegister::ShaderOutput(_, n) => *n as usize,
            HLSLRegister::ConstBuffer { n_comps, .. } => *n_comps as usize,
        }
    }
    fn indexable_depth(&self) -> usize {
        match self {
            HLSLRegister::ConstBuffer { dims: lengths, .. } => lengths.len(),
            _ => 0
        }
    }
    fn output_kind(&self) -> HLSLKind {
        self.toplevel_kind() // TODO merge these functions somehow
    }
    fn refine_output_kind_if_possible(&mut self, constraint: HLSLKind) -> Option<kinds::KindRefinementResult> {
        // Can't refine HLSLRegister - it has a fixed kind
        None
    }
    
}

pub type HLSLAction = Action<HLSLRegister>;