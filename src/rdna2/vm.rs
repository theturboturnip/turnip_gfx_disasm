use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent};
use crate::hlsl::types::HLSLHoleTypeMask;
use crate::{Action, Outcome, VMVectorNameRef};

use crate::abstract_machine::{
    AbstractVM, VMDataRef, VMRef, VMScalarDataRef, VMScalarNameRef, VMVectorDataRef,
};

pub type RDNA2Action = Box<dyn Action<RDNA2AbstractVM>>;

#[derive(Debug, Clone)]
pub enum RDNA2AbstractVM {}
impl AbstractVM for RDNA2AbstractVM {
    type Action = RDNA2Action;
    type TVectorNameRef = RDNA2DataRef;
    type TVectorDataRef = RDNA2DataRef;
    type TScalarDataRef = RDNA2DataRef;
}
pub type RDNA2Outcome = Outcome<RDNA2AbstractVM>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RDNA2Output {
    VertPosition {
        idx: u64,
        vector_comp: usize,
    },
    VertParameter {
        idx: u64,
        vector_comp: usize,
    },
    FragColor {
        idx: u64,
        vector_comp: usize,
    },
    Other {
        name: &'static str,
        idx: u64,
        vector_comp: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RDNA2DataRef {
    /// 64-bit scalar per-executor-group register
    GeneralPurposeGlobalRegister(u64),
    /// 64-bit scalar per-executor register
    GeneralPurposeRegister(u64),
    /// Value from memory (TODO: Add refs to actual included values and make copyable?)
    Memory(),
    /// 64-bit literal value
    Literal(u64),
    /// Register with special meaning
    SpecialReg { name: &'static str, idx: u64 },
    /// Refers to a specific component of a fragment shader's vector output.
    ///
    /// Fragment shaders are a special case because they usually write to vectors
    /// e.g. output color, so we have a component index here.
    Output(RDNA2Output),
}
impl VMRef for RDNA2DataRef {
    fn is_pure_input(&self) -> bool {
        match self {
            RDNA2DataRef::Literal(..) => true,
            _ => false,
        }
    }
}
impl VMVectorNameRef for RDNA2DataRef {
    fn n_components(&self) -> u8 {
        1
    }

    fn base_type_mask(&self) -> crate::hlsl::types::HLSLType {
        // TODO check this is right
        HLSLHoleTypeMask::NUMERIC.into()
    }
}
impl VMDataRef<RDNA2DataRef> for RDNA2DataRef {
    fn name(&self) -> &RDNA2DataRef {
        self
    }

    fn type_mask(&self) -> crate::hlsl::types::HLSLType {
        self.base_type_mask()
    }
}
impl VMVectorDataRef<RDNA2DataRef> for RDNA2DataRef {
    fn decompose(&self) -> Vec<(RDNA2DataRef, VectorComponent)> {
        vec![(self.clone(), VectorComponent::X)]
    }

    fn swizzle(&self) -> MaskedSwizzle {
        MaskedSwizzle::identity(1)
    }
}
impl VMScalarDataRef<RDNA2DataRef> for RDNA2DataRef {
    fn comp(&self) -> VectorComponent {
        VectorComponent::X
    }

    fn scalar_name(&self) -> VMScalarNameRef<RDNA2DataRef> {
        (*self, self.comp())
    }
}
impl Into<(RDNA2DataRef, VectorComponent)> for RDNA2DataRef {
    fn into(self) -> (RDNA2DataRef, VectorComponent) {
        (self, VectorComponent::X)
    }
}
