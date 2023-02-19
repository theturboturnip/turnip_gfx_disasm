use crate::{
    abstract_machine::{
        vector::VectorComponent, Outcome, VMDataRef, VMScalarDataRef, VMScalarNameRef,
    },
    AbstractVM, Action,
};

use super::{HLSLScalarDataRef, HLSLVector, HLSLVectorDataRef};

pub type HLSLAction = Outcome<HLSLAbstractVM>;

/// Type for the HLSL abstract VM. Implements [ScalarAbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum HLSLAbstractVM {}
impl AbstractVM for HLSLAbstractVM {
    type Action = HLSLAction;
    type TVectorNameRef = HLSLVector;
    type TVectorDataRef = HLSLVectorDataRef;
    type TScalarDataRef = HLSLScalarDataRef;
}

impl VMDataRef<HLSLVector> for HLSLScalarDataRef {
    fn name(&self) -> &HLSLVector {
        &self.0
    }

    fn type_mask(&self) -> super::types::HLSLKind {
        self.0.kind
    }
}
impl VMScalarDataRef<HLSLVector> for HLSLScalarDataRef {
    fn comp(&self) -> VectorComponent {
        self.1
    }

    fn scalar_name(&self) -> VMScalarNameRef<HLSLVector> {
        self.clone()
    }
}

impl Action<HLSLAbstractVM> for HLSLAction {
    fn outcomes(&self) -> Vec<Outcome<HLSLAbstractVM>> {
        vec![self.clone()]
    }
}
