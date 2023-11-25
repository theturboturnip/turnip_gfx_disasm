use crate::{
    abstract_machine::Outcome,
    AbstractVM, Action,
};

use super::{HLSLVector, HLSLScalarName, HLSLSingleVectorName};

pub type HLSLAction = Outcome<HLSLAbstractVM>;

/// Type for the HLSL abstract VM. Implements [ScalarAbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum HLSLAbstractVM {}
impl AbstractVM for HLSLAbstractVM {
    type Action = HLSLAction;
    type Register = HLSLSingleVectorName;
    type Scalar = HLSLScalarName;
    type Vector = HLSLVector;

    fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
        v.ts.clone()
    }
}

impl Action<HLSLAbstractVM> for HLSLAction {
    fn outcomes(&self) -> Vec<Outcome<HLSLAbstractVM>> {
        vec![self.clone()]
    }
}
