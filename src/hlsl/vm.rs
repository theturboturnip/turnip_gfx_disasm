use crate::AbstractVM;

use super::HLSLRegister;

/// Type for the HLSL abstract VM. Implements [ScalarAbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum HLSLAbstractVM {}
impl AbstractVM for HLSLAbstractVM {
    type Register = HLSLRegister;
    // type Scalar = UntypedHLSLScalar;
    // type Vector = UntypedHLSLVector;

    // fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
    //     v.ts.clone()
    // }
}
