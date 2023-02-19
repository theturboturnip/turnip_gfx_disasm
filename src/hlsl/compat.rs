use crate::abstract_machine::AbstractVM;

use super::{types::HLSLType, HLSLVectorName};

/// Trait for abstract VMs that are capable of translation to HLSL.
///
/// Allows VMs to define actions in terms of "elements" i.e. the basic unit that it specifically operates on.
pub trait HLSLCompatibleAbstractVM: AbstractVM
// where
//     <Self as AbstractVM>::TVectorNameRef: HLSLCompatibleVectorName,
{
    /// Convert the [TVectorNameRef] to a (name, kind, n_components) tuple
    fn vector_name_to_hlsl(name: &Self::TVectorNameRef) -> (HLSLVectorName, HLSLType, u8);
    /// Convert the [TVectorDataRef] to a (name, kind, n_components) tuple
    fn vector_data_to_hlsl(data: &Self::TVectorDataRef) -> (HLSLVectorName, HLSLType, u8);
}
