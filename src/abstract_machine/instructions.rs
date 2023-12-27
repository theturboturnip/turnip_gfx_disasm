//! This module contains generic structures useful for defining and displaying large instruction sets.
//!
//!  TODO Merge hole handling with hlsl::syntax setup

use crate::hlsl::kinds::HLSLKind;

use super::expr::{ContigSwizzle, UntypedVector};

/// Struct holding the arguments to an instruction for a given VM
#[derive(Debug, Clone)]
pub struct InstrArgs<TReg: Clone + PartialEq> {
    pub dst: (TReg, ContigSwizzle, HLSLKind),
    pub srcs: Vec<(UntypedVector<TReg>, HLSLKind)>,
}

// /// Trait for types which can map the indivdual output scalars of an instruction to the input scalars that affect them.
// pub trait DependencyRelation<TVM: AbstractVM> {
//     /// Given a set of input and output elements, return a vector of mappings:
//     ///     (output scalar, inputs affecting that output)
//     fn determine_dependencies(
//         &self,
//         args: &InstrArgs<TVM::Register>,
//     ) -> Vec<(
//         UntypedScalar<TVM::Register>,
//         Vec<(UntypedScalar<TVM::Register>, HLSLKind)>,
//     )>;
// }

/// Impl for [DependencyRelation] that defines simple relations
#[derive(Debug, Clone, Copy)]
pub enum SimpleDependencyRelation {
    /// Each output's component is only affected by the corresponding input components
    ///
    /// e.g. `add r0.xyz r1.xyz r2.xyz` has dependencies `[r1.x, r2.x] -> r0.x`, `[r1.y, r2.y] -> r0.y`, `[r1.z, r2.z] -> r0.z`
    PerComponent,
    /// Each output's component is affected by all input components
    ///
    /// e.g. `dp3_ieee r0.x, r1.xyz, r2.xyz` has dependency `[r1.xyz, r2.xyz] -> r0.x`
    AllToAll,
}
// impl<TVM: AbstractVM> DependencyRelation<TVM> for SimpleDependencyRelation {
//     fn determine_dependencies(
//         &self,
//         args: &InstrArgs<TVM::Register>,
//     ) -> Vec<(
//         UntypedScalar<TVM::Register>,
//         Vec<(UntypedScalar<TVM::Register>, HLSLKind)>,
//     )> {
//         // for output in outputs
//         //     for component in output
//         let expanded_dsts = args
//             .dst
//             .1
//             .iter()
//             .map(|comp| UntypedScalar::Component(args.dst.0, *comp));
//         let expanded_inputs = args
//             .srcs
//             .iter()
//             .map(|(vec, kind)| {
//                 (vec.decompose(), kind)
//             });
//         match self {
//             Self::AllToAll => {
//                 // Put all inputs in a vector
//                 let in_scalars: Vec<_> = expanded_inputs.flatten().collect();
//                 // Return all of them for each output element
//                 expanded_dsts
//                     .map(|out_scalar| (out_scalar, in_scalars.clone()))
//                     .collect()
//             }

//             Self::PerComponent => {
//                 assert_eq!(find_common(args.srcs.iter(), |v| v.0.n_components()), Some(expanded_dsts.len()));
//                 // Vector of inputs => Vector of (Vector of input scalar)
//                 let in_vecs: Vec<Vec<_>> = expanded_inputs.collect();
//                 let mut deps = vec![];
//                 // For each output
//                 for output in expanded_dsts {
//                     // For each scalar #i in output
//                     let required_len = output.len();
//                     for (i, comp) in output.into_iter().enumerate() {
//                         // Add a dependency on (input[0][i], input[1][i]...)
//                         deps.push((
//                             comp,
//                             in_vecs
//                                 .iter()
//                                 .map(|in_vec| {
//                                     // Safety - will produce wrong results otherwise
//                                     assert_eq!(in_vec.len(), required_len);
//                                     in_vec[i].clone()
//                                 })
//                                 .collect(),
//                         ))
//                     }
//                 }
//                 deps
//             }
//         }
//     }
// }
