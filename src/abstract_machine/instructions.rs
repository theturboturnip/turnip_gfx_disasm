//! This module contains generic structures useful for defining and displaying large instruction sets.
//!
//! Each instruction has X inputs and Y outputs.
//! Each input/output should have a kind, which may be a [HLSLType::Hole],
//! in which case the kind may be dynamically decided based on context
//! (TODO: Allow Holes to be numbered, i.e. Hole(1) and Hole(2) are not necessarily the same type, for more complicated expressions?)
//!
//! Each instruction, at least for vector machines, may modify the arguments e.g. mask off elements? for correctness.
//! This masking may be dependent on other arguments e.g. if the output has certain elements masked out, those elements could be masked out in the inputs.
//!
//! TODO should this be in crate::abstract_machine, or at the top level?
//!

use crate::hlsl::types::HLSLType;

use super::{
    vector::VectorComponent, AbstractVM, DataWidth, Refinable, TypedVMRef, VMVectorDataRef,
};

/// Trait for a type that manages a set of possible instructions
pub trait InstructionSet<TVM: AbstractVM>: Sized {
    /// How instructions are identified
    type InstructionID;

    /// The concrete type used to represent the specification for all instructions
    type InstructionSpec: InstructionSpec<TVM>;

    /// Given an instruction ID, retreive the relevant specification if one exists
    fn get_spec(&self, id: &Self::InstructionID) -> Option<Self::InstructionSpec>;
}

/// The specification for an instruction - how arguments should be used and how inputs/outputs relate to each other.
///
/// For convenience, exposes the functions contained in [ArgsSpec] and [DependencyRelation]
pub trait InstructionSpec<TVM: AbstractVM> {
    /// [DependencyRelation::determine_dependencies]
    fn determine_dependencies(
        &self,
        args: &InstrArgs<TVM>,
    ) -> Vec<((usize, VectorComponent), Vec<(usize, VectorComponent)>)>;
    /// [ArgsSpec::sanitize_arguments]
    fn sanitize_arguments(&self, args: Vec<TVM::TVectorDataRef>) -> InstrArgs<TVM>;
}
impl<TVM: AbstractVM, TArgsSpec: ArgsSpec<TVM>, TDepRelation: DependencyRelation<TVM>>
    InstructionSpec<TVM> for (TArgsSpec, TDepRelation)
{
    fn sanitize_arguments(&self, args: Vec<TVM::TVectorDataRef>) -> InstrArgs<TVM> {
        self.0.sanitize_arguments(args)
    }
    fn determine_dependencies(
        &self,
        args: &InstrArgs<TVM>,
    ) -> Vec<((usize, VectorComponent), Vec<(usize, VectorComponent)>)> {
        self.1.determine_dependencies(args)
    }
}

/// Struct holding the arguments to an instruction for a given VM
#[derive(Debug, Clone)]
pub struct InstrArgs<TVM: AbstractVM> {
    pub outputs: Vec<TypedVMRef<TVM::TVectorDataRef>>,
    pub inputs: Vec<TypedVMRef<TVM::TVectorDataRef>>,
}

/// Trait for types which can map the indivdual output scalars of an instruction to the input scalars that affect them.
pub trait DependencyRelation<TVM: AbstractVM> {
    /// Given a set of input and output elements, return a vector of mappings:
    ///     (output scalar, inputs affecting that output)
    fn determine_dependencies(
        &self,
        args: &InstrArgs<TVM>,
    ) -> Vec<((usize, VectorComponent), Vec<(usize, VectorComponent)>)>;
}

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
impl<TVM: AbstractVM> DependencyRelation<TVM> for SimpleDependencyRelation {
    fn determine_dependencies(
        &self,
        args: &InstrArgs<TVM>,
    ) -> Vec<((usize, VectorComponent), Vec<(usize, VectorComponent)>)> {
        // for output in outputs
        //     for component in output
        let expanded_outs = args.outputs.iter().enumerate().map(|(i, elem)| {
            elem.data
                .decompose()
                .into_iter()
                .map(move |(_, comp)| (i, comp))
        });
        let expanded_inputs = args.inputs.iter().enumerate().map(|(i, elem)| {
            elem.data
                .decompose()
                .into_iter()
                .map(move |(_, comp)| (i, comp))
        });
        match self {
            Self::AllToAll => {
                // Put all inputs in a vector
                let in_scalars: Vec<_> = expanded_inputs.flatten().collect();
                // Return all of them for each output element
                expanded_outs
                    .flatten()
                    .map(|out_scalar| (out_scalar, in_scalars.clone()))
                    .collect()
            }

            Self::PerComponent => {
                // Vector of inputs => Vector of (Vector of input scalar)
                let in_vecs: Vec<Vec<_>> = expanded_inputs
                    .map(|in_vec| in_vec.collect::<Vec<_>>())
                    .collect();
                let mut deps = vec![];
                // For each output
                for output in expanded_outs {
                    // For each scalar #i in output
                    let required_len = output.len();
                    for (i, comp) in output.into_iter().enumerate() {
                        // Add a dependency on (input[0][i], input[1][i]...)
                        deps.push((
                            comp,
                            in_vecs
                                .iter()
                                .map(|in_vec| {
                                    // Safety - will produce wrong results otherwise
                                    assert_eq!(in_vec.len(), required_len);
                                    in_vec[i].clone()
                                })
                                .collect(),
                        ))
                    }
                }
                deps
            }
        }
    }
}

pub trait ArgsSpec<TVM: AbstractVM> {
    /// Given a set of arguments to an instruction, split them into (outputs, inputs) and clean them up
    ///
    /// TODO Result<> type, probably just use anyhow
    fn sanitize_arguments(&self, args: Vec<TVM::TVectorDataRef>) -> InstrArgs<TVM>;
}
/// Implementation of [ArgsSpec] that takes a vector of args as [outputs... inputs...]
/// and applies [HLSLType]s and [DataWidth]s to create [TypedVMRef]s for each arg
#[derive(Debug, Clone)]
pub struct SimpleArgsSpec {
    output_kinds: Vec<HLSLType>,
    input_kinds: Vec<HLSLType>,
    width: DataWidth,
}
impl<TVM: AbstractVM> ArgsSpec<TVM> for SimpleArgsSpec
where
    TVM::TVectorDataRef: Refinable,
{
    fn sanitize_arguments(&self, args: Vec<TVM::TVectorDataRef>) -> InstrArgs<TVM> {
        let (output_elems, input_elems) = args.split_at(self.output_kinds.len());
        assert_eq!(output_elems.len(), self.output_kinds.len());
        assert_eq!(input_elems.len(), self.input_kinds.len());

        let outputs: Vec<_> = output_elems
            .into_iter()
            .zip(self.output_kinds.iter())
            .map(|(data, kind)| TypedVMRef {
                // TODO wish we didn't need to clone here :(
                data: data.clone(),
                kind: *kind,
                width: self.width,
            })
            .collect();

        let inputs: Vec<_> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, kind)| TypedVMRef {
                // TODO wish we didn't need to clone here :(
                data: data.clone(),
                kind: *kind,
                width: self.width,
            })
            .collect();

        assert_eq!(outputs.len(), self.output_kinds.len());
        assert_eq!(inputs.len(), self.input_kinds.len());

        InstrArgs { outputs, inputs }
    }
}
