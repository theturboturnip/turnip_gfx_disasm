//! This module contains generic structures useful for defining and displaying large instruction sets.
//!
//! Each instruction has X inputs and Y outputs.
//! Each input/output should have a kind, which may be a [DataKind::Hole],
//! in which case the kind may be dynamically decided based on context
//! (TODO: Allow Holes to be numbered, i.e. Hole(1) and Hole(2) are not necessarily the same type, for more complicated expressions?)
//!
//! Each instruction, at least for vector machines, may modify the arguments e.g. mask off elements? for correctness.
//! This masking may be dependent on other arguments e.g. if the output has certain elements masked out, those elements could be masked out in the inputs.
//!
//! TODO should this be in crate::abstract_machine, or at the top level?
//!

use std::marker::PhantomData;

use super::{DataKind, DataWidth, ScalarAbstractVM, TypedVMRef, VMElementRef};

/// Trait for a type that manages a set of possible instructions
pub trait InstructionSet<TVM: ScalarAbstractVM>: Sized {
    /// How instructions are identified
    type InstructionID;

    /// The type used to represent how instruction inputs/outputs relate to each other
    type DepRelation: DependencyRelation<TVM>;

    /// The type used to represent how instruction arguments are used
    type ArgsSpec: ArgsSpec<TVM>;

    /// Given an instruction ID, retreive the relevant specification if one exists
    fn get_spec(&self, id: &Self::InstructionID) -> Option<InstructionSpec<TVM, Self>>;
}

/// The specification for an instruction - how arguments should be used and how inputs/outputs relate to each other.
///
/// For convenience, exposes the functions contained in [ArgsSpec] and [DependencyRelation]
pub struct InstructionSpec<TVM: ScalarAbstractVM, InstrSet: InstructionSet<TVM>> {
    pub args_spec: InstrSet::ArgsSpec,
    pub dep_relation: InstrSet::DepRelation,
}
impl<TVM: ScalarAbstractVM, InstrSet: InstructionSet<TVM>> InstructionSpec<TVM, InstrSet> {
    pub fn sanitize_arguments(&self, args: Vec<TVM::TElementDataRef>) -> InstrArgs<TVM> {
        self.args_spec.sanitize_arguments(args)
    }
    pub fn determine_dependencies(
        &self,
        args: InstrArgs<TVM>,
    ) -> Vec<(
        TypedVMRef<TVM::TScalarDataRef>,
        Vec<TypedVMRef<TVM::TScalarDataRef>>,
    )> {
        self.dep_relation.determine_dependencies(args)
    }
}

/// Struct holding the arguments to an instruction for a given VM
pub struct InstrArgs<TVM: ScalarAbstractVM> {
    outputs: Vec<TypedVMRef<TVM::TElementDataRef>>,
    inputs: Vec<TypedVMRef<TVM::TElementDataRef>>,
}

/// Trait for types which can map the indivdual output scalars of an instruction to the input scalars that affect them.
pub trait DependencyRelation<TVM: ScalarAbstractVM> {
    /// Given a set of input and output elements, return a vector of mappings:
    ///     (output scalar, inputs affecting that output)
    fn determine_dependencies(
        &self,
        args: InstrArgs<TVM>,
    ) -> Vec<(
        TypedVMRef<TVM::TScalarDataRef>,
        Vec<TypedVMRef<TVM::TScalarDataRef>>,
    )>;
}

/// Impl for [DependencyRelation] that defines simple relations
pub enum SimpleDependencyRelation {
    PerComponent,
    AllToAll,
}
impl<TVM: ScalarAbstractVM> DependencyRelation<TVM> for SimpleDependencyRelation {
    fn determine_dependencies(
        &self,
        args: InstrArgs<TVM>,
    ) -> Vec<(
        TypedVMRef<TVM::TScalarDataRef>,
        Vec<TypedVMRef<TVM::TScalarDataRef>>,
    )> {
        // for output in outputs
        //     for component in output
        let expanded_outs = args.outputs.into_iter().map(|elem| {
            let kind = elem.kind;
            let width = elem.width;
            elem.data
                .decompose()
                .into_iter()
                .map(move |scalar| TypedVMRef {
                    data: scalar,
                    kind: kind,
                    width: width,
                })
        });
        let expanded_inputs = args.inputs.into_iter().map(|elem| {
            let kind = elem.kind;
            let width = elem.width;
            elem.data
                .decompose()
                .into_iter()
                .map(move |scalar| TypedVMRef {
                    data: scalar,
                    kind: kind,
                    width: width,
                })
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

pub trait ArgsSpec<TVM: ScalarAbstractVM> {
    /// Given a set of arguments to an instruction, split them into (outputs, inputs) and clean them up
    ///
    /// TODO Result<> type, probably just use anyhow
    fn sanitize_arguments(&self, args: Vec<TVM::TElementDataRef>) -> InstrArgs<TVM>;
}
/// Implementation of [ArgsSpec] that takes a vector of args as [outputs... inputs...]
/// and applies [DataKind]s and [DataWidth]s to create [TypedVMRef]s for each arg
pub struct SimpleArgsSpec<TVM: ScalarAbstractVM> {
    output_kinds: Vec<DataKind>,
    input_kinds: Vec<DataKind>,
    width: DataWidth,
    _phantom: PhantomData<TVM>,
}
impl<TVM: ScalarAbstractVM> ArgsSpec<TVM> for SimpleArgsSpec<TVM> {
    fn sanitize_arguments(&self, args: Vec<TVM::TElementDataRef>) -> InstrArgs<TVM> {
        let (output_elems, input_elems) = args.split_at(self.output_kinds.len());
        assert_eq!(output_elems.len(), self.output_kinds.len());
        assert_eq!(input_elems.len(), self.input_kinds.len());

        let outputs = output_elems
            .into_iter()
            .zip(self.output_kinds.iter())
            .map(|(data, kind)| TypedVMRef {
                // TODO wish we didn't need to clone here :(
                data: data.clone(),
                kind: *kind,
                width: self.width,
            })
            .collect();

        let inputs = input_elems
            .into_iter()
            .zip(self.output_kinds.iter())
            .map(|(data, kind)| TypedVMRef {
                // TODO wish we didn't need to clone here :(
                data: data.clone(),
                kind: *kind,
                width: self.width,
            })
            .collect();

        InstrArgs { outputs, inputs }
    }
}
