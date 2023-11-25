use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{
        instructions::{DependencyRelation, InstrArgs, SimpleDependencyRelation},
        AbstractVM, VMName
    },
    hlsl::syntax::Operator,
    Action, Outcome,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<TVM::Scalar>,
    dependents: HashMap<
        TVM::Scalar,
        HashSet<TVM::Scalar>,
    >,
}
impl<TVM: AbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
            discard_dependencies: HashSet::new(),
        }
    }

    fn resolve_input_on(
        &self,
        resolved_inputs: &mut HashSet<TVM::Scalar>,
        input: &TVM::Scalar,
    ) {
        if input.is_pure_input() {
            // Pure inputs are not resolved into their dependencies
            resolved_inputs.insert(input.clone());
        } else {
            if let Some(input_dependents) = self.dependents.get(&input) {
                resolved_inputs.extend(input_dependents.iter().cloned());
            } else {
                println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                resolved_inputs.insert(input.clone());
            }
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for dep in action.outcomes() {
            match dep {
                Outcome::Declare { .. } => {
                    // Irrelevant because no values are being assigned
                }
                Outcome::Assign { output, inputs, op } => {
                    if output.is_pure_input() {
                        println!(
                                "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                                inputs, output
                            );
                        continue;
                    }
                    let args = InstrArgs::<TVM::Vector> {
                        outputs: vec![output],
                        inputs: inputs,
                    };
                    for (output_scl, input_scls) in <SimpleDependencyRelation as DependencyRelation<TVM>>::determine_dependencies(&op.dep_rel(), &args){
                        // TODO Resolve NamedLiteral to Literal for vectors?

                        let mut resolved_inputs = HashSet::new();
                        for input_scl in input_scls.iter() {
                            self.resolve_input_on(&mut resolved_inputs, &input_scl);
                        }
                        self.dependents.insert(output_scl, resolved_inputs);
                    }
                }
                Outcome::EarlyOut { inputs } => {
                    let mut resolved_inputs = HashSet::new();
                    for input in inputs {
                        self.resolve_input_on(&mut resolved_inputs, &input);
                    }
                    self.discard_dependencies.extend(resolved_inputs)
                }
            }
        }
    }

    pub fn dependents(
        &self,
    ) -> &HashMap<TVM::Scalar, HashSet<TVM::Scalar>>
    {
        &self.dependents
    }
}
