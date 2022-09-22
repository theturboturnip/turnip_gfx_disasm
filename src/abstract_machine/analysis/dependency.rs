use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{AbstractVM, DataRef, TypedRef},
    Action, Outcome,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    dependents: HashMap<TVM::TScalarDataRef, HashSet<TypedRef<TVM::TScalarDataRef>>>,
}
impl<TVM: AbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for dep in action.outcomes() {
            match dep {
                Outcome::Declaration {
                    name,
                    value: Some(value),
                } => {
                    self.dependents.insert(name, [value].into());
                }
                Outcome::Dependency { output, inputs } => {
                    if output.data.is_pure_input() {
                        println!(
                            "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                            inputs, output
                        );
                        continue;
                    }

                    // TODO introduce logic for kind/width coercion, store keys as ValueRef

                    // TODO Resolve NamedLiteral to Literal for vectors?

                    let mut resolved_inputs = HashSet::new();
                    for input in inputs {
                        if input.data.is_pure_input() {
                            // Pure inputs are not resolved into their dependencies
                            resolved_inputs.insert(input.clone());
                        } else {
                            if let Some(input_dependents) = self.dependents.get(&input.data) {
                                resolved_inputs.extend(input_dependents.iter().cloned());
                            } else {
                                println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                                resolved_inputs.insert(input.clone());
                            }
                        }
                    }
                    self.dependents.insert(output.data, resolved_inputs);
                }
                _ => {}
            }
        }
    }

    pub fn dependents(
        &self,
    ) -> &HashMap<TVM::TScalarDataRef, HashSet<TypedRef<TVM::TScalarDataRef>>> {
        &self.dependents
    }
}
