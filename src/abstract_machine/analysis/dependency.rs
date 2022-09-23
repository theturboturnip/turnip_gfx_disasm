use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{ScalarAbstractVM, TypedVMRef, VMRef},
    ScalarAction, ScalarOutcome,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: ScalarAbstractVM> {
    dependents: HashMap<TVM::TScalarDataRef, HashSet<TypedVMRef<TVM::TScalarDataRef>>>,
}
impl<TVM: ScalarAbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn ScalarAction<TVM>) {
        for dep in action.outcomes() {
            match dep {
                ScalarOutcome::Declaration {
                    name,
                    value: Some(value),
                } => {
                    self.dependents.insert(name, [value].into());
                }
                ScalarOutcome::Dependency { output, inputs } => {
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
    ) -> &HashMap<TVM::TScalarDataRef, HashSet<TypedVMRef<TVM::TScalarDataRef>>> {
        &self.dependents
    }
}
