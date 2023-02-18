use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{AbstractVM, VMRef, VMScalarDataRef, VMScalarNameRef},
    Action, LegacyOutcome,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<TVM::TScalarDataRef>,
    dependents: HashMap<VMScalarNameRef<TVM::TVectorNameRef>, HashSet<TVM::TScalarDataRef>>,
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
        resolved_inputs: &mut HashSet<TVM::TScalarDataRef>,
        input: &TVM::TScalarDataRef,
    ) {
        if input.is_pure_input() {
            // Pure inputs are not resolved into their dependencies
            resolved_inputs.insert(input.clone());
        } else {
            if let Some(input_dependents) = self.dependents.get(&input.scalar_name()) {
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
                LegacyOutcome::Declaration { value: None, .. } => {
                    // Irrelevant because no values are being assigned
                }
                LegacyOutcome::Declaration {
                    name,
                    value: Some(value),
                } => {
                    let mut resolved_inputs = HashSet::new();
                    self.resolve_input_on(&mut resolved_inputs, &value);
                    self.dependents.insert(name.scalar_name(), resolved_inputs);
                }
                LegacyOutcome::Dependency { output, inputs } => {
                    if output.is_pure_input() {
                        println!(
                            "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                            inputs, output
                        );
                        continue;
                    }

                    // TODO Resolve NamedLiteral to Literal for vectors?

                    let mut resolved_inputs = HashSet::new();
                    for input in inputs {
                        self.resolve_input_on(&mut resolved_inputs, &input);
                    }
                    for input in &self.discard_dependencies {
                        self.resolve_input_on(&mut resolved_inputs, input);
                    }
                    self.dependents
                        .insert(output.scalar_name(), resolved_inputs);
                }
                LegacyOutcome::EarlyOut { inputs } => {
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
    ) -> &HashMap<VMScalarNameRef<TVM::TVectorNameRef>, HashSet<TVM::TScalarDataRef>> {
        &self.dependents
    }
}
