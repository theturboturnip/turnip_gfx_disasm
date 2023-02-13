use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{ScalarAbstractVM, TypedVMRef, VMRef},
    ScalarAction, ScalarOutcome,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: ScalarAbstractVM> {
    discard_dependencies: HashSet<TypedVMRef<TVM::TScalarDataRef>>,
    dependents: HashMap<TVM::TScalarDataRef, HashSet<TypedVMRef<TVM::TScalarDataRef>>>,
}
impl<TVM: ScalarAbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
            discard_dependencies: HashSet::new(),
        }
    }

    fn resolve_input_on(
        &self,
        resolved_inputs: &mut HashSet<TypedVMRef<TVM::TScalarDataRef>>,
        input: &TypedVMRef<TVM::TScalarDataRef>,
    ) {
        if input.data.is_pure_input() {
            // Pure inputs are not resolved into their dependencies
            resolved_inputs.insert(input.clone());
        } else {
            if let Some(input_dependents) = self.dependents.get(&input.data) {
                resolved_inputs.extend(input_dependents.iter().cloned());
            } else {
                println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input.data);
                resolved_inputs.insert(input.clone());
            }
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn ScalarAction<TVM>) {
        for dep in action.outcomes() {
            match dep {
                ScalarOutcome::Declaration { value: None, .. } => {
                    // Irrelevant because no values are being assigned
                }
                ScalarOutcome::Declaration {
                    name,
                    value: Some(value),
                } => {
                    let mut resolved_inputs = HashSet::new();
                    self.resolve_input_on(&mut resolved_inputs, &value);
                    self.dependents.insert(name, resolved_inputs);
                }
                ScalarOutcome::Dependency { output, inputs } => {
                    if output.data.is_pure_input() {
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
                    self.dependents.insert(output.data, resolved_inputs);
                }
                ScalarOutcome::EarlyOut { inputs } => {
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
    ) -> &HashMap<TVM::TScalarDataRef, HashSet<TypedVMRef<TVM::TScalarDataRef>>> {
        &self.dependents
    }
}
