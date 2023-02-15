use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{vector::VectorComponent, AbstractVM, TypedVMRef, VMRef, VMScalarDataRef},
    Action, Outcome,
};

pub type ScalarDataRef<TVM: AbstractVM> = VMScalarDataRef<TVM::TVectorNameRef>;

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<TypedVMRef<ScalarDataRef<TVM>>>,
    dependents: HashMap<ScalarDataRef<TVM>, HashSet<TypedVMRef<ScalarDataRef<TVM>>>>,
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
        resolved_inputs: &mut HashSet<TypedVMRef<ScalarDataRef<TVM>>>,
        input: &TypedVMRef<ScalarDataRef<TVM>>,
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
    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for dep in action.outcomes() {
            match dep {
                Outcome::Declaration { value: None, .. } => {
                    // Irrelevant because no values are being assigned
                }
                Outcome::Declaration {
                    name,
                    value: Some(value),
                } => {
                    let mut resolved_inputs = HashSet::new();
                    self.resolve_input_on(&mut resolved_inputs, &value);
                    self.dependents.insert(name, resolved_inputs);
                }
                Outcome::Dependency { output, inputs } => {
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
    ) -> &HashMap<ScalarDataRef<TVM>, HashSet<TypedVMRef<ScalarDataRef<TVM>>>> {
        &self.dependents
    }
}
