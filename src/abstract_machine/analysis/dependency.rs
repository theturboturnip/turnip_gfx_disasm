use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{
        instructions::{DependencyRelation, InstrArgs},
        AbstractVM, VMRef, VMScalarDataRef, VMScalarNameRef,
    },
    Action, Outcome, VMVectorDataRef,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<VMScalarNameRef<TVM::TVectorNameRef>>,
    dependents: HashMap<
        VMScalarNameRef<TVM::TVectorNameRef>,
        HashSet<VMScalarNameRef<TVM::TVectorNameRef>>,
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
        resolved_inputs: &mut HashSet<VMScalarNameRef<TVM::TVectorNameRef>>,
        input: &VMScalarNameRef<TVM::TVectorNameRef>,
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
                Outcome::Assign {
                    output,
                    inputs,
                    dep_rel,
                    ..
                } => {
                    if output.is_pure_input() {
                        println!(
                                "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                                inputs, output
                            );
                        continue;
                    }
                    let args = InstrArgs::<TVM> {
                        outputs: vec![output],
                        inputs: inputs,
                    };
                    for (output_scl, input_scls) in dep_rel.determine_dependencies(&args) {
                        // TODO Resolve NamedLiteral to Literal for vectors?

                        let mut resolved_inputs = HashSet::new();
                        for input_scl in input_scls.iter() {
                            self.resolve_input_on(&mut resolved_inputs, &input_scl);
                        }
                        for input_scl in &self.discard_dependencies {
                            self.resolve_input_on(&mut resolved_inputs, input_scl);
                        }
                        self.dependents.insert(output_scl, resolved_inputs);
                    }
                }
                Outcome::EarlyOut { inputs } => {
                    let mut resolved_inputs = HashSet::new();
                    for input in inputs {
                        self.resolve_input_on(&mut resolved_inputs, &input.scalar_name());
                    }
                    self.discard_dependencies.extend(resolved_inputs)
                }
            }
        }
    }

    pub fn dependents(
        &self,
    ) -> &HashMap<VMScalarNameRef<TVM::TVectorNameRef>, HashSet<VMScalarNameRef<TVM::TVectorNameRef>>>
    {
        &self.dependents
    }
}
