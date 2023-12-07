use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{
        instructions::{DependencyRelation, InstrArgs, SimpleDependencyRelation},
        AbstractVM, VMName
    },
    hlsl::syntax::Operator,
    Action,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<TVM::Scalar>,
    /// Mapping of <non-pure-input> to <pure inputs it depends on>
    dependents: HashMap<
        TVM::Scalar,
        HashSet<TVM::Scalar>,
    >,
}
impl<TVM: AbstractVM> Clone for ScalarDependencies<TVM> {
    fn clone(&self) -> Self {
        Self { discard_dependencies: self.discard_dependencies.clone(), dependents: self.dependents.clone() }
    }
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
    pub fn accum_action(&mut self, action: &Action<TVM>, control_flow_inputs: &HashSet<TVM::Scalar>) {
        match action {
            Action::Assign { output, inputs, op } => {
                if output.0.is_pure_input() {
                    println!(
                            "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                            inputs, output
                        );
                }
                let args = InstrArgs::<TVM::Vector> {
                    outputs: vec![output.clone()],
                    inputs: inputs.clone(),
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
            Action::EarlyOut => {
                let mut resolved_inputs = HashSet::new();
                for input in control_flow_inputs {
                    self.resolve_input_on(&mut resolved_inputs, &input);
                }
                self.discard_dependencies.extend(resolved_inputs)
            }
            Action::If { inputs, if_true, if_fals, .. } => {
                // Make `inputs` a dependency on everything touched inside the if_true AND if_fals
                let mut next_control_flow_inputs = control_flow_inputs.clone();
                for (vec, _) in inputs {
                    for input_scalar in TVM::decompose(vec) {
                        self.resolve_input_on(&mut next_control_flow_inputs, &input_scalar);
                    }
                }

                let mut else_branch_deps = self.clone(); // This needs to be a clone because it needs to understand how to expand previously-defined non-pure-inputs into pure inputs
                for true_action in if_true {
                    self.accum_action(true_action, &next_control_flow_inputs)
                }
                for false_action in if_fals {
                    else_branch_deps.accum_action(false_action, &next_control_flow_inputs)
                }
                // Merge the else branch into the current branch.
                // Any discard_dependencies added in the else-branch we pull in outright
                self.discard_dependencies.extend(else_branch_deps.discard_dependencies.into_iter());
                // If any new variables were created in the else branch, we don't care - we won't be able to use them in the following scope.
                // However, if any variable was modified in the else branch that was eventually seen in or before the if branch (i.e. exists in self.dependents), add those new else-only dependencies.
                for (k, v) in else_branch_deps.dependents {
                    match self.dependents.get_mut(&k) {
                        Some(v_2) => {
                            v_2.extend(v);
                        }
                        None => {}
                    }
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

    pub fn discard_dependencies(&self) -> &HashSet<TVM::Scalar> {
        &self.discard_dependencies
    }
}
