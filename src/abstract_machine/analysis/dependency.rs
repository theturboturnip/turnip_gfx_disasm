use std::collections::{HashMap, HashSet};

use crate::{
    abstract_machine::{
        AbstractVM, VMName, expr::Scalar, vector::VectorComponent
    },
    hlsl::kinds::{HLSLKind, HLSLKindBitmask},
    Action,
};

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: AbstractVM> {
    discard_dependencies: HashSet<Scalar<TVM::Register>>,
    /// Mapping of <non-pure-input> to <pure inputs it depends on>
    dependents: HashMap<
        (TVM::Register, VectorComponent),
        HashSet<Scalar<TVM::Register>>,
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
        resolved_inputs: &mut HashSet<Scalar<TVM::Register>>,
        input: &Scalar<TVM::Register>,
    ) {
        if input.is_pure_input() {
            // Pure inputs are not resolved into their dependencies
            resolved_inputs.insert(input.clone());
        } else {
            if let Some(input_dependents) = self.dependents.get(&input.0) {
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
    pub fn accum_action(&mut self, action: &Action<TVM::Register>, control_flow_inputs: &HashSet<Scalar<TVM::Register>>) {
        match action {
            Action::Assign { output, expr } => {
                if output.0.is_pure_input() {
                    println!(
                            "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                            expr, output
                        );
                }

                let output_scls = output.1.iter().map(|comp| (output.0.clone(), *comp)).collect();

                for (output_scl, input_scls) in expr.scalar_deps(output_scls) {
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
            Action::If { expr, if_true, if_fals, .. } => {
                // Make `inputs` a dependency on everything touched inside the if_true AND if_fals
                let mut next_control_flow_inputs = control_flow_inputs.clone();
                for input_scalar in expr.deps() {
                    self.resolve_input_on(&mut next_control_flow_inputs, &input_scalar);
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
    ) -> &HashMap<Scalar<TVM::Register>, HashSet<(Scalar<TVM::Register>, HLSLKind)>>
    {
        &self.dependents
    }

    pub fn discard_dependencies(&self) -> &HashSet<(Scalar<TVM::Register>, HLSLKind)> {
        &self.discard_dependencies
    }
}
