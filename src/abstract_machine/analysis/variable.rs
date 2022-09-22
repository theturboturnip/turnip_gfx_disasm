use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::abstract_machine::{
    hlsl::compat::{
        ExpandsIntoHLSLComponents, HLSLCompatibleAbstractVM, HLSLCompatibleAction,
        HLSLCompatibleOutcome, HLSLDataRefSpec,
    },
    vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
    DataKind, ScalarAbstractVM,
};

pub type ScalarVariableRef = (Variable, VectorComponent);
pub type VectorVariableRef = (Variable, MaskedSwizzle);

pub type Variable = Rc<RefCell<VariableInfo>>;

/// Single, unnamed unit of value with a specified kind
///
/// TODO store information on whether this is "important" i.e. a shader input or output
#[derive(Debug)]
pub struct VariableInfo {
    id: usize,
    name: String,
    kind: DataKind,
    components: u8,
    tick_created: u64,
}

#[derive(Debug, Clone)]
pub enum VariableAbstractVMOutcome {
    // State that a new variable exists without setting its value
    Declaration {
        new_var: Variable,
    },
    // State that a new variable exists and has a given value taken directly from other variables
    Definition {
        new_var: Variable,
        components: Vec<ScalarVariableRef>,
    },
    // State that the output of an operation has been assigned to some components of a variable
    Operation {
        output: VectorVariableRef,
        op: String,
        inputs: Vec<VectorVariableRef>,
        // Mapping of each individual scalar output to each individual scalar input
        scalar_deps: Vec<(ScalarVariableRef, Vec<ScalarVariableRef>)>,
    },
    // TODO how to handle literals
}
impl std::fmt::Display for VariableAbstractVMOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Declaration { new_var } => {
                let var = new_var.borrow();
                write!(
                    f,
                    "{:?}{} {}({});",
                    var.kind, var.components, var.name, var.id
                )
            }
            Self::Definition {
                new_var,
                components,
            } => {
                {
                    let var = new_var.borrow();
                    write!(
                        f,
                        "{:?}{} {}({}) = {:?}{}(",
                        var.kind, var.components, var.name, var.id, var.kind, var.components
                    )?;
                }
                for (refed_var, comp) in components {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}.{:?}, ", referenced_var.name, *comp)?;
                }
                write!(f, ");")
            }
            Self::Operation {
                output, op, inputs, ..
            } => {
                {
                    let output_var = output.0.borrow();
                    write!(
                        f,
                        "{}({}){} = {}(",
                        output_var.name, output_var.id, output.1, op
                    )?;
                }
                for (refed_var, swizz) in inputs {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}{}, ", referenced_var.name, swizz)?;
                }
                write!(f, ");")
            }
        }
    }
}

/// The state of the abstract machine at a given "tick" throughout the program
///
/// Maps each known scalar to a "scalar variable reference" - e.g. "at this point in the program the scalar ref r0.x is mapped to variable_0.y"
#[derive(Debug)]
pub struct VariableAbstractMachine<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> {
    tick: u64,
    current_scalar_names: HashMap<TVM::TScalarDataRef, ScalarVariableRef>,
    known_variables: Vec<Variable>,
    actions: Vec<(u64, VariableAbstractVMOutcome)>,
}
impl<TVM: HLSLCompatibleAbstractVM> VariableAbstractMachine<TVM> {
    pub fn new() -> Self {
        Self {
            tick: 0,
            current_scalar_names: HashMap::new(),
            known_variables: vec![],
            actions: vec![],
        }
    }

    fn add_new_variable_from_expandable<
        T: ExpandsIntoHLSLComponents<TName = TVM::TElementNameRef>,
    >(
        &mut self,
        components: u8,
        kind: DataKind,
        elem: &T,
    ) -> Variable {
        let variable = Rc::new(RefCell::new(VariableInfo {
            id: self.known_variables.len(),
            name: format!("var{:0>3}", self.known_variables.len()),
            kind,
            components,
            tick_created: self.tick,
        }));
        self.known_variables.push(variable.clone());
        for (scalar_ref, scalar_var_ref) in Self::map_to_scalar_vars(elem, &variable) {
            self.current_scalar_names.insert(scalar_ref, scalar_var_ref);
        }
        variable
    }

    fn add_new_variable_from_elem(
        &mut self,
        elem: &HLSLDataRefSpec<TVM::TElementNameRef>,
    ) -> Variable {
        self.add_new_variable_from_expandable(
            elem.base_name_ref.1.num_used_components(),
            elem.kind,
            elem,
        )
    }

    /// Take an element ref and convert it to a vector variable reference, potentially creating a new variable if necessary.
    ///
    /// A new variable is created if:
    /// This is true if:
    /// 1) the element name has not been registered before,
    ///       i.e. any component does not have a mapping in the current_scalar_names
    /// 2) any component changes kind
    /// 3) the element components do not combine to make a previously known variable
    ///
    /// In all other cases, an existing variable is returned
    fn map_elem_ref_to_variable(
        &mut self,
        elem: &HLSLDataRefSpec<TVM::TElementNameRef>,
    ) -> VectorVariableRef {
        // TODO ONLY REMAP NAMES FOR GENERAL REGISTERS

        let scalar_comps = elem.expand();
        let expected_number_of_comps = scalar_comps.len();
        let scalar_comps_as_vars: Vec<Option<_>> = scalar_comps
            .into_iter()
            .map(|scalar_comp| self.current_scalar_names.get(&scalar_comp))
            .collect();

        // Check for the 1) and 2) cases
        if scalar_comps_as_vars.iter().any(|name| {
            match name {
                // 1) at least one component has not been registered before.
                None => true,
                // 2) at least one component has a different type to it's previous usage
                Some((old_var, _)) => {
                    let old_kind = { old_var.borrow().kind };
                    old_kind != elem.kind && old_kind != DataKind::Hole
                }
            }
        }) {
            // make a new variable that doesn't depend on any previous values
            let (var, swizz) = (
                self.add_new_variable_from_elem(&elem),
                MaskedSwizzle::identity(expected_number_of_comps),
            );
            self.add_outcome(VariableAbstractVMOutcome::Declaration {
                new_var: var.clone(),
            });
            (var, swizz)
        } else {
            let scalar_comps_as_vars: Vec<ScalarVariableRef> = scalar_comps_as_vars
                .into_iter()
                .map(|name| name.unwrap().clone())
                .collect();

            let mut old_var_iter = scalar_comps_as_vars.iter(); //Rc::ptr_eq
            let (expected_old_var, _) = old_var_iter.next().unwrap();
            if old_var_iter.any(|(old_var, _)| !Rc::ptr_eq(expected_old_var, old_var)) {
                // 3) not all components map to the same old vector variable
                // make a new variable that doesn't depend on any previous values
                let (var, swizz) = (
                    self.add_new_variable_from_elem(elem),
                    MaskedSwizzle::identity(expected_number_of_comps),
                );
                self.add_outcome(VariableAbstractVMOutcome::Definition {
                    new_var: var.clone(),
                    components: scalar_comps_as_vars.into_iter().collect(),
                });
                (var, swizz)
            } else {
                // All components come from the same variable, we can just reuse that one
                (
                    expected_old_var.clone(),
                    MaskedSwizzle::new_from_vec(
                        scalar_comps_as_vars
                            .into_iter()
                            .map(|(_, comp)| comp)
                            .collect(),
                    ),
                )
            }
        }
    }

    /// Given a [ElementAbstractVM::TElementDataRef] and the [Variable] it *directly* maps to, return the (scalar data -> scalar variable) mappings
    fn map_to_scalar_vars<T: ExpandsIntoHLSLComponents<TName = TVM::TElementNameRef>>(
        elem: &T,
        variable: &Variable,
    ) -> Vec<((TVM::TElementNameRef, VectorComponent), ScalarVariableRef)> {
        // assert_eq!(
        //     variable.borrow().components,
        //     elem.base_name_ref.1.num_used_components()
        // );
        elem.expand()
            .into_iter()
            .enumerate()
            .map(|(idx, elem)| (elem, (variable.clone(), VECTOR_COMPONENTS[idx])))
            .collect()
    }

    fn add_outcome(&mut self, outcome: VariableAbstractVMOutcome) {
        println!("\t{}", outcome);
        self.actions.push((self.tick, outcome))
    }

    pub fn accum_action(&mut self, action: &dyn HLSLCompatibleAction<TVM>) {
        println!("tick {: >3}:", self.tick);
        for outcome in action.hlsl_outcomes() {
            match outcome {
                HLSLCompatibleOutcome::Declaration {
                    name,
                    literal_value,
                } => {
                    // Create a new variable based on the name
                    let var =
                        self.add_new_variable_from_expandable(name.n_components, name.kind, &name);
                    // TODO RECORD VARIABLE VALUE
                    self.add_outcome(VariableAbstractVMOutcome::Declaration {
                        new_var: var.clone(),
                    })
                }
                HLSLCompatibleOutcome::Operation {
                    opname,
                    output_elem,
                    input_elems,
                    component_deps,
                } => {
                    // Convert the input elements into variables
                    let input_vars: Vec<VectorVariableRef> = input_elems
                        .into_iter()
                        .map(|elem| self.map_elem_ref_to_variable(&elem))
                        .collect();

                    // Create a new variable for the output

                    // TODO apply an extra rule:
                    // 5) any component doesn't have a dependency on its old value
                    // let depends_on_old_variable = dep
                    //     .1
                    //     .iter()
                    //     .find(|input_var| *input_var == comps_old_variable)
                    //     .is_some();
                    // if !depends_on_old_variable {
                    //     // 2) component does not have a dependency on its old value => it's a new variable
                    //     needs_new_var = true;
                    //     break;
                    // }

                    let output_var = self.map_elem_ref_to_variable(&output_elem);

                    // Gather an equivalent to component_deps where all inputs have been converted to Variable references
                    // Do this after converting the input elements themselves, because that might have created new variables and changed the scalar mapping
                    let scalar_deps: Vec<_> = component_deps
                        .into_iter()
                        .map(|(scalar_output, scalar_inputs)| {
                            let scalar_input_vars: Vec<_> = scalar_inputs
                                .into_iter()
                                .map(|input| {
                                    if let Some(as_var) = self.current_scalar_names.get(&input.data)
                                    {
                                        (*as_var).clone()
                                    } else {
                                        panic!("Undeclared/uninitialized item used: {:?}", input)
                                    }
                                })
                                .collect();
                            let scalar_output_var = self
                                .current_scalar_names
                                .get(&scalar_output.data)
                                .unwrap()
                                .clone();
                            (scalar_output_var, scalar_input_vars)
                        })
                        .collect();

                    // TODO if we have a concretized type, try hole resolution in variables

                    self.add_outcome(VariableAbstractVMOutcome::Operation {
                        output: output_var,
                        op: opname,
                        inputs: input_vars,
                        scalar_deps,
                    })
                }
            }
        }
        self.tick += 1;
    }
}
