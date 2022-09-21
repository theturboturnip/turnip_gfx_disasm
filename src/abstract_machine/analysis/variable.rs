use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::abstract_machine::{
    vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
    DataKind, ElementAbstractVM, ElementAction, ElementOutcome, TypedRef,
};

// pub trait ScalarVariableRef: Eq + Clone {
//     fn get_variable_ref(&self) -> &Variable;
// }

pub type ScalarVariableRef = (Variable, VectorComponent);
pub type VectorVariableRef = (Variable, MaskedSwizzle);

/// Variable analysis is possible on vector and scalar VMs, but needs to be able to refer to specifically scalar values.
pub trait VariableCapableAbstractVM: ElementAbstractVM {
    /// Returns a (name, components) pair which informs the value of a variable mapping to the given element.
    fn variable_info(elem: &Self::TElementDataRef, unique_id: u64) -> (String, u8);
}

// pub trait VariableCapableAction<TVM: VariableCapableAbstractVM> {
//     /// Equivalent to [Action::outcomes], except the outcomes are grouped together by which "variables" they could affect.
//     fn variable_grouped_outcomes(&self) -> Vec<Vec<Outcome<TVM>>>;
// }

///
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
    },
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
            Self::Operation { output, op, inputs } => {
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
pub struct VariableAbstractMachine<TVM: VariableCapableAbstractVM> {
    tick: u64,
    current_scalar_names: HashMap<TVM::TScalarDataRef, ScalarVariableRef>,
    known_variables: Vec<Variable>,
    actions: Vec<(u64, VariableAbstractVMOutcome)>,
}
impl<TVM: VariableCapableAbstractVM> VariableAbstractMachine<TVM> {
    pub fn new() -> Self {
        Self {
            tick: 0,
            current_scalar_names: HashMap::new(),
            known_variables: vec![],
            actions: vec![],
        }
    }

    fn add_new_variable_from_elem(
        &mut self,
        elem: &TVM::TElementDataRef,
        kind: DataKind,
    ) -> Variable {
        let (name, components) = TVM::variable_info(elem, self.known_variables.len() as u64);
        let variable = Rc::new(RefCell::new(VariableInfo {
            id: self.known_variables.len(),
            name,
            kind,
            components,
            tick_created: self.tick,
        }));
        // println!(
        //     "\t creating variable {} for elem {:?}",
        //     &variable.borrow().name,
        //     elem
        // );
        self.known_variables.push(variable.clone());
        for (scalar_ref, scalar_var_ref) in Self::map_to_scalar_vars(elem, &variable) {
            self.current_scalar_names.insert(scalar_ref, scalar_var_ref);
        }
        variable
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
        elem: TypedRef<TVM::TElementDataRef>,
    ) -> VectorVariableRef {
        let scalar_comps = TVM::expand_element(&elem.data);
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
                self.add_new_variable_from_elem(&elem.data, elem.kind),
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
                    self.add_new_variable_from_elem(&elem.data, elem.kind),
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
    fn map_to_scalar_vars(
        elem: &TVM::TElementDataRef,
        variable: &Variable,
    ) -> Vec<(TVM::TScalarDataRef, ScalarVariableRef)> {
        // Check the length of the element is equal to the length of the variable
        assert_eq!(TVM::variable_info(elem, 0).1, variable.borrow().components);
        TVM::expand_element(elem)
            .into_iter()
            .enumerate()
            .map(|(idx, elem)| (elem, (variable.clone(), VECTOR_COMPONENTS[idx])))
            .collect()
    }

    fn add_outcome(&mut self, outcome: VariableAbstractVMOutcome) {
        println!("\t{}", outcome);
        self.actions.push((self.tick, outcome))
    }

    pub fn accum_action(&mut self, action: &dyn ElementAction<TVM>) {
        println!("tick {: >3}:", self.tick);
        for outcome in action.per_element_outcomes() {
            match outcome {
                ElementOutcome::Declaration { name, value } => {
                    // Create a new variable based on the name
                    let var = self.add_new_variable_from_elem(
                        &name,
                        (&value)
                            .as_ref()
                            .map_or(DataKind::Hole, |typed_ref| typed_ref.kind),
                    );
                    // TODO RECORD VARIABLE ORIGIN
                    self.add_outcome(VariableAbstractVMOutcome::Declaration {
                        new_var: var.clone(),
                    })
                }
                ElementOutcome::Dependency {
                    opname,
                    output_elem,
                    input_elems,
                    component_deps,
                } => {
                    // Convert the input elements into variables
                    let input_vars: Vec<VectorVariableRef> = input_elems
                        .into_iter()
                        .map(|elem| self.map_elem_ref_to_variable(elem))
                        .collect();

                    // Gather an equivalent to component_deps where all inputs have been converted to Variable references
                    // Do this after converting the input elements themselves, because that might have created new variables and changed the scalar mapping
                    let component_dep_vars: Vec<_> = component_deps
                        .into_iter()
                        .map(|(output, inputs)| {
                            let input_vars: Vec<_> = inputs
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
                            (output, input_vars)
                        })
                        .collect();

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

                    // allow_differing_lengths is actually OK to be true - if we remove it, things like "write o1.w" become separate variables
                    let output_var = self.map_elem_ref_to_variable(output_elem);
                    // assert_eq!(
                    //     output_var.0.borrow().components as usize,
                    //     component_dep_vars.len()
                    // );

                    // TODO if we have a concretized type, try hole resolution in variables

                    self.add_outcome(VariableAbstractVMOutcome::Operation {
                        output: output_var,
                        op: opname,
                        inputs: input_vars,
                    })
                }
            }
        }
        self.tick += 1;
    }
}
