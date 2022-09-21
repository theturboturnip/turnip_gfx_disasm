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

/// The state of the abstract machine at a given "tick" throughout the program
///
/// Maps each known scalar to a "scalar variable reference" - e.g. "at this point in the program the scalar ref r0.x is mapped to variable_0.y"
#[derive(Debug)]
pub struct VariableAbstractMachine<TVM: VariableCapableAbstractVM> {
    tick: u64,
    current_scalar_names: HashMap<TVM::TScalarDataRef, ScalarVariableRef>,
    known_variables: Vec<Variable>,
    actions: Vec<(u64, VectorVariableRef, Vec<VectorVariableRef>)>,
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
    /// 4) the element components *do* combine to make a previously known variable,
    ///    but the length of that variable is different from the element's own length
    ///    (only applied if `allow_differing_lengths` argument is false)
    ///
    /// In all other cases, an existing variable is returned
    fn map_elem_ref_to_variable(
        &mut self,
        elem: TypedRef<TVM::TElementDataRef>,
        allow_differing_lengths: bool,
    ) -> VectorVariableRef {
        let mut needs_new_var = false;
        // The ID of the "old variable" - a variable which all the output components map to, which may be reused
        let mut id_of_old_variable = None;
        // The constituent components of the "old variable" that each output component maps to
        let mut comps_of_old_variable = vec![];
        let scalar_comps = TVM::expand_element(&elem.data);
        let expected_number_of_comps = scalar_comps.len();
        for scalar_comp in scalar_comps {
            match self.current_scalar_names.get(&scalar_comp) {
                None => {
                    // 1) component has not been registered before
                    println!("\tnew var because new elem {:?}", scalar_comp);
                    needs_new_var = true;
                    break;
                }
                Some((scalar_comps_old_variable, comp_of_old_variable)) => {
                    // Get the kind and ID of the "old variable"
                    let (old_kind, old_id) = {
                        let var = scalar_comps_old_variable.borrow();
                        (var.kind, var.id)
                    };
                    if old_kind != elem.kind && old_kind != DataKind::Hole {
                        // 2) component has changed type
                        println!(
                            "\tnew var because elem {:?} changed type from {:?} to {:?}",
                            scalar_comp, old_kind, elem.kind
                        );
                        needs_new_var = true;
                        break;
                    }

                    match id_of_old_variable {
                        Some(expected_old_id) if expected_old_id == old_id => {}
                        None => {
                            id_of_old_variable = Some(old_id);
                        }
                        Some(_) => {
                            // One of the previously examined components maps to a different variable than this component
                            // => not all output components were mapped to the same old variable
                            // => 3) the output components do not combine to make a previously known variable
                            println!("\tnew var because elem {:?} had a different mapped var from previous components", scalar_comp);
                            needs_new_var = true;
                            break;
                        }
                    }

                    comps_of_old_variable.push(*comp_of_old_variable);
                }
            }
        }

        let mut old_variable_to_use = id_of_old_variable.map(|id| &self.known_variables[id]);

        if !allow_differing_lengths {
            match old_variable_to_use {
                Some(var) => {
                    if var.borrow().components as usize != expected_number_of_comps {
                        // 4) the output components *do* combine to make a previously known variable,
                        //    but the length of that variable is different from the expected output
                        needs_new_var = true;
                    }
                }
                None => {}
            }
        }

        // Combine the "needs_new_var" flag with old_variable_to_use
        if needs_new_var {
            old_variable_to_use = None;
        }

        // If old_variable_to_use is not None, i.e. we found an old variable and didn't set needs_new_var to false,
        // use the old variable
        if let Some(var) = old_variable_to_use {
            (
                var.clone(),
                MaskedSwizzle::new_from_vec(comps_of_old_variable),
            )
        } else {
            // Otherwise, create a new variable
            let (var, swizz) = (
                self.add_new_variable_from_elem(&elem.data, elem.kind),
                MaskedSwizzle::identity(expected_number_of_comps),
            );
            // TODO RECORD VARIABLE ORIGIN
            println!(
                "\t{:?}{} {} === {:?};",
                var.borrow().kind,
                var.borrow().components,
                var.borrow().name,
                elem,
            );
            (var, swizz)
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
                    println!(
                        "\t{:?}{} {} === {:?};",
                        var.borrow().kind,
                        var.borrow().components,
                        var.borrow().name,
                        value,
                    );
                }
                ElementOutcome::Dependency {
                    output_elem,
                    input_elems,
                    component_deps,
                } => {
                    // Convert the input elements into variables
                    let input_vars: Vec<VectorVariableRef> = input_elems
                        .into_iter()
                        .map(|elem| self.map_elem_ref_to_variable(elem, true))
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
                                        panic!("Undeclared/uninitialized item used")
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
                    let output_var = self.map_elem_ref_to_variable(output_elem, true);
                    // assert_eq!(
                    //     output_var.0.borrow().components as usize,
                    //     component_dep_vars.len()
                    // );

                    // TODO if we have a concretized type, try hole resolution in variables

                    // TODO record a proper action that states which variables created this variable
                    print!("\t{}{} <- ", output_var.0.borrow().name, output_var.1);
                    for in_ref in input_vars.iter() {
                        print!("{}{}, ", in_ref.0.borrow().name, in_ref.1)
                    }
                    print!(";\n");
                    self.actions.push((self.tick, output_var, input_vars))
                }
            }
        }
        self.tick += 1;
    }
}
