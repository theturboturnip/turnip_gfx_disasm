use std::{cell::RefCell, collections::HashMap, marker::PhantomData, rc::Rc};

use crate::abstract_machine::{
    hlsl::{
        compat::{
            ExpandsIntoHLSLComponents, HLSLCompatibleAbstractVM, HLSLCompatibleAction,
            HLSLCompatibleOutcome, HLSLCompatibleScalarRef, HLSLDataRefSpec, HLSLDataRefType,
            HLSLDeclarationSpec, HLSLDeclarationSpecType,
        },
        HLSLElementRef, HLSLOutcome, HLSLScalarElementRef, HLSLVariable, HLSLVariableInfo,
        HLSLVectorRef,
    },
    vector::{MaskedSwizzle, VECTOR_COMPONENTS},
    DataKind, ScalarAbstractVM,
};

#[derive(Debug)]
struct VariableStore<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> {
    general_variables: HashMap<HLSLVectorRef, HLSLVariable>,
    inputs: HashMap<HLSLVectorRef, HLSLVariable>,
    outputs: HashMap<HLSLVectorRef, HLSLVariable>,
    next_general_var_id: u64,
    _phantom: PhantomData<TVM>,
}
impl<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> VariableStore<TVM> {
    pub fn new() -> Self {
        Self {
            general_variables: HashMap::new(),
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            next_general_var_id: 0,
            _phantom: PhantomData::default(),
        }
    }

    /// Core function of the VariableStore: insert a new variable
    ///
    /// Exposed publically by [Self::add_new_variable_from_declaration], and internally by [Self::map_elem_ref_to_variable]
    fn add_new_variable_from_info(&mut self, variable_info: HLSLVariableInfo) -> HLSLVariable {
        let vector_ref = variable_info.vector_ref.clone();
        let variable = Rc::new(RefCell::new(variable_info));
        let insert_in = match vector_ref {
            HLSLVectorRef::ShaderInput(_) | HLSLVectorRef::ArrayElement { .. } => {
                // TODO ASSUMING ARRAYELEMENTS ARE READ-ONLY!
                &mut self.inputs
            }
            HLSLVectorRef::ShaderOutput(_) => &mut self.outputs,
            // TODO handle literals better?
            HLSLVectorRef::Literal(_) => {
                // Just return the full literal here - we don't care about storing this anywhere
                return variable;
            }
            HLSLVectorRef::GenericRegister(_) => &mut self.general_variables,
        };
        let preexisting_variable = insert_in.insert(vector_ref.clone(), variable.clone());
        if let Some(x) = preexisting_variable {
            panic!(
                "Inserting with key {:?} into VariableStore failed - already occupied by {:?}",
                vector_ref, x
            )
        }
        variable
    }

    /// Given a
    fn vector_refs_for_declspec(&mut self, decl_type: HLSLDeclarationSpecType) -> HLSLVectorRef {
        match decl_type {
            HLSLDeclarationSpecType::GenericRegister => {
                let id = self.next_general_var_id;
                self.next_general_var_id += 1;
                HLSLVectorRef::GenericRegister(id)
            }
            HLSLDeclarationSpecType::ShaderInput(name) => HLSLVectorRef::ShaderInput(name),
            HLSLDeclarationSpecType::ShaderOutput(name) => HLSLVectorRef::ShaderOutput(name),
            // HLSLDeclarationSpecType::Array { of, len } => {
            //     let mut next_level_up = self.vector_refs_for_declaration(*of);
            //     // for each element of this level of array...
            //     (0..len)
            //         .into_iter()
            //         .map(|idx| {
            //             // wrap all elements of the next-level-up in a array type
            //             next_level_up
            //                 .iter()
            //                 .map(move |next_item| HLSLVectorRef::ArrayElement {
            //                     of: Box::new(next_item.clone()),
            //                     idx,
            //                 })
            //         })
            //         .flatten()
            //         .collect()
            // }
        }
    }

    /// Given a [HLSLDeclarationSpec], produce information on the variable it declares.
    fn variable_for_declspec(
        &mut self,
        declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
    ) -> HLSLVariableInfo {
        HLSLVariableInfo {
            vector_ref: self.vector_refs_for_declspec(declspec.decl_type),
            kind: declspec.kind,
            n_components: declspec.n_components,
        }
        // self.vector_refs_for_declaration(compat.decl_type)
        //     .into_iter()
        //     .map(|vector_ref| HLSLVariableInfo {
        //         vector_ref,
        //         kind: compat.kind,
        //         n_components: compat.n_components,
        //     })
        //     .collect()
    }

    fn expand_declspec_to_scalars(
        declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
    ) -> Vec<HLSLCompatibleScalarRef<TVM::TElementNameRef>> {
        (0..declspec.n_components)
            .into_iter()
            .map(|i| (declspec.vm_name_ref.clone(), VECTOR_COMPONENTS[i as usize]))
            .collect()
    }

    /// Given a declaration, create all relevant new variables.
    ///
    /// DOES NOT MAP THE VARIABLES TO SCALARS.
    ///
    /// Will create many variables if declaring an array.
    pub fn add_new_variable_from_declspec(
        &mut self,
        declspec: HLSLDeclarationSpec<TVM::TElementNameRef>,
    ) -> HLSLVariable {
        let var = self.variable_for_declspec(declspec);
        self.add_new_variable_from_info(var)
    }

    /// Given a dataspec (a reference to a swizzled VM vector), create a variable with the correct type and size to hold the referenced data.
    pub fn add_new_variable_from_dataspec(
        &mut self,
        dataspec: &HLSLDataRefSpec<TVM::TElementNameRef>,
    ) -> HLSLVariable {
        let info = self.variable_info_for_dataspec(dataspec);
        self.add_new_variable_from_info(info)
    }

    /// Given a dataspec (a reference to a swizzled VM vector), create variable info with the correct type and size to hold the referenced data.
    fn variable_info_for_dataspec(
        &mut self,
        dataspec: &HLSLDataRefSpec<TVM::TElementNameRef>,
    ) -> HLSLVariableInfo {
        HLSLVariableInfo {
            vector_ref: self.vector_ref_for_datareftype(dataspec.ref_type.clone()),
            kind: dataspec.kind,
            n_components: dataspec.vm_data_ref.1.num_used_components(),
        }
    }

    /// Given the type of a data reference, return the corresponding vector name.
    ///
    /// Separate from [variable_info_for_dataspec] because it needs to be recursive for arrays.
    fn vector_ref_for_datareftype(&mut self, ref_type: HLSLDataRefType) -> HLSLVectorRef {
        match ref_type {
            HLSLDataRefType::GenericRegister => {
                let id = self.next_general_var_id;
                self.next_general_var_id += 1;
                HLSLVectorRef::GenericRegister(id)
            }
            HLSLDataRefType::ShaderInput(name) => HLSLVectorRef::ShaderInput(name),
            HLSLDataRefType::ShaderOutput(name) => HLSLVectorRef::ShaderOutput(name),
            HLSLDataRefType::Literal(data) => HLSLVectorRef::Literal(data),
            HLSLDataRefType::ArrayElement { of, idx } => HLSLVectorRef::ArrayElement {
                of: Box::new(self.vector_ref_for_datareftype(*of)),
                idx,
            },
        }
    }
}

/// The state of the abstract machine at a given "tick" throughout the program
///
/// Maps each known scalar to a "scalar variable reference" - e.g. "at this point in the program the scalar ref r0.x is mapped to variable_0.y"
#[derive(Debug)]
pub struct VariableAbstractMachine<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> {
    tick: u64,
    variables: VariableStore<TVM>,
    /// Mapping of the VM's scalar references to (Variable, VectorComponent)
    current_scalar_names:
        HashMap<HLSLCompatibleScalarRef<TVM::TElementNameRef>, HLSLScalarElementRef>,
    actions: Vec<(u64, HLSLOutcome)>,
}
impl<TVM: HLSLCompatibleAbstractVM> VariableAbstractMachine<TVM> {
    pub fn new() -> Self {
        Self {
            tick: 0,
            variables: VariableStore::new(),
            current_scalar_names: HashMap::new(),
            actions: vec![],
        }
    }

    /// Given a variable, and a VM reference to a set of scalars, map those scalars to the variable
    ///
    /// Components are taken in order
    /// e.g.
    /// map(r0.wyx_, var)
    /// will map r0.w -> var.x, r0.y -> var.y, r0.x -> var.z
    pub fn map_dataref_directly_to_variable(
        &mut self,
        vm_data_ref: &(TVM::TElementNameRef, MaskedSwizzle),
        variable: &HLSLVariable,
    ) {
        let expected_n_components = variable.borrow().n_components as usize;
        // Filter-map over the components of the swizzle to remove None
        vm_data_ref
            .1
             .0
            .iter()
            .filter_map(|comp| comp.map(|comp| (vm_data_ref.0.clone(), comp)))
            // Count the number of actual components used
            .enumerate()
            .map(|(idx, scalar)| {
                assert!(idx < expected_n_components);
                self.current_scalar_names
                    .insert(scalar, (variable.clone(), VECTOR_COMPONENTS[idx]));
            })
            .collect()
    }

    /// Take a dataspec (reference to a swizzled VM vector) and convert it to a vector variable reference, potentially creating a new variable if necessary.
    ///
    /// A new variable is created if:
    /// This is true if:
    /// 1) the element name has not been registered before,
    ///       i.e. any component does not have a mapping in the current_scalar_names
    /// 2) any component changes kind
    /// 3) the element components do not combine to make a previously known variable
    ///
    /// In all other cases, an existing variable is returned
    fn map_dataspec_to_variable(
        &mut self,
        dataref: &HLSLDataRefSpec<TVM::TElementNameRef>,
    ) -> HLSLElementRef {
        // TODO ONLY REMAP NAMES FOR GENERAL REGISTERS

        let scalar_comps = dataref.expand();
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
                    old_kind != dataref.kind && old_kind != DataKind::Hole
                }
            }
        }) {
            // make a new variable that doesn't depend on any previous values
            let variable = self.variables.add_new_variable_from_dataspec(&dataref);
            self.map_dataref_directly_to_variable(&dataref.vm_data_ref, &variable);
            self.add_outcome(HLSLOutcome::Declaration {
                new_var: variable.clone(),
            });
            (variable, MaskedSwizzle::identity(expected_number_of_comps))
        } else {
            let scalar_comps_as_vars: Vec<HLSLScalarElementRef> = scalar_comps_as_vars
                .into_iter()
                .map(|name| name.unwrap().clone())
                .collect();

            let mut old_var_iter = scalar_comps_as_vars.iter(); //Rc::ptr_eq
            let (expected_old_var, _) = old_var_iter.next().unwrap();
            if old_var_iter.any(|(old_var, _)| !Rc::ptr_eq(expected_old_var, old_var)) {
                // 3) not all components map to the same old vector variable
                // make a new variable that doesn't depend on any previous values
                let variable = self.variables.add_new_variable_from_dataspec(dataref);
                self.map_dataref_directly_to_variable(&dataref.vm_data_ref, &variable);
                self.add_outcome(HLSLOutcome::Definition {
                    new_var: variable.clone(),
                    components: scalar_comps_as_vars.into_iter().collect(),
                });
                (variable, MaskedSwizzle::identity(expected_number_of_comps))
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

    fn add_outcome(&mut self, outcome: HLSLOutcome) {
        println!("\t{}", outcome);
        self.actions.push((self.tick, outcome))
    }

    pub fn accum_action(&mut self, action: &dyn HLSLCompatibleAction<TVM>) {
        println!("tick {: >3}:", self.tick);
        for outcome in action.hlsl_outcomes() {
            match outcome {
                HLSLCompatibleOutcome::Declaration {
                    declspec,
                    literal_value,
                } => {
                    // Create a new variable based on the name
                    // TODO we can't really handle arrays well like this :(
                    let n_components = declspec.n_components;
                    let var = self
                        .variables
                        .add_new_variable_from_declspec(declspec.clone());
                    // self.add_new_variable_from_expandable(name.n_components, name.kind, &name);

                    // Map the variable
                    self.map_dataref_directly_to_variable(
                        &(
                            declspec.vm_name_ref,
                            MaskedSwizzle::identity(n_components as usize),
                        ),
                        &var,
                    );

                    match literal_value {
                        None => {
                            self.add_outcome(HLSLOutcome::Declaration {
                                new_var: var.clone(),
                            });
                        }
                        Some(literal_val) => {
                            let literal =
                                self.variables.add_new_variable_from_info(HLSLVariableInfo {
                                    vector_ref: HLSLVectorRef::Literal(literal_val.data),
                                    kind: literal_val.kind,
                                    n_components,
                                });
                            self.add_outcome(HLSLOutcome::Definition {
                                new_var: var.clone(),
                                components: (0..n_components)
                                    .into_iter()
                                    .map(|idx| (literal.clone(), VECTOR_COMPONENTS[idx as usize]))
                                    .collect(),
                            })
                        }
                    }
                }
                HLSLCompatibleOutcome::Operation {
                    opname,
                    output_dataspec,
                    input_dataspecs,
                    component_deps,
                } => {
                    // Convert the input elements into variables
                    let input_vars: Vec<HLSLElementRef> = input_dataspecs
                        .into_iter()
                        .map(|elem| self.map_dataspec_to_variable(&elem))
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

                    let output_var = self.map_dataspec_to_variable(&output_dataspec);

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

                    self.add_outcome(HLSLOutcome::Operation {
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
