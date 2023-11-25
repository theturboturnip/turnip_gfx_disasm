use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    marker::PhantomData,
    rc::Rc,
};

use crate::{
    abstract_machine::{
        instructions::InstrArgs, vector::VectorComponent, VMDataRef, VMScalarNameRef,
    },
    hlsl::{
        compat::HLSLCompatibleAbstractVM,
        syntax::{ConstructorOp, HLSLOperator, Operator, UnconcreteOpTarget},
        types::HLSLKind,
        vm::{HLSLAbstractVM, HLSLAction},
        HLSLScalarDataRef, HLSLVector, HLSLVectorDataRef, HLSLSingleVectorName,
    },
    Action, Outcome, VMRef, VMVectorDataRef, VMVectorNameRef,
};
use crate::{
    abstract_machine::{
        vector::{MaskedSwizzle, VECTOR_COMPONENTS},
        VMScalarDataRef,
    },
    hlsl::syntax::UnconcreteOpResult,
};

/// An unswizzled vector available to operations in the HLSL virtual machine.
/// See [HLSLVariableInfo].
///
/// TODO HLSLVariable has a fixed lifetime (the lifetime of the variable store). We really don't need to Rc it, but we might want to RefCell it still
pub type HLSLVariableRef = usize;

/// Metadata of [HLSLVariable] i.e. an unswizzled vector available to operations in the HLSL virtual machine
#[derive(Debug)]
pub struct HLSLVariableInfo {
    pub vector_name: HLSLSingleVectorName,
    /// The index of the vector kind mask [HLSLKind] in the global list of vector kinds.
    ///
    /// Indirection is used here because variables may be connected and have the same kind.
    pub kind_idx: usize,
    pub n_components: u8,
}

/// The outcome of an action in the HLSL virtual machine
#[derive(Debug, Clone)]
pub enum HLSLOutcome {
    /// State that a new variable exists without setting its value
    Declaration { new_var: HLSLVariableRef },
    /// State that the output of an operation has been assigned to some components of a variable
    Operation {
        op: UnconcreteOpResult<HLSLVectorVarRef>,
    },
    /// State that the program flow may end early due to some vector components
    EarlyOut { inputs: Vec<HLSLScalarVarRef> },
}

/// A reference to a single scalar in the HLSL virtual machine
pub type HLSLScalarVarRef = (HLSLVariableRef, VectorComponent);
/// A reference to a swizzled vector in the HLSL virtual machine
pub type HLSLVectorVarRef = (HLSLVariableRef, MaskedSwizzle);
impl UnconcreteOpTarget for HLSLVectorVarRef {}

#[derive(Debug)]
struct VariableStore<TVM: HLSLCompatibleAbstractVM> {
    general_variables: HashMap<HLSLSingleVectorName, HLSLVariableRef>,
    inputs: HashMap<HLSLSingleVectorName, HLSLVariableRef>,
    outputs: HashMap<HLSLSingleVectorName, HLSLVariableRef>,
    next_general_var_id: u64,
    kind_mask_vec: Vec<HLSLKind>,
    /// Keeper of all variables that have ever existed. Must always grow and never have elements removed, to keep [HLSLVariableRef] indices stable
    all_variables: Vec<HLSLVariableInfo>,
    _phantom: PhantomData<TVM>,
}
impl<TVM: HLSLCompatibleAbstractVM> VariableStore<TVM> {
    pub fn new() -> Self {
        Self {
            general_variables: HashMap::new(),
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            next_general_var_id: 0,
            kind_mask_vec: vec![],
            all_variables: vec![],
            _phantom: PhantomData::default(),
        }
    }

    /// Core function of the VariableStore: insert a new variable which kind references an element in the kind_mask_vec.
    ///
    /// Exposed publically by [Self::add_new_variable_from_declaration], and internally by [Self::map_elem_ref_to_variable]
    fn add_new_variable_from_info(
        &mut self,
        vector_name: HLSLSingleVectorName,
        initial_kind: HLSLKind,
        n_components: u8,
    ) -> HLSLVariableRef {
        let vector_ref = vector_name.clone();
        // Push a new kind to the vec
        let kind_idx = {
            let kind_idx = self.kind_mask_vec.len();
            self.kind_mask_vec.push(initial_kind);
            kind_idx
        };
        // Create the variable info by referencing the previously pushed kind
        let variable = self.all_variables.len();
        self.all_variables.push(HLSLVariableInfo {
            vector_name,
            kind_idx,
            n_components,
        });
        let insert_in = match vector_ref {
            HLSLSingleVectorName::ShaderInput(_)
            | HLSLSingleVectorName::ArrayElement { .. }
            | HLSLSingleVectorName::Texture(_) => {
                // TODO ASSUMING ARRAYELEMENTS ARE READ-ONLY!
                // TODO ASSUMING TEXTURES ARE READ-ONLY!
                &mut self.inputs
            }
            HLSLSingleVectorName::ShaderOutput(_) => &mut self.outputs,
            // TODO handle literals better?
            HLSLSingleVectorName::Literal(_) => {
                // Just return the full literal here - we don't care about storing this anywhere
                return variable;
            }
            HLSLSingleVectorName::GenericRegister(_) => &mut self.general_variables,
        };
        let preexisting_variable = insert_in.insert(vector_ref.clone(), variable);
        if let Some(x) = preexisting_variable {
            panic!(
                "Inserting variable {:?} with key {:?} into VariableStore failed - already occupied by {:?}",
                variable, vector_ref, x
            )
        }
        variable
    }

    /// Given a declaration, create all relevant new variables.
    ///
    /// DOES NOT MAP THE VARIABLES TO SCALARS.
    ///
    /// Will create many variables if declaring an array.
    pub fn add_new_variable_from_compat_name(
        &mut self,
        var_name: &TVM::TVectorNameRef,
    ) -> HLSLVariableRef {
        let (vector_name, vector_kind, n_components) = TVM::vector_name_to_hlsl(&var_name);
        self.add_new_variable_from_info(vector_name, vector_kind, n_components)
    }

    /// Given a dataspec (a reference to a swizzled VM vector), create a variable with the correct type and size to hold the referenced data.
    pub fn add_new_variable_from_dataspec(
        &mut self,
        old_name: HLSLSingleVectorName,
        kind: HLSLKind,
        n_components: u8,
    ) -> HLSLVariableRef {
        let vector_name = self.remap_generic_name(old_name);
        self.add_new_variable_from_info(vector_name, kind, n_components)
    }

    /// Given an HLSL-compatible vector name emitted by a VM, return a copy where any instance of [HLSLVectorName::GenericRegister]
    /// is replaced with a fresh name generated by incrementing [Self::next_general_var_id]
    fn remap_generic_name(&mut self, old_name: HLSLSingleVectorName) -> HLSLSingleVectorName {
        use HLSLSingleVectorName::*;
        match old_name {
            Texture(_) | ShaderInput(_) | ShaderOutput(_) | Literal(_) => old_name,
            GenericRegister(_) => {
                let id = self.next_general_var_id;
                self.next_general_var_id += 1;
                HLSLSingleVectorName::GenericRegister(format!("{:>03}", id))
            }
            ArrayElement { of, idx } => ArrayElement {
                of: Box::new(self.remap_generic_name(*of)),
                idx,
            },
        }
    }

    pub fn deref_var(&self, var: HLSLVariableRef) -> &HLSLVariableInfo {
        &self.all_variables[var]
    }
    fn deref_mut_var(&mut self, var: HLSLVariableRef) -> &HLSLVariableInfo {
        &mut self.all_variables[var]
    }

    /// Given an HLSLVariable, get its kind by indexing into the kind_mask_vec
    pub fn get_variable_kind(&mut self, var: HLSLVariableRef) -> HLSLKind {
        self.kind_mask_vec[self.deref_var(var).kind_idx]
    }

    /// Given a set of HLSLVariables, rewrite them all to use the same index in the kind_mask_vec
    /// while applying an additional constraint to that mask.
    ///
    /// Borrows the first element of the iterator immutably, then borrows the rest mutably.
    pub fn constrain_var_types<I: Iterator<Item = HLSLVariableRef>>(
        &mut self,
        vars: I,
        extra_constraint: HLSLKind,
    ) -> Result<(), Vec<HLSLKind>> {
        let new_mask_idx = self.combine_mask_indices(vars.map(|v| self.deref_var(v).kind_idx))?;
        match new_mask_idx {
            None => return Ok(()),
            Some(new_mask_idx) => {
                let old_mask = self.kind_mask_vec[new_mask_idx];
                match old_mask.intersection(extra_constraint) {
                    Some(new_mask) => {
                        self.kind_mask_vec[new_mask_idx] = new_mask;
                        Ok(())
                    }
                    None => Err(vec![old_mask, extra_constraint]),
                }
            }
        }
    }

    /// Combine the masks at the given indices in kind_mask_vec
    pub fn combine_mask_indices<I: Iterator<Item = usize>>(
        &mut self,
        mut indices: I,
    ) -> Result<Option<usize>, Vec<HLSLKind>> {
        let first_idx = match indices.next() {
            Some(f) => f,
            None => return Ok(None),
        };
        let mut new_mask = self.kind_mask_vec[first_idx];
        let mut indices_set: HashSet<usize> = indices.collect();
        indices_set.remove(&first_idx);

        // debug only
        let mut merged_masks = vec![new_mask];

        // Merge the masks
        for other_idx in indices_set.iter() {
            // intersect the mask for this set with new_mask
            let other_mask = self.kind_mask_vec[*other_idx];
            merged_masks.push(other_mask);
            match new_mask.intersection(other_mask) {
                Some(v) => {
                    new_mask = v;
                }
                None => return Err(merged_masks),
            } // TODO better error
        }
        self.kind_mask_vec[first_idx] = new_mask;
        // Rewrite all the kind_idx values in all variables referencing the other masks
        for var in self.all_variables.iter_mut() {
            if indices_set.contains(&var.kind_idx) {
                var.kind_idx = first_idx;
            }
        }
        Ok(Some(first_idx))
    }

    fn concretize_var(&self, var: HLSLVariableRef) -> HLSLVector {
        let var = self.deref_var(var);
        HLSLVector {
            vector_name: var.vector_name.clone(),
            kind: self.kind_mask_vec[var.kind_idx],
            n_components: var.n_components,
        }
    }
    pub fn concretize_scalar_var_ref(&self, var: &HLSLScalarVarRef) -> HLSLScalarDataRef {
        (self.concretize_var(var.0), var.1)
    }
    pub fn concretize_vector_var_ref(&self, var: &HLSLVectorVarRef) -> HLSLVectorDataRef {
        (self.concretize_var(var.0), var.1)
    }
}

/// The state of the abstract machine at a given "tick" throughout the program
///
/// Maps each known scalar to a "scalar variable reference" - e.g. "at this point in the program the scalar ref r0.x is mapped to variable_0.y"
#[derive(Debug)]
pub struct VariableAbstractMachine<TVM: HLSLCompatibleAbstractVM> {
    tick: u64,
    variables: VariableStore<TVM>,
    /// Mapping of the VM's scalar references to (Variable, VectorComponent)
    current_scalar_names: HashMap<TVM::Scalar, HLSLScalarVarRef>,
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

    /// Given a variable, and a VM reference to a swizzled vector, map those scalars to the variable.
    /// This is done so we can convert any scalars we see later into references to the variable.
    ///
    /// Components are taken in order
    /// e.g.
    /// map(r0.wyx_, var)
    /// will map r0.w -> var.x, r0.y -> var.y, r0.x -> var.z
    fn map_dataspec_directly_to_variable(
        &mut self,
        vm_name_ref: &TVM::TVectorNameRef,
        swizzle: &MaskedSwizzle,
        variable: HLSLVariableRef,
    ) {
        let expected_n_components = self.variables.deref_var(variable).n_components as usize;
        // Filter-map over the components of the swizzle to remove None
        swizzle
            .0
            .iter()
            .filter_map(|comp| comp.map(|comp| (vm_name_ref.clone(), comp).into()))
            // Count the number of actual components used
            .enumerate()
            .map(|(idx, scalar)| {
                assert!(idx < expected_n_components);
                self.current_scalar_names
                    .insert(scalar, (variable.clone(), VECTOR_COMPONENTS[idx]));
            })
            .collect()
    }

    /// Internal function used by map_dataspec_to_dataref
    fn add_and_map_new_variable_from_info(
        &mut self,
        var_data: &TVM::TVectorDataRef,
        old_name: HLSLSingleVectorName,
        kind: HLSLKind,
        n_components: u8,
    ) -> HLSLVariableRef {
        let variable = self
            .variables
            .add_new_variable_from_dataspec(old_name, kind, n_components);
        self.map_dataspec_directly_to_variable(&var_data.name(), &var_data.swizzle(), variable);
        variable
    }

    /// Take a reference to a swizzled VM vector, plus the typemask of the intended usage as per the HLSL op,
    /// and convert it to an HLSL vector data reference,
    /// potentially creating a new variable if necessary.
    ///
    /// A new variable is created if:
    /// 1) the element name has not been registered before,
    ///       i.e. any component does not have a mapping in the current_scalar_names
    /// 2) any component is forced to change kind
    /// 3) the element components do not combine to make a previously known variable
    ///
    /// In all other cases, a swizzle of an existing variable is returned.
    ///
    /// When mapping the output of an operation it is generally wise to always create a new variable, unless that output is to a "special" register.
    /// Setting the second argument to true will force a new variable to be created if the original is a [HLSLNameRefType::GenericRegister]
    fn map_dataspec_to_dataref(
        &mut self,
        var_data: &TVM::TVectorDataRef,
        intersect_with_kind: HLSLKind,
        force_new_general_purpose_var: bool,
    ) -> HLSLVectorVarRef {
        // TODO ONLY REMAP NAMES FOR GENERAL REGISTERS
        let (old_name, dataspec_kind, n_components) = TVM::vector_data_to_hlsl(var_data);
        let dataspec_kind = dataspec_kind.intersection(intersect_with_kind).unwrap();

        let scalar_comps = var_data.decompose();
        let expected_number_of_comps = scalar_comps.len();
        let scalar_comps_as_vars: Vec<Option<_>> = scalar_comps
            .into_iter()
            .map(|scalar_comp| self.current_scalar_names.get(&scalar_comp))
            .collect();

        // Check for the 1) and 2) cases
        if (matches!(old_name, HLSLSingleVectorName::GenericRegister(_)) && force_new_general_purpose_var)
            || scalar_comps_as_vars.iter().any(|name| {
                match name {
                    // 1) at least one component has not been registered before.
                    None => true,
                    // 2) at least one component has a different type to it's previous usage
                    Some((old_var, _)) => {
                        let old_kind = self.variables.get_variable_kind(*old_var);
                        // If the new kind and the old kind don't intersect, must be a new variable
                        dataspec_kind.intersection(old_kind).is_none()
                    }
                }
            })
        {
            // make a new variable that doesn't depend on any previous values
            let variable = self.add_and_map_new_variable_from_info(
                &var_data,
                old_name,
                dataspec_kind,
                n_components,
            );
            self.add_outcome(HLSLOutcome::Declaration {
                new_var: variable.clone(),
            });
            (variable, MaskedSwizzle::identity(expected_number_of_comps))
        } else {
            let scalar_comps_as_vars: Vec<HLSLScalarVarRef> = scalar_comps_as_vars
                .into_iter()
                .map(|name| name.unwrap().clone())
                .collect();

            let mut old_var_iter = scalar_comps_as_vars.iter(); //Rc::ptr_eq
            let (expected_old_var, _) = old_var_iter.next().unwrap();
            if old_var_iter.any(|(old_var, _)| *expected_old_var != *old_var) {
                // 3) not all components map to the same old vector variable
                // make a new variable that doesn't depend on any previous values
                let variable = self.add_and_map_new_variable_from_info(
                    &var_data,
                    old_name,
                    dataspec_kind,
                    n_components,
                );
                // Link the types of this new variable to the types of the variables it's made out of
                let mut composite_variables: Vec<_> = scalar_comps_as_vars
                    .iter()
                    .map(|(v, _)| v.clone())
                    .collect();
                composite_variables.push(variable.clone());
                match self.variables.constrain_var_types(
                    composite_variables.clone().into_iter(),
                    dataspec_kind,
                ){
                    Ok(()) => {}
                    Err(bad_combo) => panic!(
                        "Tried to merge incompatible constraints {:?} when creating variable {:?} from composites {:?}",
                        bad_combo, variable, composite_variables
                    ),
                }

                self.add_outcome(HLSLOutcome::Declaration {
                    new_var: variable.clone(),
                });

                // Compute an HLSLOutcome::Operator to generate the new variable
                // If we're just assigning one vector
                let scalar_comps_as_vecs: Vec<_> = scalar_comps_as_vars
                    .into_iter()
                    .map(|(var, comp)| (var, MaskedSwizzle::new([Some(comp), None, None, None])))
                    .collect();
                let op = match expected_number_of_comps {
                    1 => HLSLOperator::Assign,
                    2 => HLSLOperator::Constructor(ConstructorOp::Vec2),
                    3 => HLSLOperator::Constructor(ConstructorOp::Vec3),
                    4 => HLSLOperator::Constructor(ConstructorOp::Vec4),
                    _ => panic!(
                        "Impossible expected_number_of_comps: {}",
                        expected_number_of_comps
                    ),
                };
                self.add_outcome(HLSLOutcome::Operation {
                    op: UnconcreteOpResult::new(
                        op,
                        scalar_comps_as_vecs,
                        (
                            variable.clone(),
                            MaskedSwizzle::identity(expected_number_of_comps),
                        ),
                    ),
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
        self.actions.push((self.tick, outcome))
    }

    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for outcome in action.outcomes() {
            match outcome {
                Outcome::Declare(var_name) => {
                    // Create a new variable based on the name
                    // TODO we can't really handle arrays well like this :(
                    let n_components = var_name.n_components();

                    let var = self.variables.add_new_variable_from_compat_name(&var_name);

                    // Map the variable
                    self.map_dataspec_directly_to_variable(
                        &var_name,
                        &MaskedSwizzle::identity(n_components as usize),
                        var,
                    );

                    self.add_outcome(HLSLOutcome::Declaration { new_var: var });
                }
                Outcome::Assign { op, inputs, output } => {
                    let typespec = op.get_typespec();

                    // Convert the input elements into variables
                    let basic_op_input_types = typespec.get_basic_input_types();
                    let input_datarefs: Vec<HLSLVectorVarRef> = inputs
                        .into_iter()
                        .enumerate()
                        .map(|(i, elem)| {
                            self.map_dataspec_to_dataref(&elem, basic_op_input_types[i], false)
                        })
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

                    let output_dataref = self.map_dataspec_to_dataref(
                        &output,
                        typespec.get_basic_output_type(),
                        true,
                    );

                    // Apply the type constraints from this operation
                    for (type_constraint, constrained_operand_is) in typespec.get_type_constraints()
                    {
                        let constrained_operand_refs =
                            constrained_operand_is.into_iter().map(|i| {
                                if i == input_datarefs.len() {
                                    output_dataref.0.clone()
                                } else {
                                    input_datarefs[i].0.clone()
                                }
                            });
                        match self
                            .variables
                            .constrain_var_types(constrained_operand_refs, type_constraint)
                        {
                            Ok(()) => {}
                            Err(bad_combo) => panic!(
                                "Tried to merge incompatible constraints {:?} from operation {:?} on {:?} and {:?}",
                                bad_combo, op, input_datarefs, output_dataref
                            ),
                        }
                    }

                    let op = UnconcreteOpResult::new(op, input_datarefs, output_dataref);

                    self.add_outcome(HLSLOutcome::Operation { op })
                }
                Outcome::EarlyOut { inputs } => {
                    let scalar_input_vars: Vec<_> = inputs
                        .into_iter()
                        .map(|input| {
                            if let Some(as_var) =
                                self.current_scalar_names.get(&input.scalar_name())
                            {
                                (*as_var).clone()
                            } else {
                                panic!("Undeclared/uninitialized item used: {:?}", input)
                            }
                        })
                        .collect();
                    self.add_outcome(HLSLOutcome::EarlyOut {
                        inputs: scalar_input_vars,
                    });
                }
            }
        }
        self.tick += 1;
    }

    pub fn actions(&self) -> Vec<HLSLAction> {
        self.actions
            .iter()
            .map(|(_, outcome)| match outcome {
                HLSLOutcome::Declaration { new_var } => {
                    HLSLAction::Declare(self.variables.concretize_var(*new_var))
                }
                HLSLOutcome::Operation { op } => {
                    let inputs = op
                        .inputs
                        .iter()
                        .map(|v| self.variables.concretize_vector_var_ref(v))
                        .collect();
                    let output = self.variables.concretize_vector_var_ref(&op.output);

                    // Put the args inside an InstrArgs so we can do dependency resolution
                    let args = InstrArgs::<HLSLAbstractVM> {
                        inputs,
                        outputs: vec![output.clone()], // TODO ugh this sucks
                    };

                    HLSLAction::Assign {
                        op: op.op,
                        inputs: args.inputs,
                        output: output,
                    }
                }
                HLSLOutcome::EarlyOut { inputs } => HLSLAction::EarlyOut {
                    inputs: inputs
                        .iter()
                        .map(|var| self.variables.concretize_scalar_var_ref(&var))
                        .collect(),
                },
            })
            .collect()
    }
}
