use std::{cell::RefCell, rc::Rc, collections::{HashMap, HashSet}};

use crate::{hlsl::{kinds::{HLSLKind, HLSLOperandKind, HLSLKindBitmask}, compat::{HLSLCompatibleAbstractVM, HLSLCompatProgram}, HLSLRegister, vm::HLSLAbstractVM, HLSLScalar, HLSLAction, syntax::{Operator, HLSLOperator}}, abstract_machine::{vector::{VectorComponent, VectorOf}, VMName, VMVector, VMScalar}, AbstractVM, Program, Action};


type MutRef<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Variable {
    name: HLSLRegister,
    n_contig_components: usize,
    kind: HLSLKind,
}
impl Variable {
    fn new(name: HLSLRegister, kind: HLSLKind) -> Self {
        let kind = name.toplevel_kind().intersection(kind).expect("Tried to create a variable with a mismatching name and kind");
        // eprintln!("Created var {name} as {kind}");
        Self {
            n_contig_components: name.n_components(),
            name,
            kind,
        }
    }

    // Refining can only happen inside here, because this machine doesn't use <v as Variable>.toplevel_kind().
    // toplevel_kind() is necessary for other analyses to use
    fn refine(&mut self, hlsl_kind: HLSLKind) -> Option<HLSLKind> {
        self.kind = self.kind.intersection(hlsl_kind)?;
        Some(self.kind)
    }
}
impl VMName for Variable {
    fn is_pure_input(&self) -> bool {
        self.name.is_pure_input()
    }

    fn is_output(&self) -> bool {
        self.name.is_output()
    }

    fn toplevel_kind(&self) -> HLSLKind {
        self.kind
    }
}
impl VMVector for Variable {
    fn n_components(&self) -> usize {
        self.n_contig_components
    }
}

#[derive(Debug, Clone)]
enum MutScalarVar {
    Component(MutRef<Variable>, VectorComponent), // TODO casts?
    Literal(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ScalarVar {
    vec: Rc<Variable>,
    comp: VectorComponent,
    cast_to: Option<HLSLKind>,
}
impl VMName for ScalarVar {
    fn is_pure_input(&self) -> bool {
        self.vec.is_pure_input()
    }

    fn is_output(&self) -> bool {
        self.vec.is_output()
    }

    fn toplevel_kind(&self) -> HLSLKind {
        self.vec.toplevel_kind()
    }
}
impl VMScalar for ScalarVar {

}
type MutVarAction = Action<Vec<MutScalarVar>, MutScalarVar>;

#[derive(Debug, Clone)]
enum VariableVM {}
impl AbstractVM for VariableVM {
    type Scalar = ScalarVar;

    type Register = Variable;

    type Vector = VectorOf<ScalarVar>;

    fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
        v.ts.clone()
    }
}
// impl HLSLCompatibleAbstractVM for VariableVM {

// }

fn find_common<'a, TIn: 'a, TOut: PartialEq, I: Iterator<Item = &'a TIn>, F: Fn(&TIn) -> Option<TOut>>(mut iter: I, f: F) -> Option<TOut> {
    let init = f(iter.next().unwrap())?;
    for i in iter {
        if f(i)? != init {
            return None
        }
    }
    Some(init)
}

type ScalarKey = (HLSLRegister, VectorComponent);
type VariableComp = (MutRef<Variable>, VectorComponent);

/// Whenever a new (conditional) scope is created:
/// - it may create new mappings to use inside the scope, which aren't valid outside
/// - it makes any scalar keys used inside of it invalid, because they won't map to the variables they did at the start of the scope.
struct ScopeOverlay {
    overlay_mappings: HashMap<ScalarKey, VariableComp>,
    keys_to_clear: HashSet<ScalarKey>,
}

struct VariableScalarMap {
    scalar_to_var: HashMap<ScalarKey, VariableComp>,
    scopes: Vec<ScopeOverlay>,
}
impl VariableScalarMap {
    fn new() -> Self {
        Self {
            scalar_to_var: HashMap::new(),
            scopes: vec![],
        }
    }
    fn lookup(&self, reg: HLSLRegister, reg_comp: VectorComponent) -> Option<VariableComp> {
        let key = (reg, reg_comp);
        for scope in self.scopes.iter().rev() {
            match scope.overlay_mappings.get(&key) {
                Some((var, c)) => return Some((var.clone(), *c)),
                None => continue,
            }
        }
        self.scalar_to_var.get(&key).map(|(var, c)| (var.clone(), *c))
    }
    fn update(&mut self, reg: HLSLRegister, reg_comp: VectorComponent, var: MutRef<Variable>, var_comp: VectorComponent) {
        if reg.is_pure_input() {
            self.scalar_to_var.insert((reg, reg_comp), (var, var_comp));
        } else {
            match self.scopes.last_mut() {
                Some(scope) => {
                    if reg.is_output() {
                        panic!("Shouldn't be creating input/output mappings inside a scope!")
                    }
                    scope.keys_to_clear.insert((reg.clone(), reg_comp));
                    scope.overlay_mappings.insert((reg, reg_comp), (var, var_comp))
                }
                None => self.scalar_to_var.insert((reg, reg_comp), (var, var_comp)),
            };
        }
    }
    fn push_scope(&mut self) {
        self.scopes.push(ScopeOverlay {
            overlay_mappings: HashMap::new(),
            keys_to_clear: HashSet::new(),
        })
    }
    fn reset_scope_mappings_to_checkpoint(&mut self) {
        // Clear any intermediate mappings created inside the scope so far.
        // This is for if-else blocks - both the IF and the ELSE should start with the same mappings i.e. the mappings below this scope.
        // DON'T reset used_keys, because scalars used in only the IF and not the ELSE should still be counted as "things to reset once we're done"
        self.scopes
            .last_mut()
            .expect("Called reset_scope_to_checkpoint on a VariableScalarMap with no scopes")
            .overlay_mappings
            .clear();
    }
    fn pop_scope(&mut self) {
        // 1. pop the actual scope off
        let keys_to_clear = self.scopes.pop().expect("Called pop_scope on a VariableScalarMap with no scopes").keys_to_clear;
        // 2. clear the keys_to_clear from ALL mappings in the stack of scopes.
        //    even if we clear the keys from the next level down, ones further down might still use it and would be visible through lookup()
        for scope in self.scopes.iter_mut() {
            for k in keys_to_clear.iter() {
                scope.overlay_mappings.remove(k);
            }
        }
        for k in keys_to_clear {
            self.scalar_to_var.remove(&k);
        }
    }
}

/// This machine takes a program of any given abstract virtual machine and converts it to a different kind of HLSL-compatible program with a custom Static Single Assigment machine.
struct VariableState {
    io_declarations: Vec<MutRef<Variable>>,
    registers: Vec<MutRef<Variable>>,
    scalar_map: VariableScalarMap,
}
impl VariableState {
    fn new(io_declarations: &Vec<HLSLRegister>) -> Self {
        let mut s = VariableState { io_declarations: vec![], registers: vec![], scalar_map: VariableScalarMap::new() };
        // Initialize the io declarations and the scalar mappings for them
        // TODO have some sort of check in lookup() to make sure they aren't rebound?
        for io_decl in io_declarations {
            let var = Rc::new(RefCell::new(Variable::new(io_decl.clone(), io_decl.toplevel_kind())));
            s.io_declarations.push(var.clone());
            for i in 0..io_decl.n_components() {
                s.scalar_map.update(io_decl.clone(), i.into(), var.clone(), i.into());
            }
        }
        s
    }

    /// Given a set of input scalars, remap them to a vector of variable-space scalars which may or may not be from the same register
    /// 
    /// Will fail if 
    /// - any of the scalars are new and haven't been encountered before
    /// 
    /// Will cast scalars if they have an incompatible kind from their previous usage
    /// 
    /// Will create new input vectors if they haven't been seen before?
    /// TODO could replace this with more complete array handling...
    fn remap_scalars_to_input_vector(&mut self, v: &VectorOf<HLSLScalar>, mut kind: HLSLKind) -> Option<(Vec<MutScalarVar>, HLSLKind)> {
        let mut new_v = vec![];
        for s in v.ts.iter() {
            let new_s = match &s {
                HLSLScalar::Component(reg, reg_comp) => {
                    let (var, var_comp) = match self.scalar_map.lookup(reg.clone(), *reg_comp) {
                        Some((var, var_comp)) => (var, var_comp),
                        None => if reg.is_pure_input() {
                            let var = Rc::new(RefCell::new(Variable::new(reg.clone(), reg.toplevel_kind())));
                            self.io_declarations.push(var.clone());
                            for i in 0..reg.n_components() {
                                self.scalar_map.update(reg.clone(), i.into(), var.clone(), i.into());
                            }
                            (var, *reg_comp)
                        } else {
                            return None
                        }
                    };
                    let combined_kind = (*var).borrow_mut().refine(kind); // TODO cast if this fails?
                    match combined_kind {
                        Some(combined_kind) => kind = combined_kind,
                        None => {}
                    }
                    MutScalarVar::Component(var, var_comp)
                },
                HLSLScalar::Literal(x) => MutScalarVar::Literal(*x),
            };
            new_v.push(new_s);
        }

        Some((new_v, kind))
    }

    /// Given a set of output scalars, remap them to a vector of variable-space scalars which are from the same register
    /// 
    /// Accept vectors where
    /// - the scalars all map to a single real output O
    /// - the scalars are all general purpose, and can be remapped to normal variables
    /// 
    /// Will create a new register/variable if
    /// - any of the scalars are new, general-purpose, and haven't been encountered before
    /// - any of the scalars are general-purpose and have an incompatible kind from their previous usage
    /// - the scalars all remap to existing variable-space scalars which are not from the same register
    /// - ALWAYS because SSA makes handling scopes easier
    fn remap_scalars_to_output_vector(&mut self, v: &VectorOf<HLSLScalar>, mut kind: HLSLKind) -> (Vec<MutScalarVar>, HLSLKind) {
        let mut all_output = true;
        let mut any_output = false;
        let v: Vec<_> = v.ts.iter().map(|t| {
            if t.is_pure_input() {
                panic!("Found pure input {t:?} in a set of scalars used as an output")
            }
            if t.is_output() {
                any_output = true;
            } else {
                all_output = false;
            }

            match t {
                HLSLScalar::Component(reg, reg_comp) => (reg, *reg_comp),
                HLSLScalar::Literal(_) => unreachable!(),
            }
        }).collect();

        if all_output {
            // Find the common output *register*.
            let common_reg = find_common(v.iter(), |(reg, _)| Some((*reg).clone()));
            // If there is one, make sure it's in our output variables and create a single matching output
            // If there isn't one, fail.
            // We can assume the resulting vector is all-from-one-register, because the mappings for all outputs are initialized at the start
            // and shouldn't change.
            let new_v = match common_reg {
                Some(_) => v.into_iter().map(|(reg, reg_comp)| {
                    let (var, var_comp) = self.scalar_map.lookup(reg.clone(), reg_comp).unwrap();
                    let combined_kind = (*var).borrow_mut().refine(kind); // TODO cast if this fails?
                    match combined_kind {
                        Some(combined_kind) => kind = combined_kind,
                        None => {}
                    }
                    MutScalarVar::Component(var, var_comp)
                }).collect(),
                None => panic!("Tried to remap a group of scalars that "),
            };
            (new_v, kind)
        } else if any_output {
            panic!("Found a mix of general purpose and output scalars in a set that need to be used as a cohesive output");
        } else {
            // none of the scalars are output, and we panic if any are input, so they must all be general purpose.
            // Check for a common *generated variable*
            // let common_var = find_common(v.ts.iter(), |t| match t {
            //     HLSLScalar::Component(reg, comp) => self.scalar_map.get(&(*reg, *comp)).map(|(var, comp)| var.clone()),
            //     HLSLScalar::Literal(_) => unreachable!(),
            // });

            // If there is one, we could reuse it - BUT WE SHOULDN'T, BECAUSE IF STATEMENTS ARE A THING!
            // If there isn't one, we create a new common variable.
            // We know that this is all general purpose, so no need for checks here.
            self.remap_scalars_to_new_variable(v, kind)
        }
    }

    fn remap_scalars_to_new_variable(&mut self, v: Vec<(&HLSLRegister, VectorComponent)>, kind: HLSLKind) -> (Vec<MutScalarVar>, HLSLKind) {
        let n_contig_components = v.len();
        let name = HLSLRegister::GenericRegister(format!("{}{}", if n_contig_components == 1 { "s" } else { "v" }, self.registers.len()), n_contig_components as u8);

        let var = Rc::new(RefCell::new(Variable::new(name, kind)));
        
        let new_v = v.into_iter().enumerate().map(|(i, (reg, reg_comp))| {
            self.scalar_map.update(reg.clone(), reg_comp, var.clone(), i.into());
            MutScalarVar::Component(var.clone(), i.into())
        }).collect();

        let kind = var.borrow().kind;
        self.registers.push(var);

        (new_v, kind)
    }

    fn apply_operand_type_inference(&self, op: &HLSLOperator, output: &mut (Vec<MutScalarVar>, HLSLKind), inputs: &mut Vec<(Vec<MutScalarVar>, HLSLKind)>) {
        let kindspec = op.get_kindspec();
        // eprintln!("\n--------------------------\nKindspec for {:?}: {:?}", op, &kindspec);
        // eprintln!("Inputs: {:?}\nOutput: {:?}", inputs.iter().map(|(_, kind)| kind).collect::<Vec<_>>(), output.1);

        // The kindspec gives us either a concrete kind or a hole index for each input and output.
        let mut inferred_holes = kindspec.holes().clone();
        // First, for every input:
        // - if it corresponds to a hole, filter down the hole
        for ((_, supposed_kind), operand_kind) in inputs.iter_mut().zip(kindspec.input_types().iter()) {
            match operand_kind {
                HLSLOperandKind::Concrete(..) => {},
                HLSLOperandKind::Hole(idx) => inferred_holes[*idx] = inferred_holes[*idx].intersection(*supposed_kind).unwrap(),
            }
        }
        // Also do this for the output
        {
            let supposed_kind = &mut output.1;
        
            match kindspec.output_type() {
                HLSLOperandKind::Concrete(..) => {},
                HLSLOperandKind::Hole(idx) => inferred_holes[*idx] = inferred_holes[*idx].intersection(*supposed_kind).unwrap(),
            }
        }
        // eprintln!("After inference, holes = {:?}", &inferred_holes);
        // We now have complete holes or concrete types.
        // Apply them to the input/output.
        for ((input_vec, supposed_kind), operand_kind) in inputs.iter_mut().zip(kindspec.input_types().iter()) {
            let final_kind = match operand_kind {
                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                HLSLOperandKind::Hole(idx) => inferred_holes[*idx],
            };
            // These *should* match
            *supposed_kind = supposed_kind.intersection(final_kind).unwrap();
            for s in input_vec {
                match s {
                    MutScalarVar::Component(var, _) => {
                        // eprintln!("Refining {} with {}", var.borrow().name, supposed_kind);
                        (**var).borrow_mut().refine(*supposed_kind);
                    }
                    MutScalarVar::Literal(_) => {},
                }
            }
        }
        {
            let (output_vec, supposed_kind) = output;

            let final_kind = match kindspec.output_type() {
                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                HLSLOperandKind::Hole(idx) => inferred_holes[*idx],
            };
            // These *should* match
            *supposed_kind = supposed_kind.intersection(final_kind).unwrap();
            for s in output_vec {
                match s {
                    MutScalarVar::Component(var, _) => {
                        // eprintln!("Refining {} with {}", var.borrow().name, supposed_kind);
                        (**var).borrow_mut().refine(*supposed_kind);
                    }
                    MutScalarVar::Literal(_) => {},
                }
            }
        }
    }

    fn process_action(&mut self, action: &HLSLAction) -> MutVarAction {
        match action {
            Action::Assign { output, op, inputs } => {
                // Extra type inference is useful at this stage!
                // Consider an AMDIL machine which produces a program with two actions:
                // - r0.x = div v0.x, v0.y
                // - r1.x = v1.z ? r0.x : 0x0
                // because r0.x is the output of a div, the AMDIL machine has enough information to state r0.x is a Float
                // but the AMDIL machine doesn't keep that information around - generally, the variable machine is expected to propagate the type information
                // the AMDIL machine sees the ternary and all it knows is "well, r0.x and 0x0 must be the same type. I don't know what type it is, because ternarys work on anything."
                // The variable machine is the only place that understands "hey, r0.x (maps to a variable which) is a float! Which means the 0x0 must also be a float! and r1.x (maps to a variable which) must be a float!"

                let mut inputs: Vec<_> = inputs.into_iter().map(|(in_vec, kind)| {
                    self.remap_scalars_to_input_vector(in_vec, *kind).expect("Tried to make an input-vector with unencountered original scalars")
                }).collect();
                let mut output = self.remap_scalars_to_output_vector(&output.0, output.1);

                // We should do the inference here, because this is where we have the most information.
                // We have attached the inputs to their previous variables, which may have more type information than before.
                self.apply_operand_type_inference(op, &mut output, &mut inputs);

                MutVarAction::Assign { output, op: *op, inputs }
            },
            Action::EarlyOut => MutVarAction::EarlyOut,
            Action::If { inputs, cond_operator, if_true, if_fals } => {
                // Sketch:
                // 1. translate immediate input
                let inputs = inputs.iter().map(|(scalar, kind)| {
                    let remap_scalar = match scalar {
                        HLSLScalar::Component(reg, reg_comp) => {
                            let (var, var_comp) = self.scalar_map.lookup(reg.clone(), *reg_comp).expect("Tried to use unencountered original scalars in an IF");
                            MutScalarVar::Component(var, var_comp)
                        }
                        HLSLScalar::Literal(lit) => MutScalarVar::Literal(*lit),
                    };
                    (remap_scalar, *kind)
                }).collect();
                // 2. make a checkpoint for the scalar mapping
                self.scalar_map.push_scope();
                // 3. translate the IF block
                // 3a. capture the set of scalars written over by the IF block
                let if_true = if_true.iter().map(|a| self.process_action(a)).collect();
                // 4. restore the scalar mapping
                    // the if and the else should have the same starting point
                self.scalar_map.reset_scope_mappings_to_checkpoint();
                // 5. translate the else statement
                // 5a. capture the set of scalars written over by the ELSE block
                let if_fals = if_fals.iter().map(|a| self.process_action(a)).collect();
                // 6. delete all scalar mappings written over by the IF and the ELSE (THAT AREN'T OUTPUTS). Don't restore them, because any code after this point can't rely on values that may-or-may-not have been written.
                    // NOTE this assumes that IF-ELSE will never write to the same register in both blocks with the intention of reading that value afterwards (a rust-style let y = if x { 1 } else { 2 };)
                    // If this assumption is ever faulty, we have a big fuckin problem.
                self.scalar_map.pop_scope();
                MutVarAction::If { inputs, cond_operator: *cond_operator, if_true, if_fals }
            }
        }
    }

    fn finalize_actions(&self, actions: &mut Vec<MutVarAction>, ) {
        // Make kinds consistent, so the backwards type propagation propagates correct types
        for a in actions.iter_mut() {
            match a {
                Action::Assign { output, inputs, .. } => {
                    output.1 = Self::get_consistent_kind(&output.0, output.1);
                    for (var_vec, vec_kind) in inputs {
                        *vec_kind = Self::get_consistent_kind(var_vec, *vec_kind);
                    }
                }
                _ => {}
            }
        }

        // Do backwards type propagation.

        // Consider a program `A = Literal(0x0); B = A + 1.0f;`
        // `B` must have a 'kind' of f32, and the HLSL addition operator thus forces A to be of the same kind f32.
        // This then implies that the literal must be f32, but currently this machine doesn't track that metadata or re-apply constraints once forward analysis has completed.
        // This will be most visible for literals - the AMDIL machine used to handle named literals by creating a new variable and assigning a constant value to it.
        // The kind of the constant values can only be affected by how the new variable is used, but without back-propagation of the kind it will never come out.
         
        for a in actions.iter_mut().rev() {
            match a {
                Action::Assign { output, op, inputs } => self.apply_operand_type_inference(op, output, inputs),
                Action::EarlyOut => {},
                Action::If { if_true, if_fals, .. } => {
                    self.finalize_actions(if_fals);
                    self.finalize_actions(if_true);
                },
            }
        }

        // Make kinds consistent... again! Because the backwards type inference could have invalidated them again :D
        for a in actions.iter_mut() {
            match a {
                // Try to give hints at concretization if applicable
                // This is a hack! We're assuming this only ever happens at the last possible moment,
                // once kinds are already consistent (so if an output uses NUMERIC mask, the inputs must all use NUMERIC mask)
                Action::Assign { output, inputs, .. } => {
                    output.1 = match Self::get_consistent_kind(&output.0, output.1).mask() {
                        HLSLKindBitmask::NUMERIC => HLSLKindBitmask::NUMERIC_FLOAT.into(),
                        HLSLKindBitmask::INTEGER => HLSLKindBitmask::NUMERIC_UINT.into(),
                        mask => mask.into(),
                    };
                    for (var_vec, vec_kind) in inputs {
                        *vec_kind = match Self::get_consistent_kind(var_vec, *vec_kind).mask() {
                            HLSLKindBitmask::NUMERIC => HLSLKindBitmask::NUMERIC_FLOAT.into(),
                            HLSLKindBitmask::INTEGER => HLSLKindBitmask::NUMERIC_UINT.into(),
                            mask => mask.into(),
                        };
                    }
                }
                _ => {}
            }
        }
    }

    fn get_consistent_kind(var_vec: &Vec<MutScalarVar>, supposed_kind: HLSLKind) -> HLSLKind {
        // If the constituent variables referenced in var_vec were updated after-the-fact through type inference,
        // they may be out-of-date with the supposed_kind attached to the variable initially.
        // NOTE this may be difficult to resolve if two variables referenced in the same input vector (which thus initially have the same kind masks)
        // end up diverging and being of different concrete types.
        // This would require a bit cast when their elements are used in the input vector.
        // This cannot happen for outputs because they are always from the same variable. 
        var_vec.iter().fold(supposed_kind, |prev, s| {
            match s {
                MutScalarVar::Component(var, _) => match prev.intersection((**var).borrow_mut().kind){
                    Some(kind) => kind,
                    None => panic!("A variable was updated by type-inference and made incompatible with some of its previous usages?"),
                },
                MutScalarVar::Literal(_) => prev.intersection(HLSLKindBitmask::NUMERIC.into()).expect("supposed_kind was not numeric despite applying to a literal"),
            }
        })
    }

    fn resolve_action(&self, action: &MutVarAction) -> HLSLAction {
        match action {
            Action::Assign { output, op, inputs } => {
                let output = (
                    VectorOf::new(&output.0.iter().map(|s| self.resolve(s)).collect::<Vec<_>>()).unwrap(),
                    output.1,
                );
                let inputs = inputs.iter().map(|(var_vec, vec_kind)| {
                    (
                        VectorOf::new(&var_vec.iter().map(|s| self.resolve(s)).collect::<Vec<_>>()).unwrap(),
                        *vec_kind,
                    )
                }).collect();
                Action::Assign { output, op: *op, inputs }
            },
            Action::EarlyOut => Action::EarlyOut,
            Action::If { inputs, cond_operator, if_true, if_fals } => {
                let inputs = inputs.iter().map(|(s, kind)| (self.resolve(s), *kind)).collect();
                let if_true = if_true.iter().map(|a| self.resolve_action(a)).collect();
                let if_fals = if_fals.iter().map(|a| self.resolve_action(a)).collect();
                Action::If { inputs, cond_operator: *cond_operator, if_true, if_fals }
            },
        }
    }

    fn resolve(&self, mut_var: &MutScalarVar) -> HLSLScalar {
        // TODO how do we communicate more restricted type information to the outside world?
        // HLSLRegister doesn't have a HLSLKind
        match mut_var {
            MutScalarVar::Component(var, var_comp) => HLSLScalar::Component(var.borrow().name.clone(), *var_comp),
            MutScalarVar::Literal(x) => HLSLScalar::Literal(*x),
        }
    }
}

// struct 
pub fn disassemble<P: Program<HLSLAbstractVM>>(p: &P) -> HLSLCompatProgram {
    let mut variables = VariableState::new(p.io_declarations());

    // Run all the actions forwards through the machine
    let mut var_actions: Vec<_> = p.actions().iter().map(|a| variables.process_action(a)).collect();
    assert!(variables.scalar_map.scopes.is_empty());

    // Do another pass to do backwards type propagation and ensure the consistency of (var.kind, supposed_kind) - var.kind can be updated under our feet, supposed_kind needs to be made up-to-date
    variables.finalize_actions(&mut var_actions);

    HLSLCompatProgram {
        actions: var_actions.into_iter().map(|a| variables.resolve_action(&a)).collect(),
        io_registers: variables.io_declarations.iter().map(|var| var.borrow().name.clone()).collect(),
    }
}
