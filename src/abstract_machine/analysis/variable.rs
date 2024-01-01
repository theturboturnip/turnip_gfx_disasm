use std::{cell::RefCell, rc::Rc, collections::{HashMap, HashSet}};

use crate::{hlsl::{kinds::{HLSLKind, HLSLOperandKind, HLSLKindBitmask, KindRefinementResult}, compat::HLSLCompatProgram, HLSLRegister, vm::HLSLAbstractVM, HLSLAction, syntax::{Operator, HLSLOperator}}, abstract_machine::{vector::VectorComponent, VMName, VMVector, VMScalar, expr::{Scalar, Vector, HLSLVector, ContigSwizzle, Reg, HLSLScalar}}, Program, Action};


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
impl Reg for MutRef<Variable> {
    fn output_kind(&self) -> HLSLKind {
        self.borrow().toplevel_kind()
    }
    fn refine_output_kind_if_possible(&mut self, constraint: HLSLKind) -> Option<KindRefinementResult> {
        Some(self.borrow_mut().kind.refine_if_possible(constraint))
    }
}

type MutVarScalar = Scalar<MutRef<Variable>>;

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
type MutVarAction = Action<MutRef<Variable>>;

// #[derive(Debug, Clone)]
// enum VariableVM {}
// impl AbstractVM for VariableVM {
//     type Register = Variable;
// }
// // impl HLSLCompatibleAbstractVM for VariableVM {

// // }

type ScalarKey = (HLSLRegister, VectorComponent);

#[derive(Debug, Clone)]
enum VariableComp {
    Simple(MutRef<Variable>, VectorComponent),
    Complex(MutVarScalar),
}
impl From<VariableComp> for MutVarScalar {
    fn from(value: VariableComp) -> Self {
        match value {
            VariableComp::Simple(var, comp) => MutVarScalar::Component(var, comp),
            VariableComp::Complex(s) => s,
        }
    }
}

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
    fn lookup(&self, reg: HLSLRegister, reg_comp: VectorComponent) -> Option<MutVarScalar> {
        let key = (reg, reg_comp);
        for scope in self.scopes.iter().rev() {
            match scope.overlay_mappings.get(&key) {
                Some(s) => return Some(s.clone().into()),
                None => continue,
            }
        }
        self.scalar_to_var.get(&key).map(|s| s.clone().into())
    }
    fn update(&mut self, reg: HLSLRegister, reg_comp: VectorComponent, var: MutRef<Variable>, var_comp: VectorComponent) {
        if reg.is_pure_input() {
            self.scalar_to_var.insert((reg, reg_comp), VariableComp::Simple(var, var_comp));
        } else {
            match self.scopes.last_mut() {
                Some(scope) => {
                    if reg.is_output() {
                        panic!("Shouldn't be creating input/output mappings inside a scope!")
                    }
                    scope.keys_to_clear.insert((reg.clone(), reg_comp));
                    scope.overlay_mappings.insert((reg, reg_comp), VariableComp::Simple(var, var_comp))
                }
                None => self.scalar_to_var.insert((reg, reg_comp), VariableComp::Simple(var, var_comp)),
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
    io_declarations: HashMap<HLSLRegister, MutRef<Variable>>,
    registers: Vec<MutRef<Variable>>,
    scalar_map: VariableScalarMap,
}
impl VariableState {
    fn new(io_declarations: &Vec<HLSLRegister>) -> Self {
        let mut s = VariableState { io_declarations: HashMap::new(), registers: vec![], scalar_map: VariableScalarMap::new() };
        // Initialize the io declarations and the scalar mappings for them
        // TODO have some sort of check in lookup() to make sure they aren't rebound?
        for io_decl in io_declarations {
            let var = Rc::new(RefCell::new(Variable::new(io_decl.clone(), io_decl.toplevel_kind())));
            s.io_declarations.insert(io_decl.clone(), var.clone());
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
    fn remap_input_expr_to_use_variables(&mut self, v: &HLSLVector) -> Vector<MutRef<Variable>> {
        let mut new_v = v.map_scalar(&mut |reg, reg_comp, usage_kind| {
            match self.scalar_map.lookup(reg.clone(), reg_comp) {
                Some(s) => s,
                None => if reg.is_pure_input() {
                    let mut var = Rc::new(RefCell::new(Variable::new(reg.clone(), reg.toplevel_kind())));
                    self.io_declarations.insert(reg.clone(), var.clone());
                    for i in 0..reg.n_components() {
                        self.scalar_map.update(reg.clone(), i.into(), var.clone(), i.into());
                    }
                    var.refine_output_kind_if_possible(usage_kind); // TODO is this necessary? see below
                    Scalar::Component(var, reg_comp)
                } else {
                    panic!("Encountered unknown non-input scalar {:?}.{} in a operation input", reg, reg_comp)
                }
            }
        });

        new_v
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
    fn remap_scalars_to_output_vector(&mut self, reg: &HLSLRegister, contig: ContigSwizzle, kind: &mut HLSLKind) -> (MutRef<Variable>, ContigSwizzle) {
        if reg.is_pure_input() {
            panic!("pure input register {reg:?} used as an output")
        } else if reg.is_output() {
            // Find the associated output variable. There will only be one.
            let var = self.io_declarations.get(reg).expect("Encountered undeclared output");
            *kind = var.borrow_mut().refine(*kind).unwrap(); // TODO is this necessary
            (var.clone(), contig)
        } else {
            // the register is neither output nor input, so it must be general purpose
            
            // Check for a common *generated variable*
            // let common_var = find_common(v.ts.iter(), |t| match t {
            //     HLSLScalar::Component(reg, comp) => self.scalar_map.get(&(*reg, *comp)).map(|(var, comp)| var.clone()),
            //     HLSLScalar::Literal(_) => unreachable!(),
            // });

            // If there is one, we could reuse it - BUT WE SHOULDN'T, BECAUSE IF STATEMENTS ARE A THING!
            // If there isn't one, we create a new common variable.
            // We know that this is all general purpose, so no need for checks here.
            self.remap_scalars_to_new_variable(reg, contig, *kind)
        }
    }

    fn remap_scalars_to_new_variable(&mut self, reg: &HLSLRegister, contig: ContigSwizzle, kind: HLSLKind) -> (MutRef<Variable>, ContigSwizzle) {
        let n_contig_components = contig.len();
        let name = HLSLRegister::GenericRegister(format!("{}{}", if n_contig_components == 1 { "s" } else { "v" }, self.registers.len()), n_contig_components as u8);

        let var = Rc::new(RefCell::new(Variable::new(name, kind)));
        
        let new_swizzle: ContigSwizzle = contig.into_iter().enumerate().map(|(i, reg_comp)| {
            self.scalar_map.update(reg.clone(), reg_comp, var.clone(), i.into());
            i.into()
        }).collect();

        self.registers.push(var.clone());

        (var, new_swizzle)
    }

    /*
    fn apply_operand_type_inference(&self, op: &HLSLOperator, output_var: &MutRef<Variable>, input_expr: &mut Vector<MutRef<Variable>>) {
        let output_kind = match input_expr {
            Vector::Construction(scalars, usage_kind) => {
                *usage_kind = usage_kind.intersection(*final_kind).unwrap();
                for s in scalars {
                    match s {
                        Scalar::Literal(_, scalar_usage_kind) => {
                            *scalar_usage_kind = scalar_usage_kind.intersection(*usage_kind).unwrap();
                        }
                        Scalar::Component(var, comp, scalar_usage_kind) => {
                            *scalar_usage_kind = scalar_usage_kind.intersection(*usage_kind).unwrap();
                            (*var).borrow_mut().refine(*usage_kind);
                        }
                        Scalar::Expr { op, inputs, output_kind } => {
                            *output_kind = output_kind.intersection(*usage_kind).unwrap();
                            self.apply_scalar_expr_type_inference(op, output_kind, inputs)
                        }
                    }
                }
            },
            Vector::PureSwizzle(_, _, _) => todo!(),
            // Note: output_kind here refers to the output of nested expression constituting an input to the expression we're currently processing,
            // NOT the output of the top-l
            Vector::PerCompExpr { op, inputs, output_kind, .. } | 
            Vector::AllToAllExpr { op, inputs, output_kind } => {
                *output_kind = output_kind.intersection(*final_kind).unwrap();
                *final_kind = *output_kind;
                self.apply_vector_expr_type_inference(op, output_kind, inputs)
            }
        }
        (*output_var).borrow_mut().refine(*final_kind);
    }

    fn apply_scalar_expr_type_inference(&self, op: &HLSLOperator, output_kind: &mut HLSLKind, inputs: &mut Vec<Scalar<MutRef<Variable>>>) {
        let mut kindspec = op.get_kindspec();
        // The kindspec gives us either a concrete kind or a hole index for each input and output.
        // Apply constraints to the holes in the kindspec
        kindspec.apply_input_constraints(inputs.iter().map(|s| s.usage_kind()));
        kindspec.apply_output_constraint(*output_kind);

        todo!()
    }

    fn apply_vector_expr_type_inference(&self, op: &HLSLOperator, output_kind: &mut HLSLKind, inputs: &mut Vec<Vector<MutRef<Variable>>>) {
        let mut kindspec = op.get_kindspec();
        // The kindspec gives us either a concrete kind or a hole index for each input and output.
        // Apply constraints to the holes in the kindspec
        kindspec.apply_input_constraints(inputs.iter().map(|v| v.usage_kind()));
        kindspec.apply_output_constraint(*output_kind);

        // eprintln!("\n--------------------------\nKindspec for {:?}: {:?}", op, &kindspec);
        // eprintln!("Inputs: {:?}\nOutput: {:?}", inputs.iter().map(|(_, kind)| kind).collect::<Vec<_>>(), output.1);

        // eprintln!("After inference, holes = {:?}", &inferred_holes);
        // We now have complete holes or concrete types.
        // Apply them to the input/output.
        let holes = kindspec.holes();
        for (input_vec, operand_kind) in inputs.iter_mut().zip(kindspec.input_types().iter()) {
            let final_kind = match operand_kind {
                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                HLSLOperandKind::Hole(idx) => holes[*idx],
            };
            match input_vec {
                Vector::Construction(scalars, usage_kind) => {
                    *usage_kind = usage_kind.intersection(final_kind).unwrap();
                    for s in scalars {
                        match s {
                            Scalar::Literal(_, scalar_usage_kind) => {
                                *scalar_usage_kind = scalar_usage_kind.intersection(*usage_kind).unwrap();
                            }
                            Scalar::Component(var, comp, scalar_usage_kind) => {
                                *scalar_usage_kind = scalar_usage_kind.intersection(*usage_kind).unwrap();
                                (*var).borrow_mut().refine(*usage_kind);
                            }
                            Scalar::Expr { op, inputs, output_kind } => {
                                *output_kind = output_kind.intersection(*usage_kind).unwrap();
                                self.apply_scalar_expr_type_inference(op, output_kind, inputs)
                            }
                        }
                    }
                },
                Vector::PureSwizzle(_, _, _) => todo!(),
                // Note: output_kind here refers to the output of nested expression constituting an input to the expression we're currently processing,
                // NOT the output of the top-l
                Vector::PerCompExpr { op, inputs, output_kind, .. } | 
                Vector::AllToAllExpr { op, inputs, output_kind } => {
                    *output_kind = output_kind.intersection(final_kind).unwrap();
                    self.apply_vector_expr_type_inference(op, output_kind, inputs)
                }
            }
        }
        {
            let final_kind = match kindspec.output_type() {
                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                HLSLOperandKind::Hole(idx) => holes[*idx],
            };
            // These *should* match
            *output_kind = output_kind.intersection(final_kind).unwrap();
        }
    }
    */

    fn process_action(&mut self, action: &HLSLAction) -> MutVarAction {
        match action {
            Action::Assign { output, expr } => {
                // Extra type inference is useful at this stage!
                // Consider an AMDIL machine which produces a program with two actions:
                // - r0.x = div v0.x, v0.y
                // - r1.x = v1.z ? r0.x : 0x0
                // because r0.x is the output of a div, the AMDIL machine has enough information to state r0.x is a Float
                // but the AMDIL machine doesn't keep that information around - generally, the variable machine is expected to propagate the type information
                // the AMDIL machine sees the ternary and all it knows is "well, r0.x and 0x0 must be the same type. I don't know what type it is, because ternarys work on anything."
                // The variable machine is the only place that understands "hey, r0.x (maps to a variable which) is a float! Which means the 0x0 must also be a float! and r1.x (maps to a variable which) must be a float!"

                // Rewrite the input expression to reference variables instead of registers.
                let mut expr = self.remap_input_expr_to_use_variables(expr);
                let mut output_kind = expr.output_kind();
                let mut output = self.remap_scalars_to_output_vector(
                    &output.0,
                    output.1.clone(),
                    &mut output_kind,
                );
                // reapply output_kind to input expr, in case creating the output vector gave us more information
                expr.refine_output_kind_from_usage(output_kind);

                MutVarAction::Assign { output: (output.0, output.1), expr }
            },
            Action::EarlyOut => MutVarAction::EarlyOut,
            Action::If { expr, if_true, if_fals } => {
                // Sketch:
                // 1. translate immediate input
                let expr = expr.map_scalar(&mut |reg, reg_comp, usage| {
                    let mut s = self.scalar_map.lookup(reg.clone(), reg_comp).expect("Tried to use unencountered original scalars in an IF");
                    s.refine_output_kind_from_usage(usage);
                    s
                }, HLSLKind::INTEGER);
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
                    // WOOT this assumption was faulty. Need some way to merge variables.
                self.scalar_map.pop_scope();
                MutVarAction::If { expr, if_true, if_fals }
            }
        }
    }

    fn finalize_actions(&self, actions: &mut Vec<MutVarAction>, ) {
        // // Make kinds consistent, so the backwards type propagation propagates correct types
        // // This function recurses on IFs so we only need to handle Action::Assign here
        // for a in actions.iter_mut() {
        //     match a {
        //         Action::Assign { output, expr, .. } => {
        //             expr.recompute_output_kind_from_internal_output_kinds();
        //         }
        //         _ => {}
        //     }
        // }

        // Do backwards type propagation.

        // Consider a program `A = Literal(0x0); B = A + 1.0f;`
        // `B` must have a 'kind' of f32, and the HLSL addition operator thus forces A to be of the same kind f32.
        // This then implies that the literal must be f32, but currently this machine doesn't track that metadata or re-apply constraints once forward analysis has completed.
        // This will be most visible for literals - the AMDIL machine used to handle named literals by creating a new variable and assigning a constant value to it.
        // The kind of the constant values can only be affected by how the new variable is used, but without back-propagation of the kind it will never come out.
         
        for a in actions.iter_mut().rev() {
            match a {
                Action::Assign { output, expr } => {
                    // 1. update the expression's output kind from any variables it uses that changed
                    // 2. refine the kind of the output variable, based on the expression's output
                    // 3. refine the output kind of the expression based on the kind of the output variable!!
                    //      step 1 doesn't cover this because it only does a "bottom-up" inference from it's base inputs up through subexpressions to its output_kind.
                    //      this does a "top-down" inference based on what the output must be
                    match expr.recompute_output_kind_from_internal_output_kinds(true) {
                        Some(KindRefinementResult::RefinedTo(new_output_kind)) => {
                            output.0.refine_output_kind_if_possible(new_output_kind);
                        }
                        _ => {}
                    }
                    expr.refine_output_kind_from_usage(output.0.output_kind())
                },
                Action::EarlyOut => {},
                Action::If { if_true, if_fals, .. } => {
                    self.finalize_actions(if_fals);
                    self.finalize_actions(if_true);
                },
            }
        }

        // // Make kinds consistent... again! Because the backwards type inference could have invalidated them again :D
        // for a in actions.iter_mut() {
        //     match a {
        //         // Try to give hints at concretization if applicable
        //         // This is a hack! We're assuming this only ever happens at the last possible moment,
        //         // once kinds are already consistent (so if an output uses NUMERIC mask, the inputs must all use NUMERIC mask)
        //         Action::Assign { output, inputs, .. } => {
        //             output.1 = match Self::get_consistent_kind(&output.0, output.1).mask() {
        //                 HLSLKindBitmask::NUMERIC => HLSLKind::NUMERIC_FLOAT,
        //                 HLSLKindBitmask::INTEGER => HLSLKind::NUMERIC_UINT,
        //                 mask => mask.into(),
        //             };
        //             for (var_vec, vec_kind) in inputs {
        //                 *vec_kind = match Self::get_consistent_kind(var_vec, *vec_kind).mask() {
        //                     HLSLKindBitmask::NUMERIC => HLSLKind::NUMERIC_FLOAT,
        //                     HLSLKindBitmask::INTEGER => HLSLKind::NUMERIC_UINT,
        //                     mask => mask.into(),
        //                 };
        //             }
        //         }
        //         _ => {}
        //     }
        // }
    }

    fn get_consistent_kind(var_vec: &Vec<MutVarScalar>, supposed_kind: HLSLKind) -> HLSLKind {
        // If the constituent variables referenced in var_vec were updated after-the-fact through type inference,
        // they may be out-of-date with the supposed_kind attached to the variable initially.
        // NOTE this may be difficult to resolve if two variables referenced in the same input vector (which thus initially have the same kind masks)
        // end up diverging and being of different concrete types.
        // This would require a bit cast when their elements are used in the input vector.
        // This cannot happen for outputs because they are always from the same variable. 
        var_vec.iter().fold(supposed_kind, |prev, s| {
            prev.intersection(s.output_kind()).unwrap()
        })
    }

    fn resolve_action(&self, action: &MutVarAction) -> HLSLAction {
        match action {
            Action::Assign { output, expr } => {
                let output = (output.0.borrow().name.clone(), output.1.clone());
                let expr = expr.map_scalar(&mut |var, comp, _| {
                    self.resolve(var, comp)
                });
                Action::Assign { output, expr }
            },
            Action::EarlyOut => Action::EarlyOut,
            Action::If { expr, if_true, if_fals } => {
                let expr = expr.map_scalar(&mut |var, comp, _| {
                    self.resolve(var, comp)
                }, HLSLKind::ALL);
                let if_true = if_true.iter().map(|a| self.resolve_action(a)).collect();
                let if_fals = if_fals.iter().map(|a| self.resolve_action(a)).collect();
                Action::If { expr, if_true, if_fals }
            },
        }
    }

    fn resolve(&self, var: &MutRef<Variable>, var_comp: VectorComponent) -> HLSLScalar {
        // TODO how do we communicate more restricted type information to the outside world?
        // HLSLRegister doesn't have a HLSLKind
        Scalar::Component(var.borrow().name.clone(), var_comp)
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
        io_registers: variables.io_declarations.values().map(|var| var.borrow().name.clone()).collect(),
    }
}
