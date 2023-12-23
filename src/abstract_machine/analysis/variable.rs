use std::{cell::RefCell, rc::Rc, collections::{HashMap, HashSet}};

use crate::{hlsl::{kinds::HLSLKind, compat::HLSLCompatibleAbstractVM, HLSLRegister, vm::HLSLAbstractVM, HLSLScalar, HLSLAction}, abstract_machine::{vector::{VectorComponent, VectorOf}, VMName, VMVector, VMScalar}, AbstractVM, Program, Action};


type MutRef<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Variable {
    name: HLSLRegister,
    n_contig_components: usize,
    kind: HLSLKind,
}
impl Variable {
    fn new(name: HLSLRegister, kind: HLSLKind) -> Self {
        Self {
            n_contig_components: name.n_components(),
            name,
            kind,
        }
    }

    // Refining can only happen inside here, because this machine doesn't use <v as Variable>.toplevel_kind().
    // toplevel_kind() is necessary for other analyses to use
    fn refine(&mut self, hlsl_kind: HLSLKind) -> Option<()> {
        self.kind = self.kind.intersection(hlsl_kind)?;
        Some(())
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

/// TODO backwards propagation of kind
/// Consider a program `A = X + Y; B = A + 1.0f;`
/// `B` must have a 'kind' of f32, and the HLSL addition operator thus forces A to be of the same kind f32.
/// This then implies that X and Y must be of f32, but currently this machine doesn't track that metadata or re-apply constraints once forward analysis has completed.
/// This will be most visible for literals - the AMDIL machine used to handle named literals by creating a new variable and assigning a constant value to it.
/// The kind of the constant values can only be affected by how the new variable is used, but without back-propagation of the kind it will never come out.

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
    fn remap_scalars_to_input_vector(&mut self, v: &VectorOf<HLSLScalar>, kind: HLSLKind) -> Option<(Vec<MutScalarVar>, HLSLKind)> {
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
                    var.borrow_mut().refine(kind); // TODO cast if this fails?
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
    fn remap_scalars_to_output_vector(&mut self, v: &VectorOf<HLSLScalar>, kind: HLSLKind) -> (Vec<MutScalarVar>, HLSLKind) {
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
                    var.borrow_mut().refine(kind); // TODO cast if this fails?
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
        let name = HLSLRegister::GenericRegister(format!("{}_{:03}", if n_contig_components == 1 { "s" } else { "v" }, self.registers.len()), n_contig_components as u8);

        let var = Rc::new(RefCell::new(Variable::new(name, kind)));
        
        let new_v = v.into_iter().enumerate().map(|(i, (reg, reg_comp))| {
            self.scalar_map.update(reg.clone(), reg_comp, var.clone(), i.into());
            MutScalarVar::Component(var.clone(), i.into())
        }).collect();

        self.registers.push(var);

        (new_v, kind)
    }

    fn process_action(&mut self, action: &HLSLAction) -> MutVarAction {
        match action {
            Action::Assign { output, op, inputs } => {
                let inputs = inputs.into_iter().map(|(in_vec, kind)| {
                    self.remap_scalars_to_input_vector(in_vec, *kind).expect("Tried to make an input-vector with unencountered original scalars")
                }).collect();
                let output = self.remap_scalars_to_output_vector(&output.0, output.1);
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

    fn resolve_action(&self, action: &MutVarAction) -> HLSLAction {
        match action {
            Action::Assign { output, op, inputs } => {
                let output = (
                    VectorOf::new(&output.0.iter().map(|s| self.resolve(s)).collect::<Vec<_>>()).unwrap(),
                    output.1
                );
                let inputs = inputs.iter().map(|(var_vec, vec_kind)| {
                    (
                        VectorOf::new(&var_vec.iter().map(|s| self.resolve(s)).collect::<Vec<_>>()).unwrap(),
                        *vec_kind
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
pub fn disassemble<P: Program<HLSLAbstractVM>>(p: &P) -> Vec<HLSLAction> {
    let mut variables = VariableState::new(p.io_declarations());

    // Run all the actions through the machine
    let var_actions: Vec<_> = p.actions().iter().map(|a| variables.process_action(a)).collect();
    assert!(variables.scalar_map.scopes.is_empty());

    var_actions.into_iter().map(|a| variables.resolve_action(&a)).collect()
}
