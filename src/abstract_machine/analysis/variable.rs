use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{hlsl::{kinds::HLSLKind, compat::HLSLCompatibleAbstractVM, HLSLRegister, vm::HLSLAbstractVM, HLSLScalar}, abstract_machine::{vector::{VectorComponent, VectorOf}, VMName, VMVector, VMScalar}, AbstractVM, Program, Action};


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

struct VariableScalarMap(HashMap<(HLSLRegister, VectorComponent), (MutRef<Variable>, VectorComponent)>);
impl VariableScalarMap {
    fn new() -> Self {
        Self(HashMap::new())
    }
    fn lookup(&self, reg: HLSLRegister, reg_comp: VectorComponent) -> Option<(MutRef<Variable>, VectorComponent)> {
        self.0.get(&(reg, reg_comp)).map(|(var, c)| (var.clone(), *c))
    }
    fn update(&mut self, reg: HLSLRegister, reg_comp: VectorComponent, var: MutRef<Variable>, var_comp: VectorComponent) {
        self.0.insert((reg, reg_comp), (var, var_comp));
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
    fn remap_scalars_to_input_vector(&self, v: &VectorOf<HLSLScalar>, kind: HLSLKind) -> Option<(Vec<MutScalarVar>, HLSLKind)> {
        let mut new_v = vec![];
        for s in v.ts.iter() {
            let new_s = match &s {
                HLSLScalar::Component(reg, reg_comp) => {
                    let (var, var_comp) = self.scalar_map.lookup(reg.clone(), *reg_comp)?;
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

        (new_v, kind)
    }

    /*fn process_action<TVM: HLSLCompatibleAbstractVM>(&mut self, action: &Action<TVM>) -> VecAction<Vec<MutScalarVar>> {
        match action {
            VMAction::Assign { output, op, inputs } => {
                let inputs = inputs.into_iter().map(|(in_vec, kind)| {
                    self.remap_scalars_to_input_vector(in_vec, *kind).unwrap()
                }).collect();
                let output = self.remap_scalars_to_output_vector(&output.0, output.1);
                VecAction::Assign { output, op: *op, inputs }
            },
            VMAction::EarlyOut => VecAction::EarlyOut,
            VMAction::If { inputs, cond_operator, if_true, if_fals } => {
                
            }
        };
    }*/
}

// struct 
fn disassemble<P: Program<HLSLAbstractVM>>(p: &P) {
    let mut variables = VariableState::new(p.io_declarations());

    // let mut var_actions = vec![];
    // for action in p.actions() {
    //     // let var_action = 
    //     // var_actions.push(var_action);
    // }
    // todo!()
}
