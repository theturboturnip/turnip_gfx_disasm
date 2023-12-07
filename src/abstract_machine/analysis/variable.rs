use std::{cell::RefCell, rc::Rc, collections::HashMap};

use crate::{hlsl::{kinds::HLSLKind, compat::HLSLCompatibleAbstractVM, HLSLRegister}, abstract_machine::{vector::{VectorComponent, VectorOf}, VMName, VMVector, VMScalar, SimpleAction}, AbstractVM};


type MutRef<T> = Rc<RefCell<T>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Variable {
    name: HLSLRegister,
    n_contig_components: usize,
    kind: HLSLKind,
}
impl Variable {
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

struct MutScalarVar {
    vec: MutRef<Variable>,
    comp: VectorComponent,
    cast_to: Option<HLSLKind>,
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
struct VariableVM {}
impl AbstractVM for VariableVM {
    type Action = SimpleAction<VariableVM>;

    type Scalar = ScalarVar;

    type Register = Variable;

    type Vector = VectorOf<ScalarVar>;

    fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
        v.ts.clone()
    }
}
impl HLSLCompatibleAbstractVM for VariableVM {

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct InputScalar {
    input_register: HLSLRegister,
    comp: VectorComponent
}

/// TODO backwards propagation of kind
/// Consider a program `A = X + Y; B = A + 1.0f;`
/// `B` must have a 'kind' of f32, and the HLSL addition operator thus forces A to be of the same kind f32.
/// This then implies that X and Y must be of f32, but currently this machine doesn't track that metadata or re-apply constraints once forward analysis has completed.
/// This will be most visible for literals - the AMDIL machine handles named literals by creating a new variable and assigning a constant value to it.
/// The kind of the constant values can only be affected by how the new variable is used, but without back-propagation of the kind it will never come out.

/// This machine takes a program of any given abstract virtual machine and converts it to a different kind of HLSL-compatible program with a custom Static Single Assigment machine.
struct VariableState {
    registers: Vec<MutRef<Variable>>,
    scalar_map: HashMap<InputScalar, (MutRef<Variable>, VectorComponent)>,
}
impl VariableState {
    /// Given a set of input scalars, remap them to a vector of variable-space scalars which may or may not be from the same register
    /// 
    /// Will fail if 
    /// - any of the scalars are new and haven't been encountered before
    /// 
    /// Will cast scalars if they have an incompatible kind from their previous usage
    fn remap_scalars_to_input_vector(&mut self, v: Vec<(InputScalar, HLSLKind)>) -> Option<Vec<MutScalarVar>> {
        let mut new_v = vec![];
        for s in v.into_iter() {
            let (vec, comp) = self.scalar_map.get(&s.0)?;
            // Try to refine the original vector's kind with the current usage.
            // If that fails, assume this is a cast
            let new_s = match vec.borrow_mut().refine(s.1) {
                Some(valid_common_kind) => {
                    MutScalarVar {
                        vec: vec.clone(),
                        comp: *comp,
                        cast_to: None,
                    }
                },
                None => {
                    // No common kind found. Cast the scalar to its usage
                    MutScalarVar {
                        vec: vec.clone(),
                        comp: *comp,
                        cast_to: Some(s.1),
                    }
                }
            };
            new_v.push(new_s);
        }

        Some(new_v)
    }

    /// Given a set of input scalars, remap them to a vector of variable-space scalars which are from the same register
    /// 
    /// Will create a new register/variable if
    /// - any of the scalars are new and haven't been encountered before
    /// - any of the scalars have an incompatible kind from their previous usage
    /// - the scalars all remap to existing variable-space scalars which are not from the same register
    fn remap_scalars_to_output_vector(&mut self, v: Vec<InputScalar>, kind: HLSLKind) -> Vec<MutScalarVar> {
        // Lookup the variable we expect all the InputScalars to map to in the happy-path.
        // If they all map to the same Variable, the first will map to the correct Variable.
        // If they don't, we need to make a new Variable.
        let expected_var = match self.scalar_map.get(&v[0]) {
            Some((var, _)) => var.clone(),
            None => return self.remap_scalars_to_new_gp_register(v, kind)
        };

        // Check all these inputs map to the same output.
        // If they don't, we need to make a new Variable for all of them
        let mut new_v = vec![];
        for scalar in v.iter() {
            match self.scalar_map.get(scalar) {
                Some((var, comp)) if expected_var == *var => {
                    new_v.push(MutScalarVar {
                        vec: expected_var.clone(),
                        comp: *comp,
                        cast_to: None, // Assume a cast isn't necessary - this is what the next step checks
                    });
                },
                // Either this InputScalar doesn't have a mapping, or it maps to a different Variable.
                // In either case, create a new Variable
                _ => return self.remap_scalars_to_new_gp_register(v, kind)
            }
        }

        // Try to refine the kind of the common variable with the kind of this output.
        // If we fail, we need to make a new Variable.
        // Do this after we check they all map to the same Variable, because we don't want to prematurely mutate state.
        match expected_var.borrow_mut().refine(kind) {
            Some(_success) => {},
            None => return self.remap_scalars_to_new_gp_register(v, kind)
        };

        new_v
    }

    fn remap_scalars_to_new_gp_register(&mut self, v: Vec<InputScalar>, kind: HLSLKind) -> Vec<MutScalarVar> {
        let n_contig_components = v.len();
        let name = HLSLRegister::GenericRegister(format!("{}_{:03}", if n_contig_components == 1 { "s" } else { "v" }, self.registers.len()), n_contig_components as u8);
        // let kind = v.iter().fold(Some(v[0].1), |k, s| match k {
        //     Some(k) => k.intersection(s.1),
        //     None => None
        // }).expect("Can't create a new register for a group of scalars that aren't of a common kind");

        let var = Rc::new(RefCell::new(Variable {
            name,
            n_contig_components,
            kind,
        }));
        
        v.into_iter().enumerate().map(|(i, scalar)| {
            self.scalar_map.insert(scalar, (var.clone(), i.into()));
            MutScalarVar {
                vec: var.clone(),
                comp: i.into(),
                cast_to: None
            }
        }).collect()
    }
}
// fn convert_with_variables<TVM: AbstractVM>(p: Program<TVM>) -> Program<VariableVM> {

// }
