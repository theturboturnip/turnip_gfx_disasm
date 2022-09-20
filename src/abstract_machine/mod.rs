use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Deref,
};

pub mod scalar;
pub mod vector;

pub trait DataRef: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataKind {
    Float,
    SignedInt,
    UnsignedInt,
    Untyped,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataWidth {
    E8,
    E16,
    E32,
    E64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValueRef<TData: DataRef> {
    pub data: TData,
    pub kind: DataKind,
    pub width: DataWidth,
}

pub trait AbstractVM {
    type TScalarRef: DataRef;
}

pub trait Action<TVM: AbstractVM> {
    fn dependencies(&self) -> Vec<Dependency<TVM>>;
}

/// A Dependency from many scalar input values to a single scalar output value.
#[derive(Debug)]
pub struct Dependency<TVM: AbstractVM> {
    pub parents: Vec<ValueRef<TVM::TScalarRef>>,
    pub child: ValueRef<TVM::TScalarRef>,
}
impl<TVM: AbstractVM> Dependency<TVM> {
    pub fn new(parents: Vec<ValueRef<TVM::TScalarRef>>, child: ValueRef<TVM::TScalarRef>) -> Self {
        Dependency { parents, child }
    }
}

pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TVM>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

pub struct ScalarDependencies<TVM: AbstractVM> {
    dependents: HashMap<TVM::TScalarRef, HashSet<ValueRef<TVM::TScalarRef>>>,
}
impl<TVM: AbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for dep in action.dependencies() {
            if dep.child.data.is_pure_input() {
                println!(
                    "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                    dep.parents, dep.child
                );
                continue;
            }

            // TODO introduce logic for kind/width coercion, store keys as ValueRef

            let mut resolved_inputs = HashSet::new();
            for input in dep.parents.iter() {
                if input.data.is_pure_input() {
                    // Pure inputs are not resolved into their dependencies
                    resolved_inputs.insert(input.clone());
                } else {
                    if let Some(input_dependents) = self.dependents.get(&input.data) {
                        resolved_inputs.extend(input_dependents.iter().cloned());
                    } else {
                        println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                        resolved_inputs.insert(input.clone());
                    }
                }
            }
            self.dependents.insert(dep.child.data, resolved_inputs);
        }
    }

    pub fn dependents(&self) -> &HashMap<TVM::TScalarRef, HashSet<ValueRef<TVM::TScalarRef>>> {
        &self.dependents
    }
}
