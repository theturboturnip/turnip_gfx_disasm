use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Deref,
};

pub mod scalar;
pub mod vector;

pub trait DataRef: Clone + Copy + PartialEq + Eq + Hash + std::fmt::Debug {
    fn as_bits(&self) -> Option<u64>;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueRef<TData: DataRef> {
    pub data: TData,
    pub kind: DataKind,
    pub width: DataWidth,
}

pub trait Action<TData: DataRef> {
    fn dependencies(&self) -> Vec<Dependency<TData>>;
}

#[derive(Debug)]
pub struct Dependency<TData: DataRef> {
    pub parents: Vec<ValueRef<TData>>,
    pub child: ValueRef<TData>,
}
impl<TData: DataRef> Dependency<TData> {
    pub fn new(parents: Vec<ValueRef<TData>>, child: ValueRef<TData>) -> Self {
        Dependency { parents, child }
    }
}

pub trait Decoder<TData: DataRef> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TData>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

pub trait DataRefFilter<TData: DataRef> {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self, v: TData) -> bool;
}

pub struct WorldState<TData: DataRef, T: DataRefFilter<TData>> {
    valueref_filter: T,
    dependents: HashMap<TData, HashSet<ValueRef<TData>>>,
}
impl<TData: DataRef, T: DataRefFilter<TData>> WorldState<TData, T> {
    pub fn new(valueref_filter: T) -> Self {
        Self {
            valueref_filter,
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action<TData>) {
        for dep in action.dependencies() {
            if self.valueref_filter.is_pure_input(dep.child.data) {
                println!(
                    "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                    dep.parents, dep.child
                );
                continue;
            }

            // TODO introduce logic for kind/width coercion, store keys as ValueRef

            let mut resolved_inputs = HashSet::new();
            for input in dep.parents.iter() {
                if self.valueref_filter.is_pure_input(input.data) {
                    // Pure inputs are not resolved into their dependencies
                    resolved_inputs.insert(*input);
                } else {
                    if let Some(input_dependents) = self.dependents.get(&input.data) {
                        resolved_inputs.extend(input_dependents);
                    } else {
                        println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                        resolved_inputs.insert(*input);
                    }
                }
            }
            self.dependents.insert(dep.child.data, resolved_inputs);
        }
    }

    pub fn dependents(&self) -> &HashMap<TData, HashSet<ValueRef<TData>>> {
        &self.dependents
    }
}
