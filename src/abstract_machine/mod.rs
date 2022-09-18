use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Deref,
};

pub mod scalar;
pub mod vector;

pub trait ValueRef: Clone + Copy + PartialEq + Eq + Hash + std::fmt::Debug {}

pub trait Action<TVal: ValueRef> {
    fn dependencies(&self) -> Vec<Dependency<TVal>>;
}

#[derive(Debug)]
pub struct Dependency<TVal: ValueRef> {
    pub parents: Vec<TVal>,
    pub child: TVal,
}
impl<TVal: ValueRef> Dependency<TVal> {
    pub fn new(parents: Vec<TVal>, child: TVal) -> Self {
        Dependency { parents, child }
    }
}

pub trait Decoder<TVal: ValueRef> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TVal>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

pub trait ValueRefFilter<TVal: ValueRef> {
    /// Returns true if the value is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self, v: TVal) -> bool;
}

pub struct WorldState<TVal: ValueRef, T: ValueRefFilter<TVal>> {
    valueref_filter: T,
    dependents: HashMap<TVal, HashSet<TVal>>,
}
impl<TVal: ValueRef, T: ValueRefFilter<TVal>> WorldState<TVal, T> {
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
    pub fn accum_action(&mut self, action: &dyn Action<TVal>) {
        for dep in action.dependencies() {
            if self.valueref_filter.is_pure_input(dep.child) {
                println!(
                    "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                    dep.parents, dep.child
                );
                continue;
            }

            let mut resolved_inputs = HashSet::new();
            for input in dep.parents.iter() {
                if self.valueref_filter.is_pure_input(*input) {
                    // Pure inputs are not resolved into their dependencies
                    resolved_inputs.insert(*input);
                } else {
                    if let Some(input_dependents) = self.dependents.get(input) {
                        resolved_inputs.extend(input_dependents);
                    } else {
                        println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                        resolved_inputs.insert(*input);
                    }
                }
            }
            self.dependents.insert(dep.child, resolved_inputs);
        }
    }

    pub fn dependents(&self) -> &HashMap<TVal, HashSet<TVal>> {
        &self.dependents
    }
}
