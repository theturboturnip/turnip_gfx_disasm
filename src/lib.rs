//! This crate translates compiled shader bytecode to a list of [Action]s that operate on a global state.
//! That state is made entirely of scalar values, unlike the DXBC bytecode which operates on e.g. vector registers.
//!
//! [Action]s result in [Dependency]s: for example, a = b + c results in a [Dependency] from (b, c) to a.
//!
//! a, b, and c are represented as [ValueRef]. All [ValueRef]s refer to *scalar* values.
//!
//! This architecture is based on RDNA2, but is intended to function on other backends too.

#[macro_use]
extern crate num_derive;

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Deref,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Output {
    VertPosition {
        idx: u64,
        vector_comp: usize,
    },
    VertParameter {
        idx: u64,
        vector_comp: usize,
    },
    FragColor {
        idx: u64,
        vector_comp: usize,
    },
    Other {
        name: &'static str,
        idx: u64,
        vector_comp: usize,
    },
}

/// TODO more values here - special registers? function arguments?
///
/// TODO type information?
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ValueRef {
    /// 64-bit scalar per-executor-group register
    GeneralPurposeGlobalRegister(u64),
    /// 64-bit scalar per-executor register
    GeneralPurposeRegister(u64),
    /// Value from memory (TODO: Add refs to actual included values and make copyable?)
    Memory(),
    /// 64-bit literal value
    Literal(u64),
    /// Register with special meaning
    SpecialReg { name: &'static str, idx: u64 },
    /// Refers to a specific component of a fragment shader's vector output.
    ///
    /// Fragment shaders are a special case because they usually write to vectors
    /// e.g. output color, so we have a component index here.
    Output(Output),
}

pub trait Action {
    fn dependencies(&self) -> Vec<Dependency>;
}

#[derive(Debug)]
pub struct Dependency {
    parents: Vec<ValueRef>,
    child: ValueRef,
}
impl Dependency {
    fn new(parents: Vec<ValueRef>, child: ValueRef) -> Dependency {
        Dependency { parents, child }
    }
}

pub trait Decoder {
    type Input;
    type BaseAction: Deref<Target = dyn Action>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

pub trait ValueRefFilter {
    /// Returns true if the value is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self, v: ValueRef) -> bool;
}
pub struct BasicValueRefFilter {}
impl BasicValueRefFilter {
    pub fn new() -> Self {
        Self {}
    }
}
impl ValueRefFilter for BasicValueRefFilter {
    fn is_pure_input(&self, v: ValueRef) -> bool {
        match v {
            ValueRef::Literal(..) => true,
            _ => false,
        }
    }
}

pub struct WorldState<T: ValueRefFilter> {
    valueref_filter: T,
    dependents: HashMap<ValueRef, HashSet<ValueRef>>,
}
impl<T: ValueRefFilter> WorldState<T> {
    pub fn new(valueref_filter: T) -> Self {
        Self {
            valueref_filter,
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o), set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action) {
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

    pub fn dependents(&self) -> &HashMap<ValueRef, HashSet<ValueRef>> {
        &self.dependents
    }
}

pub mod rdna2;
