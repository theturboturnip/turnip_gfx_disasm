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

use std::ops::Deref;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueRef {
    /// 64-bit scalar per-executor-group register
    GeneralPurposeGlobalRegister(u64),
    /// 64-bit scalar per-executor register
    GeneralPurposeRegister(u64),
    /// Location in memory (TODO: Add refs to actual included values and make copyable?)
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

pub mod rdna2;
