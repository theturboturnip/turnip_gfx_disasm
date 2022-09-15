//! This crate translates compiled shader bytecode to a list of [Action]s that operate on a global state.
//! That state is made entirely of scalar values, unlike the DXBC bytecode which operates on e.g. vector registers.
//!
//! [Action]s result in [Dependency]s: for example, a = b + c results in a [Dependency] from (b, c) to a.
//!
//! a, b, and c are represented as [ValueRef]. All [ValueRef]s refer to *scalar* values.
//!
//! This architecture is based on RDNA2, but is intended to function on other backends too.

use std::ops::Deref;

/// TODO more values here - special registers? function arguments?
///
/// TODO type information?
enum ValueRef {
    /// 64-bit scalar register
    GeneralPurposeRegister(u64),
    /// Location in memory that is calculated from a set of [ValueRef]s
    Memory(Vec<ValueRef>),
    /// 64-bit literal value
    Literal(u64),
    NamedSpecialReg(&'static str),
    /// Refers to a specific component of a fragment shader's vector output.
    ///
    /// Fragment shaders are a special case because they usually write to vectors
    /// e.g. output color, so we have a component index here.
    FragOutput {
        target: u64,
        vector_idx: u64,
    },
}

trait Action {
    fn dependencies(&self) -> Vec<Dependency>;
}

struct Dependency {
    parents: Vec<ValueRef>,
    child: ValueRef,
}

trait Decoder {
    type Input;
    type BaseAction: Deref<Target = dyn Action>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

pub mod rdna2;
