//! This crate translates compiled shader bytecode to a list of [Action]s that operate on a global state.
//! [Action]s result in [Dependency]s: for example, a = b + c results in a [Dependency] from (b, c) to a.

#[macro_use]
extern crate num_derive;

pub mod abstract_machine;
pub use abstract_machine::{Action, Decoder, Dependency};
pub mod rdna2;
