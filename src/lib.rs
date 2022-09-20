//! This crate translates shaders to a list of [Action]s that operate on an abstract virtual machine.
//! [Action]s result in [Outcome]s: for example, a = b + c results in a [Outcome::Dependency] from (b, c) to a,
//! and some instructions may *declare* a value L exists with [Outcome::Declaration].

#[macro_use]
extern crate num_derive;

pub mod abstract_machine;
pub use abstract_machine::{Action, Decoder, Outcome};
pub mod amdil;
pub mod amdil_text;
pub mod rdna2;
