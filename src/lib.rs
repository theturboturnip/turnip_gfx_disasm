//! The crate allows the parsing and analysis of many shader languages, by representing each language as an abstract virtual machine.
//!
//! Each language backend should at least create an implementation of [AbstractVM], and create a [Decoder] which decodes
//! shader text (be it binary or character-based) into [Program] returning a set of [Action]s.
//! Usually, each instruction in the original language should map to a single [Action], but this isn't required.
//!
//! The aforementioned scalar values are represented with types implementing [VMVectorDataRef] (a reference to a known piece of data inside the abstract VM),
//! and in some cases with [abstract_machine::RefinableVMDataRef]s enclosing them for extra metadata.
//!
//! At time of writing, scalar machines can have dependency analysis through [abstract_machine::analysis::dependency::ScalarDependencies],
//! and through the implementation of extra traits a VM can take advantage of [abstract_machine::analysis::variable::VariableAbstractMachine],
//! to be translated into a HLSL virtual machine.
//!
//! Current virtual machines:
//! - [amdil_text::vm::AMDILAbstractVM] - AMDIL text-based disassembly language, very similar to DXBC, very incomplete
//! - [rdna2::vm::RDNA2AbstractVM] - RDNA2 binary language, scalar based, very incomplete
//! - [hlsl::vm::HLSLAbstractVM] - HLSL, currently only supported via translation from other backends.

#[macro_use]
extern crate num_derive;

pub mod abstract_machine;
pub mod amdil_text;
pub mod hlsl;
// pub mod rdna2;

pub use abstract_machine::{
    AbstractVM, Action, DataWidth, Decoder, Program
};
