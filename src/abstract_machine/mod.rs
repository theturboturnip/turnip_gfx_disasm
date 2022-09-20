use std::{hash::Hash, ops::Deref};

pub mod analysis;
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
pub struct TypedRef<TData: DataRef> {
    pub data: TData,
    pub kind: DataKind,
    pub width: DataWidth,
}

pub trait AbstractVM: std::fmt::Debug {
    type TDataRef: DataRef;
}
// Marker trait for VMs that use a TDataRef that refers to a single Scalar.
pub trait ScalarBasedAbstractVM: AbstractVM {}

pub trait Action<TVM: AbstractVM> {
    fn outcomes(&self) -> Vec<Outcome<TVM>>;
}

#[derive(Debug, Clone)]
pub enum Outcome<TVM: AbstractVM> {
    // Declare that some named scalar exists, and optionally has a known value.
    Declaration {
        name: TVM::TDataRef,
        value: Option<TypedRef<TVM::TDataRef>>,
    },
    // Declare that an output scalar has a new value, based on many input scalars.
    Dependency {
        output: TypedRef<TVM::TDataRef>,
        inputs: Vec<TypedRef<TVM::TDataRef>>,
    },
}

pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TVM>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}
