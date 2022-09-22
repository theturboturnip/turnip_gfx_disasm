use std::{hash::Hash, ops::Deref};

pub mod analysis;
pub mod hlsl;
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
    // Hole => could change based on context.
    Hole,
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

// TODO rename this to ScalarBasedAbstractVM?
pub trait AbstractVM: std::fmt::Debug {
    type TScalarDataRef: DataRef;
}

pub trait Action<TVM: AbstractVM> {
    fn outcomes(&self) -> Vec<Outcome<TVM>>;
}

// TODO does Declaration need to be separate from Dependency?
// or can every Declaration be represented as a Dependency

#[derive(Debug, Clone)]
pub enum Outcome<TVM: AbstractVM> {
    // Declare that some named scalar exists, and optionally has a known value.
    Declaration {
        name: TVM::TScalarDataRef,
        value: Option<TypedRef<TVM::TScalarDataRef>>,
    },
    // Declare that an output scalar has a new value, based on many input scalars.
    Dependency {
        output: TypedRef<TVM::TScalarDataRef>,
        inputs: Vec<TypedRef<TVM::TScalarDataRef>>,
    },
}

pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TVM>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}
