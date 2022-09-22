use std::{hash::Hash, ops::Deref};

pub mod analysis;
pub mod hlsl;
pub mod vector;

pub trait DataRef: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self) -> bool;
}
impl DataRef for [u64; 4] {
    fn is_pure_input(&self) -> bool {
        true
    }
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

/// Base type for abstract VMs
///
/// All VMs, even vector-based ones, should implement simplistic vector-to-scalar translation
pub trait ScalarAbstractVM: std::fmt::Debug {
    type TScalarDataRef: DataRef;
}

pub trait ScalarAction<TVM: ScalarAbstractVM> {
    fn outcomes(&self) -> Vec<ScalarOutcome<TVM>>;
}

// TODO does Declaration need to be separate from Dependency?
// or can every Declaration be represented as a Dependency

#[derive(Debug, Clone)]
pub enum ScalarOutcome<TVM: ScalarAbstractVM> {
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

pub trait Decoder<TVM: ScalarAbstractVM> {
    type Input;
    type BaseAction: Deref<Target = dyn ScalarAction<TVM>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}
