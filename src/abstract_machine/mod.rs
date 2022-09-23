use std::hash::Hash;

pub mod analysis;
pub mod hlsl;
pub mod vector;

pub trait VMRef: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self) -> bool;
}
impl VMRef for [u64; 4] {
    fn is_pure_input(&self) -> bool {
        true
    }
}
/// Marker trait for types referencing the *name* of a VM element, e.g. a vector which could be subscripted.
///
/// For scalar machines, the same type may implement [VMNameRef] and [VMDataRef].
pub trait VMNameRef: VMRef {}

/// Marker trait for types referencing a piece of *data* inside a VM element, e.g. a single scalar component of a vector
///
/// For scalar machines, the same type may implement [VMNameRef] and [VMDataRef].
pub trait VMDataRef: VMRef {}

/// The "kind" of a piece of data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataKind {
    /// IEEE floating-point
    Float,
    /// Twos complement signed integer
    SignedInt,
    /// Unsigned integer
    UnsignedInt,
    /// Any other type, including potentially non-numeric types e.g. texture samplers
    Untyped,
    // A "type hole", where the type of this data could change based on context.
    Hole,
}

/// The width in bits of a piece of data.
///
/// In vector contexts, this refers to the width of a single scalar component.
/// All vectors in a VM are assumed to have components of the same width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataWidth {
    E8,
    E16,
    E32,
    E64,
}

/// A [VMRef] with extra type information - the [DataWidth] and [DataKind].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedVMRef<TData: VMRef> {
    pub data: TData,
    pub kind: DataKind,
    pub width: DataWidth,
}

/// Base type for abstract VMs
///
/// All VMs, even vector-based ones, should implement simplistic vector-to-scalar translation
pub trait ScalarAbstractVM: std::fmt::Debug + Sized {
    type Action: ScalarAction<Self>;
    type TScalarDataRef: VMDataRef;
}

pub trait ScalarAction<TVM: ScalarAbstractVM> {
    fn outcomes(&self) -> Vec<ScalarOutcome<TVM>>;
}
/// Helper implementation for VMs which want to type-erase their actions
impl<TVM: ScalarAbstractVM> ScalarAction<TVM> for Box<dyn ScalarAction<TVM>> {
    fn outcomes(&self) -> Vec<ScalarOutcome<TVM>> {
        self.as_ref().outcomes()
    }
}

#[derive(Debug, Clone)]
pub enum ScalarOutcome<TVM: ScalarAbstractVM> {
    /// Declare that some named scalar exists, and optionally has a known value.
    Declaration {
        name: TVM::TScalarDataRef,
        value: Option<TypedVMRef<TVM::TScalarDataRef>>,
    },
    /// Declare that an output scalar has a new value, based on many input scalars.
    Dependency {
        output: TypedVMRef<TVM::TScalarDataRef>,
        inputs: Vec<TypedVMRef<TVM::TScalarDataRef>>,
    },
    /// Declare that program flow may end early due to a set of input scalars.
    EarlyOut {
        inputs: Vec<TypedVMRef<TVM::TScalarDataRef>>,
    },
}

/// Trait for programs that run on an abstract VM
pub trait Program<TVM: ScalarAbstractVM> {
    fn actions(&self) -> &Vec<TVM::Action>;
}

/// Trait for structs that can turn an arbitrary representation of a program (e.g. binary data) into a [Program] for a given [ScalarAbstractVM]
pub trait Decoder<TVM: ScalarAbstractVM> {
    type Input;
    type ScalarProgram: Program<TVM>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Self::ScalarProgram, Self::Err>;
}
