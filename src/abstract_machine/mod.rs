use std::hash::Hash;

use crate::hlsl::types::HLSLType;

use self::{vector::MaskedSwizzle, vector::VectorComponent};

pub mod analysis;
pub mod display;
pub mod instructions;
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
pub trait VMVectorNameRef: VMRef {}

/// A VMRef referring to a specific scalar within a VM
pub type VMScalarDataRef<T: VMVectorNameRef> = (T, VectorComponent);
impl<T: VMVectorNameRef> VMRef for VMScalarDataRef<T> {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}

/// A VMDataRef that represents a VM's "element" - the main unit of computation for instructions.
pub trait VMVectorDataRef<T: VMVectorNameRef>: VMRef {
    fn name(&self) -> &T;
    fn decompose(&self) -> Vec<VMScalarDataRef<T>>;
}
impl<T: VMVectorNameRef> VMRef for (T, MaskedSwizzle) {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}
impl<T: VMVectorNameRef> VMVectorDataRef<T> for (T, MaskedSwizzle) {
    fn decompose(&self) -> Vec<VMScalarDataRef<T>> {
        self.1
             .0
            .iter()
            .filter_map(|comp| match comp {
                Some(comp) => Some((self.0.clone(), *comp)),
                None => None,
            })
            .collect()
    }

    fn name(&self) -> &T {
        &self.0
    }
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

/// A [VMRef] with extra type information - the [DataWidth] and [HLSLType].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedVMRef<TData: VMRef> {
    pub data: TData,
    pub kind: HLSLType,
    pub width: DataWidth,
}

/// Base type for abstract VMs
///
/// Defines an abstract machine where each instruction operates on
/// "elements" - vectors of length `n <= 4` which can be subscripted with [VectorComponent] or [MaskedSwizzle].
///
/// Scalar-based machines can be defined by setting n = 1.
pub trait AbstractVM: std::fmt::Debug + Sized {
    /// The type which [Program]s written for this VM are defined in terms of
    type Action: Action<Self>;
    /// The unique name of an element in the abstract VM.
    ///
    /// Element = the unit that abstract VM instructions operate on.
    ///
    /// e.g. for DXBC and AMDIL instructions operate on vectors => TVectorNameRef = a VectorNameRef.
    type TVectorNameRef: VMVectorNameRef;
    /// A reference to data held inside an element.
    /// e.g. in a vector machine, a TVectorNameRef plus a swizzle.
    ///
    /// Can be decomposed into [ScalarDataRef]s.
    type TVectorDataRef: VMVectorDataRef<Self::TVectorNameRef>; // (Self::TVectorNameRef, MaskedSwizzle);
}

pub trait Action<TVM: AbstractVM> {
    fn outcomes(&self) -> Vec<LegacyOutcome<TVM>>;
}
/// Helper implementation for VMs which want to type-erase their actions
impl<TVM: AbstractVM> Action<TVM> for Box<dyn Action<TVM>> {
    fn outcomes(&self) -> Vec<LegacyOutcome<TVM>> {
        self.as_ref().outcomes()
    }
}

#[derive(Debug, Clone)]
pub enum LegacyOutcome<TVM: AbstractVM> {
    /// Declare that some named element exists, and optionally has a known value.
    Declaration {
        name: VMScalarDataRef<TVM::TVectorNameRef>,
        value: Option<TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>>,
    },
    /// Declare that an output scalar has a new value, based on many input scalars.
    Dependency {
        output: TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>,
        inputs: Vec<TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>>,
    },
    /// Declare that program flow may end early due to a set of input scalars.
    EarlyOut {
        inputs: Vec<TypedVMRef<VMScalarDataRef<TVM::TVectorNameRef>>>,
    },
}

// pub enum Outcome<TVM: AbstractVM> {
//     /// Declare a name exists
//     Declare(TVM::TVectorNameRef),
//     /// Assign a value derived from a set of inputs using an [HLSLOperator] to an output.
//     ///
//     /// The names for all inputs and output must have been previously declared.
//     Assign {
//         output: TypedVMRef<TVM::TVectorDataRef>,
//         op: HLSLOperator,
//         inputs: Vec<TypedVMRef<TVM::TVectorDataRef>>,
//         dep_rel: Box<dyn DependencyRelation<TVM>>,
//     },
//     /// Early out based on a set of inputs
//     EarlyOut {
//         inputs: Vec<TypedVMRef<TVM::TVectorDataRef>>,
//     },
// }

/// Trait for programs that run on an abstract VM
pub trait Program<TVM: AbstractVM> {
    fn actions(&self) -> &Vec<TVM::Action>;
}

/// Trait for structs that can turn an arbitrary representation of a program (e.g. binary data) into a [Program] for a given [ScalarAbstractVM]
pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type Program: Program<TVM>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Self::Program, Self::Err>;
}
