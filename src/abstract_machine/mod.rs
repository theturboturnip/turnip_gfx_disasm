use std::hash::Hash;

use crate::hlsl::{syntax::HLSLOperator, types::HLSLType};

use self::{vector::MaskedSwizzle, vector::VectorComponent};

pub mod analysis;
pub mod display;
pub mod instructions;
pub mod vector;

pub trait VMRef: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self) -> bool;
    // TODO all names and data must refer to values of known width
    // fn data_width(&self) -> DataWidth;
}
impl VMRef for [u64; 4] {
    fn is_pure_input(&self) -> bool {
        true
    }
}

/// A reference to a piece of data in the VM which is being used for computation.
///
/// The computation it is used for should give extra context for its type.
pub trait VMDataRef<T: VMVectorNameRef>: VMRef {
    /// Return the vector name
    fn name(&self) -> &T;
    /// The type of the data - must be compatible with + should be more specific than [VMVectorNameRef::base_type_mask]
    fn type_mask(&self) -> HLSLType;
}
/// A [VMDataRef] may also be [Refinable] - i.e. can have its type refined with further context
pub trait Refinable: Sized {
    fn refine_type(&self, type_mask: HLSLType) -> Option<Self>;
}

/// Marker trait for types referencing the *name* of a VM element, e.g. a vector which could be subscripted.
/// May only be able to store a subset of [HLSLType], but does not have any information about
///
/// For scalar machines, the same type may implement [VMVectorNameRef] and [VMDataRef].
pub trait VMVectorNameRef: VMRef {
    /// Number of components in the vector
    fn n_components(&self) -> u8;
    /// Base type - the lowest common denominator [HLSLType] that could this name could possibly hold.
    fn base_type_mask(&self) -> HLSLType;
}

/// A VMRef referring to a specific scalar within a VM
pub type VMScalarDataRef<T: VMVectorNameRef> = (T, VectorComponent);
impl<T: VMVectorNameRef> VMRef for VMScalarDataRef<T> {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}

/// A VMDataRef that represents a VM's "element" - the main unit of computation for instructions.
pub trait VMVectorDataRef<T: VMVectorNameRef>: VMDataRef<T> {
    fn swizzle(&self) -> MaskedSwizzle;
    /// Returns a list of the components that were actually used from self
    ///
    /// e.g. for r0.x_w_, (r0, x) and (r0, w) will be returned
    fn decompose(&self) -> Vec<VMScalarDataRef<T>> {
        self.swizzle()
            .0
            .iter()
            .filter_map(|comp| match comp {
                Some(comp) => Some((self.name().clone(), *comp)),
                None => None,
            })
            .collect()
    }
}
impl<T: VMVectorNameRef> VMRef for (T, MaskedSwizzle) {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}
impl<T: VMVectorNameRef> VMDataRef<T> for (T, MaskedSwizzle) {
    fn name(&self) -> &T {
        &self.0
    }

    fn type_mask(&self) -> HLSLType {
        self.name().base_type_mask()
    }
}
impl<T: VMVectorNameRef> VMVectorDataRef<T> for (T, MaskedSwizzle) {
    fn swizzle(&self) -> MaskedSwizzle {
        self.1
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
impl<TData: VMRef> Refinable for TypedVMRef<TData> {
    /// Return a copy of this data ref with the intersection of the given type mask and your actual type mask,
    /// or None if the masks are incompatible.
    fn refine_type(&self, type_mask: HLSLType) -> Option<Self> {
        Some(Self {
            data: self.data.clone(),
            kind: self.kind.intersection(type_mask)?,
            width: self.width,
        })
    }
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

pub enum Outcome<TVM: AbstractVM> {
    /// Declare a name exists
    Declare(TVM::TVectorNameRef),
    /// Assign a value derived from a set of inputs using an [HLSLOperator] to an output.
    ///
    /// The names for all inputs and output must have been previously declared.
    Assign {
        output: TVM::TVectorDataRef,
        op: HLSLOperator,
        inputs: Vec<TVM::TVectorDataRef>,
        /// ```python
        /// for i in len(dep_rel)
        ///     output.decompose()[i] depends on [
        ///         (inputs[input_idx], input_comp)
        ///         for (input_idx, input_comp) in dep_rel[i]
        ///     ]
        /// ```
        dep_rel: Vec<Vec<(usize, VectorComponent)>>,
    },
    /// Early out based on a set of inputs
    EarlyOut { inputs: Vec<TVM::TVectorDataRef> },
}

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
