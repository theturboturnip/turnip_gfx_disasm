use std::hash::Hash;

use crate::hlsl::kinds::HLSLKind;

use expr::{Vector, Scalar, ContigSwizzle};

use self::expr::Reg;

pub mod analysis;
pub mod display;
pub mod instructions;
pub mod vector;
pub mod expr;

/// A name of an item referenced by some VM.
pub trait VMName: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    // TODO I should evaluate if these are more useful as is_readable() and is_writable().
    // is_pure_input() = !is_writable()?
    // is_output() = !is_readable()?

    /// Returns true if the data is a pure input, i.e. always read and never written.
    /// The dependency analyser uses this to see if it should be expanded into its dependencies, or whether it should have none.
    fn is_pure_input(&self) -> bool;
    /// Returns true if the data is an output.
    /// I'm not confident enough to state this means it isn't ever written, read, then written again, but anything interpreting
    /// a program will need to know if it's intended to be passed out of the program.
    fn is_output(&self) -> bool;

    // TODO all names and data must refer to values of known width
    // fn data_width(&self) -> DataWidth;
    
    /// Returns the possible HLSL kinds this may refer to.
    /// 
    /// This should not be mutable/refinable
    fn toplevel_kind(&self) -> HLSLKind;
}

/// A name of a scalar item referenced by some VM
pub trait VMScalar: VMName {}

/// A group of scalar names (NOTE: THIS DOES NOT IMPLY THE SCALARS COME FROM THE SAME VECTOR!)
pub trait VMVector: VMName {
    /// TODO SHOULD THIS IMPLY CONTIGUITY? HOW DO WE HANDLE .x_z_
    fn n_components(&self) -> usize;
}

/// A [VMName] may also be [Refinable] - i.e. can have its type refined with further context
pub trait Refinable: Sized {
    fn refined_kind(&self) -> HLSLKind;
    fn refine_kind(&self, hlsl_kind: HLSLKind) -> Option<Self>;
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

/// A [Refinable] [VMDataRef] with extra type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefinableRef<TName: VMName> {
    pub name: TName,
    pub kind: HLSLKind,
}
impl<TName: VMName> Refinable for RefinableRef<TName> {
    /// Return a copy of this data ref with the intersection of the given type mask and your actual type mask,
    /// or None if the masks are incompatible.
    fn refine_kind(&self, hlsl_kind: HLSLKind) -> Option<Self> {
        Some(Self {
            name: self.name.clone(),
            kind: self.kind.intersection(hlsl_kind)?,
        })
    }

    fn refined_kind(&self) -> HLSLKind {
        self.kind
    }
}

/// Base type for abstract VMs
///
/// Defines an abstract machine where each instruction operates on scalar elements.
pub trait AbstractVM: std::fmt::Debug + Sized {
    // /// The smallest element a VM operates on
    // type Scalar: VMScalar;
    // Scalars may originate from the same location in the VM's mind: e.g. myVector.xyz all come from myVector.
    type Register: VMVector + Reg; 
    // /// An arbitrary vector of Scalars that an instruction can operate on.
    // /// An instruction could use vec3(myVector.x, myOtherVector.y, myThirdVector.z) as an argument or an output.
    // type Vector: VMVector;

    // /// Given a vector, return the list of scalars it contains. If Vector has masked elements, they are not counted.
    // fn decompose(v: &Self::Vector) -> Vec<Self::Scalar>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action<TReg: Reg> {
    /// Assign a value derived from a set of inputs using an [HLSLOperator] to an output.
    ///
    /// The names for all inputs and output must have been previously declared.
    Assign {
        output: (TReg, ContigSwizzle),
        expr: Vector<TReg> // This also stores the HLSLKind of the output
    },
    /// discard; statement
    EarlyOut,
    If {
        expr: Scalar<TReg>,
        if_true: Vec<Self>,
        if_fals: Vec<Self>,
    }
}

/// Trait for programs that run on an abstract VM
pub trait Program<TVM: AbstractVM> {
    /// The list of Registers that are used by the program. Either pure inputs or outputs. 
    fn io_declarations(&self) -> &Vec<TVM::Register>;
    fn actions(&self) -> &Vec<Action<TVM::Register>>;
}

/// Trait for structs that can turn an arbitrary representation of a program (e.g. binary data) into a [Program] for a given [ScalarAbstractVM]
pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type Program: Program<TVM>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Self::Program, Self::Err>;
}

/// Utility function for determining if all elements collection return the same value when a mapping function is called 
/// 
/// TODO: make this handle lifetimes nicely. If i iterate over a pair &'a (x, y) maybe I should be able to find common &'a x?
pub fn find_common<'a, TIn: 'a, TOut: PartialEq, I: Iterator<Item = &'a TIn>, F: Fn(&TIn) -> Option<TOut>>(mut iter: I, f: F) -> Option<TOut> {
    let init = f(iter.next().unwrap())?;
    for i in iter {
        if f(i)? != init {
            return None
        }
    }
    Some(init)
}