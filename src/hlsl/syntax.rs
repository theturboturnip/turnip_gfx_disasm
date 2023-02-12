//! This module implements types that reflect HLSL syntax.
//! Operators are based on [https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-operators].
//! Only the operators required to translate from assembly to HLSL are currently implemented
//! i.e. the Structure and Array operators are not implemented because they should be handled in [HLSLVectorName],
//! and pre/postfix operators are not implemented because their more complex usages are not possible in assembly.
//!
//! Plan to implement
//! - Arithmetic operators (including bitwise arithmetic): +, -, *, /, %, <<, >>, &, |, ^
//! - Binary casts/cast operator
//! - Unary operators (including bitwise) (except !, becuase it's boolean) -, +, ~
//!
//! Not planned for now:
//! - Binary/numeric assignment operators +=, -=, *=, /=, %=, <<=, >>=, &=, |=, ^=
//! - Booleans &&, ||, ?:, including unary operator !
//! - Array [i]
//! - Comma ,
//! - Comparison operators (they're boolean)
//! - Prefix/Postfix ++, --
//! - Structure .
//! - Assignment (this is implicitly represented in Outcome types)
//!
//! TODO need a way for a hole to have multiple possible values but not all of them,
//! e.g. float/int but NOT uint.
//! Operations do this a lot (e.g. bit shifts only work on uint or int)
//!
//! ## Usage Plan
//! Virtual machines that implement [HLSLCompatibleAbstractVM] provide actions that support [HLSLCompatibleAction] returning [HLSLCompatibleOutcome]s.
//! [HLSLCompatibleOutcome] will use `Box<dyn OperatorTarget>`? to represent values, or something along those lines, which allows for e.g. negation of scalar values in Definitions.
//! [HLSLCompatibleOutcome::Operation] will use [UnconcreteOpResult] or some compat-equivalent instead of (opname, input_datarefs, output_datarefs).
//! Essentially, the VM will be responsible for converting its own actions into HLSL operations, rather than retrofitting that later.
//!
//! This comes with its own problems when performing type coersion:
//!     1) Coercing to concrete types is not necessarily possible initially, as some operations are extremely agnostic and only have their types known later.
//!         e.g. in AMDIL, after declaring input v1.xyz (which may be any numeric type), the operation `mov r0.xyz, v1.xyz` does not provide clarification on what types are involved
//!         and the only way to tell the types of v1 and r0 is to see how `r0` is used later.
//!     2) VM operations may have less permissive type holes than HLSL.
//!         e.g. in AMDIL addition has separate instructions for (float add), (int add), (uint add); but HLSL just has [ArithmeticOp::Plus] which allows Float, Int, or Uint.
//!     3) VM operations may have more permissive type holes than HLSL?
//!     4) VM operations may have mismatching type holes compared to HLSL.
//!         e.g. a VM operation *may* have issues where, say, a shift instruction requires all operands to be the same (uint << uint = uint, or int << int = int),
//!         which is equivalent to `Hole(0, INTEGER) << Hole(0, INTEGER) = Hole(0, INTEGER)`,
//!         but the HLSL version may have two holes `Hole(0, INTEGER) << Hole(1, INTEGER) = Hole(0, INTEGER).
//!         I'm not sure we will encounter this problem, but we need a safety check for it at least.
//!         

use std::cmp::max;

use super::types::{
    HLSLConcreteType, HLSLHoleTypeMask, HLSLNumericType, HLSLOperandType, HLSLType,
};

/// Trait implemented for HLSL operators and intrinsic functions which take 1..N inputs and output 1 value.
///
/// Used to determine what data types are accepted as inputs and outputs.
pub trait Operator: std::fmt::Debug {
    /// Return the "type-spec" for the operator - the set of input types and output type.
    /// These are not concretized, and may involve type holes.
    fn get_typespec(&self) -> OperatorTypeSpec;

    /// Return the number of input arguments the operator takes.
    fn n_inputs(&self) -> usize;
}

/// The "typespec" for an operator - the input and output types that it accepts, and the accepted types for any referenced type holes.
#[derive(Debug)]
pub struct OperatorTypeSpec {
    input_types: Vec<HLSLOperandType>,
    output_type: HLSLOperandType,
    holes: Vec<HLSLType>,
}
impl OperatorTypeSpec {
    /// Creates a new OperatorTypeSpec while checking the number of holes is correct
    fn new(
        input_types: Vec<HLSLOperandType>,
        output_type: HLSLOperandType,
        holes: Vec<HLSLType>,
    ) -> Self {
        // Sanity check - make sure the maximum hole ID referenced in any HLSLOperandType::Hole(id) == the number of elements in `holes`
        let mut max_referenced_hole = None;
        for input in input_types.iter() {
            if let HLSLOperandType::Hole(id) = input {
                max_referenced_hole =
                    Some(max_referenced_hole.map_or(*id, |max_hole_id| max(max_hole_id, *id)));
            }
        }
        if let HLSLOperandType::Hole(id) = output_type {
            max_referenced_hole =
                Some(max_referenced_hole.map_or(id, |max_hole_id| max(max_hole_id, id)));
        }

        if let Some(max_referenced_hole) = max_referenced_hole {
            if max_referenced_hole != holes.len() - 1 {
                panic!("Mismatch in referenced holes - maximum referenced hole = {}, but {} are present: {:?}", max_referenced_hole, holes.len(), holes);
            }
        }

        OperatorTypeSpec {
            input_types,
            output_type,
            holes,
        }
    }

    /// If all holes represent a single type, return a [ConcreteOperatorTypes] with all of the types concretized
    pub fn try_concretize(&self) -> Option<ConcreteOperatorTypes> {
        let concrete_holes: Option<Vec<HLSLConcreteType>> =
            self.holes.iter().map(|h| h.try_concretize()).collect();
        concrete_holes.map(|holes| {
            let input_types = self
                .input_types
                .iter()
                .map(|t| t.concretize(&holes))
                .collect();
            let output_type = self.output_type.concretize(&holes);
            ConcreteOperatorTypes {
                input_types,
                output_type,
            }
        })
    }
}

/// Equivalent to [OperatorTypeSpec], but all types are concrete
pub struct ConcreteOperatorTypes {
    input_types: Vec<HLSLConcreteType>,
    output_type: HLSLConcreteType,
}

// TODO: FUCK WE NEED A SINGLE CATCH-ALL HLSLOperator ENUM
// WE NEED TO BE ABLE TO STORE THEM CONSISTENTLY
// AND IT MEANS WE DONT NEED THE RC BULLSHIT ANYMORE
#[derive(Debug, Clone, Copy)]
pub enum HLSLOperator {
    Assign,
    Unary(UnaryOp),
    Arithmetic(ArithmeticOp),
    BinaryArithmetic(BinaryArithmeticOp),
    NumericCast(NumericCastTo),
    SampleI(SampleIntrinsic),
    NumericI(NumericIntrinsic),
    FauxBoolean(FauxBooleanOp),
}
impl Operator for HLSLOperator {
    fn get_typespec(&self) -> OperatorTypeSpec {
        match self {
            HLSLOperator::Assign => OperatorTypeSpec::new(
                vec![HLSLOperandType::Hole(0)],
                HLSLOperandType::Hole(0),
                vec![HLSLHoleTypeMask::all().into()],
            ),
            HLSLOperator::Unary(x) => x.get_typespec(),
            HLSLOperator::Arithmetic(x) => x.get_typespec(),
            HLSLOperator::BinaryArithmetic(x) => x.get_typespec(),
            HLSLOperator::NumericCast(x) => x.get_typespec(),
            HLSLOperator::SampleI(x) => x.get_typespec(),
            HLSLOperator::NumericI(x) => x.get_typespec(),
            HLSLOperator::FauxBoolean(x) => x.get_typespec(),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            HLSLOperator::Assign => 1,
            HLSLOperator::Unary(x) => x.n_inputs(),
            HLSLOperator::Arithmetic(x) => x.n_inputs(),
            HLSLOperator::BinaryArithmetic(x) => x.n_inputs(),
            HLSLOperator::NumericCast(x) => x.n_inputs(),
            HLSLOperator::SampleI(x) => x.n_inputs(),
            HLSLOperator::NumericI(x) => x.n_inputs(),
            HLSLOperator::FauxBoolean(x) => x.n_inputs(),
        }
    }
}

/// Unary operations that operate on a single value and return a single value
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    /// Inverts each bit of X
    ///
    /// uint/int only
    BinaryNot,
    /// Makes X = -X
    ///
    /// Supports float/int, unsure how uint works
    Negate,
    /// Does nothing
    ///
    /// Supports float/int/uint
    Plus,
}
impl Operator for UnaryOp {
    fn get_typespec(&self) -> OperatorTypeSpec {
        let hole = match self {
            UnaryOp::BinaryNot => HLSLHoleTypeMask::INTEGER,
            UnaryOp::Negate => HLSLHoleTypeMask::NUMERIC_FLOAT | HLSLHoleTypeMask::NUMERIC_SINT,
            UnaryOp::Plus => HLSLHoleTypeMask::NUMERIC,
        };

        OperatorTypeSpec {
            input_types: vec![HLSLOperandType::Hole(0)],
            output_type: HLSLOperandType::Hole(0),
            holes: vec![hole.into()],
        }
    }

    fn n_inputs(&self) -> usize {
        1
    }
}

/// Integer/float arithmetic operations, which take two inputs X and Y and return one output.
#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    /// Adds X and Y
    ///
    /// float/int/uint
    Plus,
    /// Subtracts Y from X
    ///
    /// float/int/uint
    Minus,
    /// Multiplies X by Y
    ///
    /// float/int/uint
    Times,
    /// Divides X by Y
    ///
    /// float/int/uint
    Div,
    /// Remainder of division of X and Y
    ///
    /// float/int/uint
    Mod,
}
impl Operator for ArithmeticOp {
    fn get_typespec(&self) -> OperatorTypeSpec {
        // All of these operators are compatible with float/int/uint i.e. NUMERIC
        OperatorTypeSpec::new(
            vec![HLSLOperandType::Hole(0), HLSLOperandType::Hole(0)],
            HLSLOperandType::Hole(0),
            vec![HLSLHoleTypeMask::NUMERIC.into()],
        )
    }

    fn n_inputs(&self) -> usize {
        2
    }
}

/// Binary/bitwise operations which take two inputs X and Y and return a single output.
#[derive(Debug, Clone, Copy)]
pub enum BinaryArithmeticOp {
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}
impl Operator for BinaryArithmeticOp {
    fn get_typespec(&self) -> OperatorTypeSpec {
        let is_shift = match self {
            BinaryArithmeticOp::LeftShift => true,
            BinaryArithmeticOp::RightShift => true,
            BinaryArithmeticOp::BitwiseAnd => false,
            BinaryArithmeticOp::BitwiseOr => false,
            BinaryArithmeticOp::BitwiseXor => false,
        };

        if !is_shift {
            OperatorTypeSpec::new(
                vec![HLSLOperandType::Hole(0), HLSLOperandType::Hole(0)],
                HLSLOperandType::Hole(0),
                vec![HLSLHoleTypeMask::INTEGER.into()],
            )
        } else {
            // Left input = the thing getting modified
            // Right input = the shift amount - must be unsigned int?
            OperatorTypeSpec::new(
                vec![
                    HLSLOperandType::Hole(0),
                    HLSLNumericType::UnsignedInt.into(),
                ],
                HLSLOperandType::Hole(0),
                vec![HLSLHoleTypeMask::INTEGER.into()],
            )
        }
    }

    fn n_inputs(&self) -> usize {
        2
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NumericCastTo(pub HLSLNumericType);
impl Operator for NumericCastTo {
    fn get_typespec(&self) -> OperatorTypeSpec {
        OperatorTypeSpec::new(
            vec![HLSLOperandType::Hole(0)],
            self.0.into(),
            vec![HLSLHoleTypeMask::NUMERIC.into()],
        )
    }

    fn n_inputs(&self) -> usize {
        1
    }
}

/// Texture sampling intrinsic functions, which take a texture argument and at least one other input to produce a single output.
#[derive(Debug, Clone, Copy)]
pub enum SampleIntrinsic {
    Tex2D,
    // Just exists for compatibility with current bad behaviour in AMDIL - TODO remove
    Tex2DFakeDoesntTakeTex,
}
impl Operator for SampleIntrinsic {
    fn get_typespec(&self) -> OperatorTypeSpec {
        match self {
            Self::Tex2D => OperatorTypeSpec::new(
                vec![
                    HLSLConcreteType::Texture2D.into(),
                    HLSLNumericType::Float.into(),
                ],
                HLSLNumericType::Float.into(),
                vec![],
            ),
            Self::Tex2DFakeDoesntTakeTex => OperatorTypeSpec::new(
                vec![HLSLNumericType::Float.into()],
                HLSLNumericType::Float.into(),
                vec![],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Tex2D => 2,
            Self::Tex2DFakeDoesntTakeTex => 1,
        }
    }
}

/// Numeric intrinsic functions
#[derive(Debug, Clone, Copy)]
pub enum NumericIntrinsic {
    Dot,
    Min,
    Max,
}
impl Operator for NumericIntrinsic {
    fn get_typespec(&self) -> OperatorTypeSpec {
        match self {
            Self::Min | Self::Max | Self::Dot => OperatorTypeSpec::new(
                vec![HLSLOperandType::Hole(0), HLSLOperandType::Hole(0)],
                HLSLOperandType::Hole(0),
                vec![HLSLHoleTypeMask::NUMERIC.into()],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Min | Self::Max | Self::Dot => 2,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FauxBooleanOp {
    Lt,
    Le,
    Gt,
    Ge,
    Ternary,
}
impl Operator for FauxBooleanOp {
    fn get_typespec(&self) -> OperatorTypeSpec {
        match self {
            Self::Lt | Self::Le | Self::Gt | Self::Ge => OperatorTypeSpec::new(
                vec![HLSLOperandType::Hole(0), HLSLOperandType::Hole(0)],
                HLSLOperandType::Hole(1),
                vec![
                    HLSLHoleTypeMask::NUMERIC.into(),
                    HLSLHoleTypeMask::INTEGER.into(),
                ],
            ),
            Self::Ternary => OperatorTypeSpec::new(
                vec![
                    HLSLOperandType::Hole(0),
                    HLSLOperandType::Hole(1),
                    HLSLOperandType::Hole(1),
                ],
                HLSLOperandType::Hole(1),
                vec![
                    HLSLHoleTypeMask::INTEGER.into(),
                    HLSLHoleTypeMask::NUMERIC.into(),
                ],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Lt | Self::Le | Self::Gt | Self::Ge => 2,
            Self::Ternary => 3,
        }
    }
}

pub trait UnconcreteOpTarget: std::fmt::Debug {
    fn unconcrete_type(&self) -> HLSLType;
}

/// Data on an operation performed on a set of inputs, each of which may not have a concrete type.
///
/// NOTE: TData may be an Rc<RefCell<Variable>>, which during the course of analysis may have its type refined.
#[derive(Debug, Clone)]
pub struct UnconcreteOpResult<TData: UnconcreteOpTarget> {
    /// The operation being performed.
    pub op: HLSLOperator,
    /// List of input values, which each have a (potentially not-concrete) type.
    ///
    /// Checked by the [new] constructor to have the same number of elements as `op.n_inputs()`.
    pub inputs: Vec<TData>,
}
impl<TData: UnconcreteOpTarget> UnconcreteOpResult<TData> {
    pub fn new(op: HLSLOperator, inputs: Vec<TData>) -> Self {
        if op.n_inputs() != inputs.len() {
            panic!(
                "Op {:?} takes {} inputs but UnconcreteOpResult got {} ({:?})",
                op,
                op.n_inputs(),
                inputs.len(),
                inputs
            );
        }
        // TODO better way to do this
        let x = Self { op, inputs };
        // Check that the types aren't completely batshit
        x.get_current_typespec(None);
        x
    }

    pub fn get_current_typespec(&self, output_type: Option<HLSLType>) -> OperatorTypeSpec {
        let typespec = self.op.get_typespec();
        let mut new_holes = typespec.holes.clone();
        for (input, input_typespec) in self.inputs.iter().zip(typespec.input_types.iter()) {
            let input_variable_type = input.unconcrete_type();
            // 1. check that the input_variable_type is compatible with the input_typespec
            if !input_variable_type
                .intersect(&input_typespec.as_hlsltype(&typespec.holes))
                .is_ok()
            {
                panic!("Value {:?} has unconcrete type {:?} that is incompatible with op {:?}'s typespec {:?}", input, input_variable_type, self.op, typespec)
            }
            // 2. for each non-concrete type i.e. type hole in the typespec, intersect it with the inputs that map to it
            match input_typespec {
                HLSLOperandType::Hole(idx) => {
                    new_holes[*idx] = new_holes[*idx]
                        .intersect(&input_variable_type)
                        .expect("Intersection error during hole resolution :(");
                }
                _ => {}
            }
        }
        if let Some(output_type) = &output_type {
            if !typespec
                .output_type
                .as_hlsltype(&typespec.holes)
                .intersect(&output_type)
                .is_ok()
            {
                panic!("Output of {:?} has been assigned unconcrete type {:?} that is incompatible with typespec {:?}", self.op, output_type, typespec)
            }
            // 2. for each non-concrete type i.e. type hole in the typespec, intersect it with the inputs that map to it
            match &typespec.output_type {
                HLSLOperandType::Hole(idx) => {
                    new_holes[*idx] = new_holes[*idx]
                        .intersect(&output_type)
                        .expect("Intersection error during hole resolution :(");
                }
                _ => {}
            }
        }

        new_holes
            .iter_mut()
            .map(|c| *c = c.concretize_single_mask())
            .for_each(drop);

        // Go back through and check the holes still work now that they've been changed
        for (input, input_typespec) in self.inputs.iter().zip(typespec.input_types.iter()) {
            let input_variable_type = input.unconcrete_type();
            // The type of the variable (e.g. Hole) may be more general than the newly reduced value
            // That's fine, as long as the newly reduced value is within the original bounds
            // Basically, check that this input variable could be treated as
            if !input_variable_type
                .intersect(&input_typespec.as_hlsltype(&typespec.holes))
                .is_ok()
            {
                panic!("During hole resolution, value {:?} has unconcrete type {:?} that was made incompatible with op {:?}'s typespec {:?} (new holes {:?})", input, input_variable_type, self.op, input_typespec, new_holes)
            }
        }
        if let Some(output_type) = &output_type {
            if !typespec
                .output_type
                .as_hlsltype(&typespec.holes)
                .intersect(&output_type)
                .is_ok()
            {
                panic!("During hole resolution, output of {:?} has unconcrete type {:?} that is incompatible with typespec {:?} and new holes {:?}", self.op, output_type, typespec, new_holes)
            }
        }

        // TODO does the constructor need to do any more typechecking? I don't think so
        OperatorTypeSpec::new(typespec.input_types, typespec.output_type, new_holes)
    }
}
