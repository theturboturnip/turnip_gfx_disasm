//! This module implements types that reflect HLSL syntax.
//! Operators are based on [https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-operators].
//! Only the operators required to translate from assembly to HLSL are currently implemented
//! i.e. the Structure and Array operators are not implemented because they should be handled in [HLSLVectorName],
//! and pre/postfix operators are not implemented because their more complex usages are not possible in assembly.
//!
//! Implements
//! - [ArithmeticOp], [BinaryArithmeticOp], Arithmetic operators (including bitwise arithmetic): +, -, *, /, %, <<, >>, &, |, ^
//! - [NumericCastTo] Binary casts/cast operator
//! - [UnaryOp] Unary operators (including bitwise) (except !, becuase it's boolean) -, +, ~
//! - [FauxBooleanOp] Comparison operators
//! - [HLSLOperator::Assign] Assignment (i.e. identity function)
//! - [SampleIntrinsic] tex2D (more to follow)
//! - [NumericIntrinsic] dot(), min(), max()
//! - [ConstructorOp] Construction of new vectors from arbitrary inputs {float,uint,int}{2,3,4}
//!
//!
//! Not planned for now:
//! - Binary/numeric assignment operators +=, -=, *=, /=, %=, <<=, >>=, &=, |=, ^=
//! - Booleans &&, ||, ?:, including unary operator !
//! - Array [i]
//! - Comma ,
//! - Prefix/Postfix ++, --
//! - Structure .
//!
//!
//! ## Usage Plan
//! Virtual machines generate [crate::Outcome]s and [crate::VMDataRef]s in terms of [HLSLKind]s and [HLSLOperator]s.
//!
//! Virtual machines may have more information in terms of types, e.g. AMDIL has separate add(float), add(uint), add(int) instructions
//! while [ArithmeticOp::Plus] allows float|uint|int. Analysis machines (e.g. [VariableAbstractMachine]) should be aware of this.

use std::cmp::max;

use crate::abstract_machine::instructions::SimpleDependencyRelation;

use super::kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind, HLSLOperandKind};

/// Trait implemented for HLSL operators and intrinsic functions which take 1..N inputs and output 1 value.
///
/// Used to determine what data types are accepted as inputs and outputs.
pub trait Operator: std::fmt::Debug {
    /// Return the "type-spec" for the operator - the set of input types and output type.
    /// These are not concretized, and may involve type holes.
    fn get_kindspec(&self) -> OperatorKindspec;

    /// Return the number of input arguments the operator takes.
    fn n_inputs(&self) -> usize;

    /// Return the dependency relation - how the input arguments are connected to the output
    ///
    /// Mapping of (input data ref) -> (output data refs affected by input data ref)
    fn dep_rel(&self) -> SimpleDependencyRelation;
}

/// The "kindspec" for an operator - the input and output types that it accepts, and the accepted types for any referenced type holes.
#[derive(Debug)]
pub struct OperatorKindspec {
    /// [inputs..., output]
    operand_types: Vec<HLSLOperandKind>,
    holes: Vec<HLSLKind>,
}
impl OperatorKindspec {
    /// Creates a new OperatorKindspec while checking the number of holes is correct
    fn new(
        input_types: Vec<HLSLOperandKind>,
        output_type: HLSLOperandKind,
        holes: Vec<HLSLKind>,
    ) -> Self {
        let mut operand_types = input_types;
        operand_types.push(output_type);

        // Sanity check - make sure the maximum hole ID referenced in any HLSLOperandKind::Hole(id) == the number of elements in `holes`
        let mut max_referenced_hole = None;
        for operand in operand_types.iter() {
            if let HLSLOperandKind::Hole(id) = operand {
                max_referenced_hole =
                    Some(max_referenced_hole.map_or(*id, |max_hole_id| max(max_hole_id, *id)));
            }
        }

        if let Some(max_referenced_hole) = max_referenced_hole {
            if max_referenced_hole != holes.len() - 1 {
                panic!("Mismatch in referenced holes - maximum referenced hole = {}, but {} are present: {:?}", max_referenced_hole, holes.len(), holes);
            }
        }

        OperatorKindspec {
            operand_types,
            holes,
        }
    }

    /// Return a vector of HLSLKind masks corresponding to the input arguments.
    /// Does not consider the actual types of the other arguments or do any type coercion logic.
    pub fn get_basic_input_types(&self) -> Vec<HLSLKind> {
        self.input_types()
            .iter()
            .map(|t| -> HLSLKind {
                match t {
                    HLSLOperandKind::Concrete(c) => (*c).into(),
                    HLSLOperandKind::Hole(h) => self.holes[*h],
                }
            })
            .collect()
    }

    /// [get_basic_input_types] but for the output type.
    pub fn get_basic_output_type(&self) -> HLSLKind {
        match self.output_type() {
            HLSLOperandKind::Concrete(c) => (*c).into(),
            HLSLOperandKind::Hole(h) => self.holes[*h],
        }
    }

    /// Return a vector of reverse type mappings.
    ///
    /// Each element in the returned vector is either
    /// - (concrete HLSLKind, [operand index]) or
    /// - (hole HLSLKind, [operands associated with hole])
    ///
    /// The second type implies all of the operands should have the same hole type.
    pub fn get_type_constraints(&self) -> Vec<(HLSLKind, Vec<usize>)> {
        // Create hole mappings at the same indices HLSLOperandKind::Hole will refer to
        let mut mappings: Vec<_> = self.holes.iter().map(|t| (*t, vec![])).collect();
        for (i, t) in self.operand_types.iter().enumerate() {
            match t {
                HLSLOperandKind::Hole(h) => mappings[*h].1.push(i),
                HLSLOperandKind::Concrete(c) => mappings.push(((*c).into(), vec![i])),
            }
        }

        mappings
    }

    pub fn input_types(&self) -> &[HLSLOperandKind] {
        &self.operand_types[0..(self.operand_types.len() - 1)]
    }
    pub fn output_type(&self) -> &HLSLOperandKind {
        self.operand_types.last().unwrap()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HLSLOperator {
    Assign,
    Unary(UnaryOp),
    Arithmetic(ArithmeticOp),
    BinaryArithmetic(BinaryArithmeticOp),
    NumericCast(NumericCastTo),
    SampleI(SampleIntrinsic),
    NumericI(NumericIntrinsic),
    FauxBoolean(FauxBooleanOp),
    Constructor(ConstructorOp),
}
impl Operator for HLSLOperator {
    fn get_kindspec(&self) -> OperatorKindspec {
        match self {
            HLSLOperator::Assign => OperatorKindspec::new(
                vec![HLSLOperandKind::Hole(0)],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::all().into()],
            ),
            HLSLOperator::Unary(x) => x.get_kindspec(),
            HLSLOperator::Arithmetic(x) => x.get_kindspec(),
            HLSLOperator::BinaryArithmetic(x) => x.get_kindspec(),
            HLSLOperator::NumericCast(x) => x.get_kindspec(),
            HLSLOperator::SampleI(x) => x.get_kindspec(),
            HLSLOperator::NumericI(x) => x.get_kindspec(),
            HLSLOperator::FauxBoolean(x) => x.get_kindspec(),
            HLSLOperator::Constructor(x) => x.get_kindspec(),
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
            HLSLOperator::Constructor(x) => x.n_inputs(),
        }
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        match self {
            HLSLOperator::Assign => SimpleDependencyRelation::PerComponent,
            HLSLOperator::Unary(x) => x.dep_rel(),
            HLSLOperator::Arithmetic(x) => x.dep_rel(),
            HLSLOperator::BinaryArithmetic(x) => x.dep_rel(),
            HLSLOperator::NumericCast(x) => x.dep_rel(),
            HLSLOperator::SampleI(x) => x.dep_rel(),
            HLSLOperator::NumericI(x) => x.dep_rel(),
            HLSLOperator::FauxBoolean(x) => x.dep_rel(),
            HLSLOperator::Constructor(x) => x.dep_rel(),
        }
    }
}

/// Unary operations that operate on a single value and return a single value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    fn get_kindspec(&self) -> OperatorKindspec {
        let hole = match self {
            UnaryOp::BinaryNot => HLSLKindBitmask::INTEGER,
            UnaryOp::Negate => HLSLKindBitmask::NUMERIC_FLOAT | HLSLKindBitmask::NUMERIC_SINT,
            UnaryOp::Plus => HLSLKindBitmask::NUMERIC,
        };

        OperatorKindspec::new(
            vec![HLSLOperandKind::Hole(0)],
            HLSLOperandKind::Hole(0),
            vec![hole.into()],
        )
    }

    fn n_inputs(&self) -> usize {
        1
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        SimpleDependencyRelation::PerComponent
    }
}

/// Integer/float arithmetic operations, which take two inputs X and Y and return one output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    fn get_kindspec(&self) -> OperatorKindspec {
        // All of these operators are compatible with float/int/uint i.e. NUMERIC
        OperatorKindspec::new(
            vec![HLSLOperandKind::Hole(0), HLSLOperandKind::Hole(0)],
            HLSLOperandKind::Hole(0),
            vec![HLSLKindBitmask::NUMERIC.into()],
        )
    }

    fn n_inputs(&self) -> usize {
        2
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        SimpleDependencyRelation::PerComponent
    }
}

/// Binary/bitwise operations which take two inputs X and Y and return a single output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryArithmeticOp {
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}
impl Operator for BinaryArithmeticOp {
    fn get_kindspec(&self) -> OperatorKindspec {
        let is_shift = match self {
            BinaryArithmeticOp::LeftShift => true,
            BinaryArithmeticOp::RightShift => true,
            BinaryArithmeticOp::BitwiseAnd => false,
            BinaryArithmeticOp::BitwiseOr => false,
            BinaryArithmeticOp::BitwiseXor => false,
        };

        if !is_shift {
            OperatorKindspec::new(
                vec![HLSLOperandKind::Hole(0), HLSLOperandKind::Hole(0)],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::INTEGER.into()],
            )
        } else {
            // Left input = the thing getting modified
            // Right input = the shift amount - must be unsigned int?
            OperatorKindspec::new(
                vec![
                    HLSLOperandKind::Hole(0),
                    HLSLNumericKind::UnsignedInt.into(),
                ],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::INTEGER.into()],
            )
        }
    }

    fn n_inputs(&self) -> usize {
        2
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        SimpleDependencyRelation::PerComponent
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NumericCastTo(pub HLSLNumericKind);
impl Operator for NumericCastTo {
    fn get_kindspec(&self) -> OperatorKindspec {
        OperatorKindspec::new(
            vec![HLSLOperandKind::Hole(0)],
            self.0.into(),
            vec![HLSLKindBitmask::NUMERIC.into()],
        )
    }

    fn n_inputs(&self) -> usize {
        1
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        SimpleDependencyRelation::PerComponent
    }
}

/// Texture sampling intrinsic functions, which take a texture argument and at least one other input to produce a single output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SampleIntrinsic {
    Tex2D,
}
impl Operator for SampleIntrinsic {
    fn get_kindspec(&self) -> OperatorKindspec {
        match self {
            Self::Tex2D => OperatorKindspec::new(
                vec![
                    HLSLConcreteKind::Texture2D.into(),
                    HLSLNumericKind::Float.into(),
                ],
                HLSLNumericKind::Float.into(),
                vec![],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Tex2D => 2,
        }
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        SimpleDependencyRelation::AllToAll
    }
}

/// Numeric intrinsic functions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericIntrinsic {
    Dot,
    Min,
    Max,
}
impl Operator for NumericIntrinsic {
    fn get_kindspec(&self) -> OperatorKindspec {
        match self {
            // TODO apparently not valid for uint? need to check further
            Self::Min | Self::Max | Self::Dot => OperatorKindspec::new(
                vec![HLSLOperandKind::Hole(0), HLSLOperandKind::Hole(0)],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::NUMERIC.into()],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Min | Self::Max | Self::Dot => 2,
        }
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        // v3 = max(v1, v2)
        // implies v3.x = max(v1.x, v2.x) etc
        match self {
            Self::Min | Self::Max => SimpleDependencyRelation::PerComponent,
            Self::Dot => SimpleDependencyRelation::AllToAll,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FauxBooleanOp {
    Lt,
    Le,
    Gt,
    Ge,
    Ternary,
}
impl Operator for FauxBooleanOp {
    fn get_kindspec(&self) -> OperatorKindspec {
        match self {
            Self::Lt | Self::Le | Self::Gt | Self::Ge => OperatorKindspec::new(
                vec![HLSLOperandKind::Hole(0), HLSLOperandKind::Hole(0)],
                HLSLOperandKind::Hole(1),
                vec![
                    HLSLKindBitmask::NUMERIC.into(),
                    HLSLKindBitmask::INTEGER.into(),
                ],
            ),
            Self::Ternary => OperatorKindspec::new(
                vec![
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(1),
                    HLSLOperandKind::Hole(1),
                ],
                HLSLOperandKind::Hole(1),
                vec![
                    HLSLKindBitmask::INTEGER.into(),
                    HLSLKindBitmask::NUMERIC.into(),
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

    fn dep_rel(&self) -> SimpleDependencyRelation {
        match self {
            // TODO something better
            Self::Ternary => SimpleDependencyRelation::AllToAll,
            _ => SimpleDependencyRelation::PerComponent,
        }
    }
}

/// For constructing new vectors from old scalars
/// e.g. Vec2 => float2(a.x, b.y)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstructorOp {
    Vec2,
    Vec3,
    Vec4,
}
impl Operator for ConstructorOp {
    fn get_kindspec(&self) -> OperatorKindspec {
        match self {
            // TODO force all inputs to be scalar
            Self::Vec2 => OperatorKindspec::new(
                vec![HLSLOperandKind::Hole(0), HLSLOperandKind::Hole(0)],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::NUMERIC.into()],
            ),
            Self::Vec3 => OperatorKindspec::new(
                vec![
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(0),
                ],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::NUMERIC.into()],
            ),
            Self::Vec4 => OperatorKindspec::new(
                vec![
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(0),
                    HLSLOperandKind::Hole(0),
                ],
                HLSLOperandKind::Hole(0),
                vec![HLSLKindBitmask::NUMERIC.into()],
            ),
        }
    }

    fn n_inputs(&self) -> usize {
        match self {
            Self::Vec2 => 2,
            Self::Vec3 => 3,
            Self::Vec4 => 4,
        }
    }

    fn dep_rel(&self) -> SimpleDependencyRelation {
        // TODO need something better than this
        SimpleDependencyRelation::AllToAll
    }
}

pub trait UnconcreteOpTarget: std::fmt::Debug {}

/// Data on an operation performed on a set of inputs producing an output, each of which may not have a concrete type.
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
    /// The name of the output value
    pub output: TData,
}
impl<TData: UnconcreteOpTarget> UnconcreteOpResult<TData> {
    pub fn new(op: HLSLOperator, inputs: Vec<TData>, output: TData) -> Self {
        if op.n_inputs() != inputs.len() {
            panic!(
                "Op {:?} takes {} inputs but UnconcreteOpResult got {} ({:?})",
                op,
                op.n_inputs(),
                inputs.len(),
                inputs
            );
        }
        Self { op, inputs, output }
    }
}
