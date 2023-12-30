use std::fmt::{Display, Formatter};

use crate::abstract_machine::{vector::VectorComponent, VMVector, VMName, expr::{HLSLScalar, HLSLVector, Vector, ContigSwizzle, Reg}};

use super::{
    kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind}, HLSLRegister, HLSLAction, syntax::HLSLOperator,
};

pub struct DWrap<T>(pub T);

impl std::fmt::Display for HLSLRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO have a formatter that takes type into account
        match self {
            Self::TextureCube(id) => write!(f, "texCube{:0>3}", id),
            Self::Texture2D(id) => write!(f, "tex2D{:0>3}", id),
            Self::Texture3D(id) => write!(f, "tex3D{:0>3}", id),
            Self::GenericRegister(id, _) => write!(f, "var_{}", id),
            Self::ShaderInput(name, _) | Self::ShaderOutput(name, _) => write!(f, "{}", name),
            Self::ArrayElement { of: elem, idx } => write!(f, "{}[{}]", elem, idx),
        }
    }
}
fn display_scalar_kind(f: &mut Formatter<'_>, scalar: &HLSLScalar, usage_kind: &HLSLKind) -> std::fmt::Result {
    match scalar {
        HLSLScalar::Expr { op, inputs, output_kind } => {
            // TODO add cast if output_kind != usage_kind
            write!(f, "{}", DWrap((op, inputs)))
        },
        HLSLScalar::Component(reg, comp) => {
            let minimum_kind = usage_kind.intersection(reg.toplevel_kind());
            if minimum_kind.is_none() {
                write!(f, "({})", usage_kind)?;
            }
            write!(f, "{}.", reg)?;
            match comp {
                VectorComponent::X => write!(f, "x"),
                VectorComponent::Y => write!(f, "y"),
                VectorComponent::Z => write!(f, "z"),
                VectorComponent::W => write!(f, "w"),
            }
        }
        HLSLScalar::Literal(val) => {
            match usage_kind.mask() {
                HLSLKindBitmask::NUMERIC_FLOAT => write!(f, "{:?}f", f32::from_bits(*val as u32)),
                HLSLKindBitmask::NUMERIC_SINT => write!(f, "{}", *val),
                HLSLKindBitmask::NUMERIC_UINT => write!(f, "{}u", *val),
                HLSLKindBitmask::NUMERIC => write!(f, "(num?)0x{:x}", *val),
                HLSLKindBitmask::INTEGER => write!(f, "(u?int)0x{:x}", *val),
                HLSLKindBitmask::ALL => write!(f, "(any)0x{:x}", *val),
                _ => write!(f, "({})0x{:x}", usage_kind, *val),
            }
        },
    }
}
impl std::fmt::Display for DWrap<(&HLSLScalar, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_scalar_kind(f, self.0.0, &self.0.1)
    }
}
impl std::fmt::Display for DWrap<&(HLSLScalar, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_scalar_kind(f, &self.0.0, &self.0.1)
    }
}
// TODO take usage_kind and cast if necessary?
fn display_vector_pure_swizzle(f: &mut Formatter<'_>, reg: &HLSLRegister, comps: &ContigSwizzle) -> std::fmt::Result {
    write!(f, "{reg}.")?;
    for c in comps.iter() {
        write!(f, "{}", c)?;
    }
    Ok(())
}
fn display_vector_kind(f: &mut Formatter<'_>, v: &HLSLVector, usage_kind: &HLSLKind) -> std::fmt::Result {
    // TODO cast things if usage_kind != output_kind
    match v {
        Vector::Construction(scalars, output_kind) => if scalars.len() == 1 {
            write!(f, "{}", DWrap((&scalars[0], *output_kind)))
        } else {
            write!(f, "{}{}(", output_kind, v.n_components())?;
            let mut first = true;
            for scalar in scalars.iter() {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{}", DWrap((scalar, *output_kind)))?;
                first = false;
            }
            write!(f, ")")
        },
        Vector::PureSwizzle(reg, comps, _output_kind) => {
            display_vector_pure_swizzle(f, reg, comps)
        },
        // TODO take output_kind and cast if necessary?
        Vector::Expr { op, inputs, ..} => {
            let d = DWrap((op, inputs));
            write!(f, "({})", d)
        },
    }
}
impl std::fmt::Display for DWrap<&(HLSLVector, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_vector_kind(f, &self.0.0, &self.0.1)
    }
}
impl std::fmt::Display for DWrap<(&HLSLVector, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_vector_kind(f, &self.0.0, &self.0.1)
    }
}
impl<'a, T: 'a> std::fmt::Display for DWrap<(&'a HLSLOperator, &'a Vec<T>)> where DWrap<&'a T>: std::fmt::Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (op, inputs) = self.0;

        match op {
            HLSLOperator::Assign => {
                assert_eq!(inputs.len(), 1);
                write!(f, "{}", DWrap(&inputs[0]))
            },
            HLSLOperator::Unary(unary) => {
                assert_eq!(inputs.len(), 1);
                match unary {
                    super::syntax::UnaryOp::BinaryNot => write!(f, "!{}", DWrap(&inputs[0])),
                    super::syntax::UnaryOp::Negate => write!(f, "-{}", DWrap(&inputs[0])),
                    super::syntax::UnaryOp::Plus => write!(f, "+{}", DWrap(&inputs[0])),
                }
            },
            HLSLOperator::Arithmetic(arith) => {
                assert_eq!(inputs.len(), 2);
                match arith {
                    crate::hlsl::syntax::ArithmeticOp::Plus => write!(f, "{} + {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::ArithmeticOp::Minus => write!(f, "{} - {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::ArithmeticOp::Times => write!(f, "{} * {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::ArithmeticOp::Div => write!(f, "{} / {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::ArithmeticOp::Mod => write!(f, "{} % {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                }
            },
            HLSLOperator::BinaryArithmetic(arith) => {
                assert_eq!(inputs.len(), 2);
                match arith {
                    crate::hlsl::syntax::BinaryArithmeticOp::LeftShift => write!(f, "{} << {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::BinaryArithmeticOp::RightShift => write!(f, "{} >> {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::BinaryArithmeticOp::BitwiseAnd => write!(f, "{} & {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::BinaryArithmeticOp::BitwiseOr => write!(f, "{} | {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    crate::hlsl::syntax::BinaryArithmeticOp::BitwiseXor => write!(f, "{} ^ {}", DWrap(&inputs[0]), DWrap(&inputs[1])),
                }
            },
            HLSLOperator::NumericCast(crate::hlsl::syntax::NumericCastTo(target)) => {
                assert_eq!(inputs.len(), 1);
                write!(f, "({}){}", target, DWrap(&inputs[0]))
            },
            HLSLOperator::SampleI(samp) => {
                match samp {
                    crate::hlsl::syntax::SampleIntrinsic::Tex2D => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{}.Sample(NO_SAMPLER, {})", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    }
                }
            },
            HLSLOperator::NumericI(intr) => {
                match intr {
                    super::syntax::NumericIntrinsic::Dot => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "dot({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    super::syntax::NumericIntrinsic::Min => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "min({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    super::syntax::NumericIntrinsic::Max => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "max({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    super::syntax::NumericIntrinsic::Mad => {
                        assert_eq!(inputs.len(), 3);
                        write!(f, "mad({}, {}, {})", DWrap(&inputs[0]), DWrap(&inputs[1]), DWrap(&inputs[2]))
                    },
                    super::syntax::NumericIntrinsic::Sqrt => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "sqrt({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Rsqrt => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "rsqrt({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Exp => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "exp({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Exp2 => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "exp2({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Saturate => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "saturate({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Abs => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "abs({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Log => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "log({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Log10 => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "log10({})", DWrap(&inputs[0]))
                    },
                    super::syntax::NumericIntrinsic::Log2 => {
                        assert_eq!(inputs.len(), 1);
                        write!(f, "log2({})", DWrap(&inputs[0]))
                    },
                }
            },
            HLSLOperator::FauxBoolean(b) => {
                match b {
                    crate::hlsl::syntax::FauxBooleanOp::Eq => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} == {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Ne => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} != {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Lt => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} < {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Le => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} <= {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Gt => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} > {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Ge => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{} >= {}", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    },
                    crate::hlsl::syntax::FauxBooleanOp::Ternary => {
                        assert_eq!(inputs.len(), 3);
                        write!(f, "{} ? {} : {}", DWrap(&inputs[0]), DWrap(&inputs[1]), DWrap(&inputs[2]))
                    },
                }
            },
            // HLSLOperator::Constructor(_) => todo!(),
        }
    }
}
impl std::fmt::Display for HLSLAction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Assign {
                expr, output
            } => {
                write!(f, "{}{} ", expr.usage_kind(), output.0.n_components())?;
                display_vector_pure_swizzle(f, &output.0, &output.1)?;
                write!(f, " = {};", DWrap((expr, output.0.output_kind())))
            }
            Self::EarlyOut => {
                write!(f, "discard;")
            }
            Self::If { expr, if_true, if_fals } => {
                write!(f, "if ({}) {{\n", DWrap((expr, HLSLKind::INTEGER)))?;
                for i in if_true {
                    write!(f, "\t{}\n", i)?;
                }
                write!(f, "}} else {{")?;
                for i in if_fals {
                    write!(f, "\t{}\n", i)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for HLSLNumericKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::UnsignedInt => write!(f, "uint"),
            Self::SignedInt => write!(f, "int"),
        }
    }
}
impl Display for HLSLConcreteKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Numeric(n) => write!(f, "{}", n),
            Self::Texture2D => write!(f, "texture2D"),
            Self::Texture3D => write!(f, "texture3D"),
            Self::TextureCube => write!(f, "textureCube"),
        }
    }
}
impl Display for HLSLKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.mask() {
            HLSLKindBitmask::NUMERIC_FLOAT => write!(f, "float"),
            HLSLKindBitmask::NUMERIC_SINT => write!(f, "int"),
            HLSLKindBitmask::NUMERIC_UINT => write!(f, "uint"),
            HLSLKindBitmask::NUMERIC => write!(f, "num?"),
            HLSLKindBitmask::INTEGER => write!(f, "u?int"),
            HLSLKindBitmask::ALL => write!(f, "any"),
            _ => write!(f, "{:?}", self),
        }
    }
}
impl Display for VectorComponent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VectorComponent::X => write!(f, "x"),
            VectorComponent::Y => write!(f, "y"),
            VectorComponent::Z => write!(f, "z"),
            VectorComponent::W => write!(f, "w"),
        }
    }
}