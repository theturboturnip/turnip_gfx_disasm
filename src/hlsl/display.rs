use std::fmt::{Display, Formatter};

use crate::{abstract_machine::{vector::{VectorComponent, VectorOf}, VMVector, VMName}, Action};

use super::{
    kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind}, HLSLRegister, HLSLScalar, HLSLVector, vm::HLSLAbstractVM, HLSLAction, syntax::HLSLOperator,
};

pub struct DWrap<T>(pub T);

impl std::fmt::Display for HLSLRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO have a formatter that takes type into account
        match self {
            Self::Texture(id) => write!(f, "tex{:0>3}", id),
            Self::GenericRegister(id, _) => write!(f, "var{:0>3}", id),
            Self::ShaderInput(name, _) | Self::ShaderOutput(name, _) => write!(f, "{}", name),
            Self::ArrayElement { of: elem, idx } => write!(f, "{}[{}]", elem, idx),
        }
    }
}
fn display_scalar_kind(f: &mut Formatter<'_>, scalar: &HLSLScalar, kind: HLSLKind) -> std::fmt::Result {
    match scalar {
        HLSLScalar::Component(reg, comp) => {
            let minimum_kind = kind.intersection(reg.toplevel_kind());
            if minimum_kind.is_none() {
                write!(f, "({})", kind)?;
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
            match kind.mask() {
                HLSLKindBitmask::NUMERIC_FLOAT => write!(f, "{:?}f", f32::from_bits(*val)),
                HLSLKindBitmask::NUMERIC_SINT => write!(f, "{}", *val),
                HLSLKindBitmask::NUMERIC_UINT => write!(f, "{}u", *val),
                HLSLKindBitmask::NUMERIC => write!(f, "(num?)0x{:x}", *val),
                HLSLKindBitmask::INTEGER => write!(f, "(u?int)0x{:x}", *val),
                HLSLKindBitmask::ALL => write!(f, "(any)0x{:x}", *val),
                _ => write!(f, "({})0x{:x}", kind, *val),
            }
        },
    }
}
impl std::fmt::Display for DWrap<(&HLSLScalar, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_scalar_kind(f, self.0.0, self.0.1)
    }
}
impl std::fmt::Display for DWrap<&(HLSLScalar, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_scalar_kind(f, &self.0.0, self.0.1)
    }
}

impl std::fmt::Display for DWrap<&(HLSLVector, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // let (v, cs) = &self.0;
        // write!(f, "{}{}", v.vector_name, cs)
        let ts = &self.0.0.ts;
        let kind = self.0.1;
        let common_origin = match &ts[0] {
            HLSLScalar::Component(reg, _) => {
                if ts.iter().all(|t| match t {
                    HLSLScalar::Component(other_reg, _) => other_reg == reg,
                    HLSLScalar::Literal(_) => false,
                }) {
                    Some(reg)
                } else {
                    None
                }
            },
            HLSLScalar::Literal(_) => None,
        };
        // If all the scalars are components of the same register
        match common_origin {
            Some(common_origin) => {
                write!(f, "{common_origin}.")?;
                for t in ts.iter() {
                    if let HLSLScalar::Component(_, c) = t {
                        write!(f, "{}", c)?;
                    } else {
                        unreachable!("common_origin can only be Some if every t in ts is a HLSLScalar::Component")
                    }
                }
                Ok(())
            },
            None => {
                write!(f, "{}{}(", self.0.1, self.0.0.n_components())?;
                for t in ts.iter() {
                    // TODO correct comma joining
                    write!(f, "{}, ", DWrap((t, kind)))?;
                }
                write!(f, ")")
            }
        }
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
            HLSLOperator::NumericCast(_) => todo!(),
            HLSLOperator::SampleI(samp) => {
                match samp {
                    crate::hlsl::syntax::SampleIntrinsic::Tex2D => {
                        assert_eq!(inputs.len(), 2);
                        write!(f, "{}.Sample(NO_SAMPLER, {})", DWrap(&inputs[0]), DWrap(&inputs[1]))
                    }
                }
            },
            HLSLOperator::NumericI(intr) => {
                assert_eq!(inputs.len(), 2);
                match intr {
                    super::syntax::NumericIntrinsic::Dot => write!(f, "dot({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    super::syntax::NumericIntrinsic::Min => write!(f, "min({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1])),
                    super::syntax::NumericIntrinsic::Max => write!(f, "max({}, {})", DWrap(&inputs[0]), DWrap(&inputs[1])),
                }
            },
            HLSLOperator::FauxBoolean(b) => {
                match b {
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
            HLSLOperator::Constructor(_) => todo!(),
        }
    }
}
impl std::fmt::Display for HLSLAction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Assign {
                op, output, inputs, ..
            } => {
                write!(f, "{}{} {} = {};", output.1, output.0.n_components(), DWrap(output), DWrap((op, inputs)))
                // for i in inputs {
                //     write!(f, "{}, ", DWrap(i))?;
                // }
                // write!(f, ");")
            }
            Self::EarlyOut => {
                write!(f, "discard;")
            }
            Self::If { inputs, cond_operator, if_true, if_fals } => {
                write!(f, "if ({}) {{\n", DWrap((cond_operator, inputs)))?;
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