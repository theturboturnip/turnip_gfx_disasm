use std::fmt::{Display, Formatter};

use crate::{abstract_machine::{vector::VectorComponent, VMVector}, Action};

use super::{
    kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind}, HLSLRegister, HLSLScalar, HLSLVector, vm::HLSLAbstractVM,
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

impl std::fmt::Display for DWrap<(&HLSLScalar, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let kind = self.0.1;
        match &self.0.0 {
            HLSLScalar::Component(reg, comp) => {
                write!(f, "({}){}.", kind, reg)?;
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
                    _ => write!(f, "({})0x{:x}", kind, *val),
                }
            },
        }
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

impl std::fmt::Display for Action<HLSLAbstractVM> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Assign {
                op, output, inputs, ..
            } => {
                {
                    write!(f, "{} = {:?}(", DWrap(output), op)?;
                }
                for i in inputs {
                    write!(f, "{}, ", DWrap(i))?;
                }
                write!(f, ");")
            }
            Self::EarlyOut => {
                write!(f, "discard;")
            }
            Self::If { inputs, cond_operator, if_true, if_fals } => {
                write!(f, "if ({:?}(", cond_operator)?;
                for i in inputs {
                    write!(f, "{}, ", DWrap(i))?;
                }
                write!(f, ")) {{\n")?;
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