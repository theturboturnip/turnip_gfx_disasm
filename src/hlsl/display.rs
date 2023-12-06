use std::fmt::{Display, Formatter};

use crate::abstract_machine::{vector::VectorComponent, VMName, VMVector};

use super::{
    kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind},
    vm::HLSLAction, HLSLSingleVectorName, HLSLScalarName, HLSLVector,
};

pub struct DWrap<T>(pub T);

impl std::fmt::Display for HLSLSingleVectorName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO have a formatter that takes type into account
        match self {
            Self::Texture(id) => write!(f, "tex{:0>3}", id),
            Self::GenericRegister(id) => write!(f, "var{:0>3}", id),
            Self::Literal(data) => write!(
                f,
                "(0x{:x}, 0x{:x}, 0x{:x}, 0x{:x})",
                data[0], data[1], data[2], data[3]
            ),
            Self::ShaderInput(name) | Self::ShaderOutput(name) => write!(f, "{}", name),
            Self::ArrayElement { of: elem, idx } => write!(f, "{}[{}]", elem, idx),
        }
    }
}

impl std::fmt::Display for DWrap<&HLSLScalarName> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = &self.0.vec;
        let c = self.0.comp;
        let v_kind = v.toplevel_kind();
        if let HLSLSingleVectorName::Literal(vals) = v {
            let val: u64 = vals[c.into_index()];
            match v_kind.mask() {
                HLSLKindBitmask::NUMERIC_FLOAT => write!(f, "{:?}f", f32::from_bits(val as u32)),
                HLSLKindBitmask::NUMERIC_SINT => write!(f, "{}", val),
                HLSLKindBitmask::NUMERIC_UINT => write!(f, "{}u", val),
                HLSLKindBitmask::NUMERIC => write!(f, "(num?)0x{:x}", val),
                HLSLKindBitmask::INTEGER => write!(f, "(u?int)0x{:x}", val),
                _ => write!(f, "({})0x{:x}", v_kind, val),
            }
        } else {
            write!(f, "({}){}.", v_kind, v)?;
            match c {
                VectorComponent::X => write!(f, "x"),
                VectorComponent::Y => write!(f, "y"),
                VectorComponent::Z => write!(f, "z"),
                VectorComponent::W => write!(f, "w"),
            }
        }
    }
}

impl std::fmt::Display for DWrap<&(HLSLVector, HLSLKind)> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // let (v, cs) = &self.0;
        // write!(f, "{}{}", v.vector_name, cs)
        let ts = &self.0.0.ts;
        let common_origin = &ts[0].vec;
        if ts.iter().all(|t| &t.vec == common_origin) {
            write!(f, "{common_origin}.")?;
            for t in ts.iter() {
                write!(f, "{}", t.comp)?;
            }
            Ok(())
        } else {
            write!(f, "{}{}(", self.0.1, self.0.0.n_components())?;
            for t in ts.iter() {
                // TODO correct comma joining
                write!(f, "{}.{}, ", t.vec, t.comp)?;
            }
            write!(f, ")")
        }
    }
}

impl std::fmt::Display for HLSLAction {
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
            Self::EarlyOut { inputs } => {
                write!(f, "early_out_based_on(")?;
                for i in inputs {
                    write!(f, "{}, ", DWrap(i))?;
                }
                write!(f, ");")
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