use std::fmt::{Display, Formatter};

use super::{
    types::{HLSLConcreteType, HLSLNumericType, HLSLType},
    HLSLOutcome, HLSLVectorName,
};

impl std::fmt::Display for HLSLVectorName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // TODO have a formatter that takes type into account
        match self {
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

impl std::fmt::Display for HLSLOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Declaration { new_var } => {
                let var = new_var.borrow();
                write!(f, "{:?}{} {};", var.kind, var.n_components, var.vector_name)
            }
            Self::Definition {
                new_var,
                components,
            } => {
                {
                    let var = new_var.borrow();
                    write!(
                        f,
                        "{:?}{} {} = {:?}{}(",
                        var.kind, var.n_components, var.vector_name, var.kind, var.n_components
                    )?;
                }
                for (refed_var, comp) in components {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}.{:?}, ", referenced_var.vector_name, *comp)?;
                }
                write!(f, ");")
            }
            Self::Operation {
                output_dataref, op, ..
            } => {
                {
                    let output_var = output_dataref.0.borrow();
                    write!(
                        f,
                        "{}{} = {:?}(",
                        output_var.vector_name, output_dataref.1, op.op
                    )?;
                }
                for (refed_var, swizz) in &op.inputs {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}{}, ", referenced_var.vector_name, swizz)?;
                }
                write!(f, ");")
            }
            Self::EarlyOut { inputs } => {
                write!(f, "early_out_based_on(")?;
                for (refed_var, comp) in inputs {
                    let referenced_var = refed_var.borrow();
                    write!(f, "{}.{:?}, ", referenced_var.vector_name, *comp)?;
                }
                write!(f, ");")
            }
        }
    }
}

impl Display for HLSLNumericType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::UnsignedInt => write!(f, "uint"),
            Self::SignedInt => write!(f, "int"),
        }
    }
}
impl Display for HLSLConcreteType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Numeric(n) => write!(f, "{}", n),
            Self::Texture2D => write!(f, "texture2D"),
        }
    }
}
impl Display for HLSLType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Self::Concrete(c) => write!(f, "{}", c),
            Self::Hole(mask) => write!(f, "{:?}", mask),
        }
    }
}
