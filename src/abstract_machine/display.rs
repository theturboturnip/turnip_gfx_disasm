use std::fmt::{Display, Formatter, Result};

use crate::{DataKind, DataWidth, TypedVMRef, VMRef};

impl Display for DataKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::UnsignedInt => write!(f, "uint"),
            Self::SignedInt => write!(f, "int"),
            Self::Hole => write!(f, "???"),
            Self::Untyped => write!(f, "Untyped"),
        }
    }
}
impl<TData> Display for TypedVMRef<TData>
where
    TData: VMRef + Display,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{} as ", self.data)?;
        write!(f, "{}", self.kind)?;
        match self.width {
            DataWidth::E8 => write!(f, " (8bit)"),
            DataWidth::E16 => write!(f, " (16bit)"),
            DataWidth::E32 => write!(f, " (32bit)"),
            DataWidth::E64 => write!(f, " (64bit)"),
        }
    }
}

pub enum DisplayVec<'a, T: Display> {
    Sep { vec: &'a Vec<T>, sep: &'a str },
    Prefix { vec: &'a Vec<T>, prefix: &'a str },
}
impl<'a, T: Display> Display for DisplayVec<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "[")?;
        match self {
            Self::Sep { vec, sep } => {
                let mut first = true;
                for t in *vec {
                    if first {
                        write!(f, "{}", t)?;
                        first = false;
                    } else {
                        write!(f, "{}{}", sep, t)?;
                    }
                }
                write!(f, "]")
            }
            Self::Prefix { vec, prefix } => {
                for t in *vec {
                    write!(f, "{}{}", prefix, t)?;
                }
                write!(f, "{}]", prefix)
            }
        }
    }
}
