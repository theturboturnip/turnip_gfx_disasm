use std::fmt::{Debug, Display, Formatter, Result};

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

pub enum DebugVec<'a, T: Debug> {
    Sep { vec: &'a Vec<T>, sep: &'a str },
    Prefix { vec: &'a Vec<T>, prefix: &'a str },
}
impl<'a, T: Debug> Display for DebugVec<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "[")?;
        match self {
            Self::Sep { vec, sep } => {
                let mut first = true;
                for t in *vec {
                    if first {
                        write!(f, "{:?}", t)?;
                        first = false;
                    } else {
                        write!(f, "{}{:?}", sep, t)?;
                    }
                }
                write!(f, "]")
            }
            Self::Prefix { vec, prefix } => {
                for t in *vec {
                    write!(f, "{}{:?}", prefix, t)?;
                }
                write!(f, "{}]", prefix)
            }
        }
    }
}
