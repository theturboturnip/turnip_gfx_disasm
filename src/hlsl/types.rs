use bitflags::bitflags;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HLSLNumericType {
    Float,
    UnsignedInt,
    SignedInt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HLSLConcreteType {
    Numeric(HLSLNumericType),
    Texture2D,
}
impl From<HLSLNumericType> for HLSLConcreteType {
    fn from(num: HLSLNumericType) -> Self {
        Self::Numeric(num)
    }
}

/// A type for HLSL values.
/// Equivalent to [HLSLOperandType] but holds a hole mask instead of a hold ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HLSLType {
    Concrete(HLSLConcreteType),
    Hole(HLSLHoleTypeMask),
}
impl From<HLSLConcreteType> for HLSLType {
    fn from(c: HLSLConcreteType) -> Self {
        Self::Concrete(c)
    }
}
impl From<HLSLNumericType> for HLSLType {
    fn from(num: HLSLNumericType) -> Self {
        Self::Concrete(num.into())
    }
}
impl From<HLSLHoleTypeMask> for HLSLType {
    fn from(mask: HLSLHoleTypeMask) -> Self {
        Self::Hole(mask)
    }
}
impl HLSLType {
    pub fn encompasses(&self, other: &HLSLType) -> bool {
        match self {
            // If we're concrete, the other type has to match exactly
            Self::Concrete(_) => self == other,
            // If we're a hole of a specific type, our type must be a superset or equal to the other type
            Self::Hole(our_mask) => {
                let their_mask: HLSLHoleTypeMask = match other {
                    Self::Concrete(c) => {
                        // Convert their type into a mask which just holds that type
                        (*c).into()
                    }
                    Self::Hole(their_mask) => {
                        // Get the mask for that hole directly
                        *their_mask
                    }
                };
                our_mask.contains(their_mask)
            }
        }
    }

    /// If this is a type hole with a single valid possible type, concretize it to that type.
    /// Returns *self for already-concrete types.
    /// Returns *self for type holes with multiple possible types
    pub fn concretize_single_mask(&self) -> Self {
        match self {
            Self::Concrete(_) => *self, // already concrete
            Self::Hole(mask) => mask
                .try_concretize()
                .map_or(*self, |concrete| Self::Concrete(concrete)),
        }
    }

    pub fn try_concretize(&self) -> Option<HLSLConcreteType> {
        match self {
            Self::Concrete(c) => Some(*c), // already concrete
            Self::Hole(mask) => mask.try_concretize(),
        }
    }

    pub fn intersect(&self, other: &HLSLType) -> Result<Self, TypeCoersionError> {
        match self {
            Self::Concrete(our_c) => {
                let intersects = match other {
                    HLSLType::Concrete(their_c) => our_c == their_c,
                    HLSLType::Hole(mask) => mask.contains((*our_c).into()),
                };
                if intersects {
                    Ok(*self)
                } else {
                    Err(TypeCoersionError::IntersectionFail(
                        self.clone(),
                        other.clone(),
                    ))
                }
            }
            Self::Hole(mask) => {
                // Intersect our mask with theirs
                let result = mask.intersection((*other).into());
                if result.is_empty() {
                    // Empty mask => no intersection
                    Err(TypeCoersionError::IntersectionFail(
                        self.clone(),
                        other.clone(),
                    ))
                } else {
                    // Try to concretize the mask, if it's concrete return the concrete type, else return the hole
                    Ok(result
                        .try_concretize()
                        .map_or(Self::Hole(result), |concrete| Self::Concrete(concrete)))
                }
            }
        }
    }
}

/// A type for operands passed to [Operator]s.
/// Equivalent to [HLSLType] but holds a hole ID instead of the actual hole mask.
///
/// [Operator]s need to keep track of type holes separately,
/// because there may be correlations between different argument's type holes,
/// so this enum uses zero-indexed IDs for correlated holes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HLSLOperandType {
    Concrete(HLSLConcreteType),
    Hole(usize),
}
impl From<HLSLConcreteType> for HLSLOperandType {
    fn from(c: HLSLConcreteType) -> Self {
        Self::Concrete(c)
    }
}
impl From<HLSLNumericType> for HLSLOperandType {
    fn from(num: HLSLNumericType) -> Self {
        Self::Concrete(num.into())
    }
}
impl HLSLOperandType {
    pub fn as_hlsltype(&self, holes: &[HLSLType]) -> HLSLType {
        match self {
            Self::Concrete(c) => HLSLType::Concrete(*c),
            Self::Hole(idx) => holes[*idx],
        }
    }

    pub fn concretize(&self, holes: &[HLSLConcreteType]) -> HLSLConcreteType {
        match self {
            Self::Concrete(c) => *c,
            Self::Hole(idx) => holes[*idx],
        }
    }
}

bitflags! {
    pub struct HLSLHoleTypeMask: u32 {
        const NUMERIC_FLOAT = 0b0000_0001;
        const NUMERIC_UINT = 0b0000_0010;
        const NUMERIC_SINT = 0b0000_0100;
        const TEXTURE2D = 0b0001_0000;

        const NUMERIC = Self::NUMERIC_FLOAT.bits | Self::NUMERIC_SINT.bits | Self::NUMERIC_UINT.bits;
        const INTEGER = Self::NUMERIC_SINT.bits | Self::NUMERIC_UINT.bits;
    }
}
impl From<HLSLNumericType> for HLSLHoleTypeMask {
    fn from(num: HLSLNumericType) -> Self {
        match num {
            HLSLNumericType::Float => HLSLHoleTypeMask::NUMERIC_FLOAT,
            HLSLNumericType::UnsignedInt => HLSLHoleTypeMask::NUMERIC_UINT,
            HLSLNumericType::SignedInt => HLSLHoleTypeMask::NUMERIC_SINT,
        }
    }
}
impl From<HLSLConcreteType> for HLSLHoleTypeMask {
    fn from(t: HLSLConcreteType) -> Self {
        match t {
            HLSLConcreteType::Numeric(num) => num.into(),
            HLSLConcreteType::Texture2D => HLSLHoleTypeMask::TEXTURE2D,
        }
    }
}
impl From<HLSLType> for HLSLHoleTypeMask {
    fn from(t: HLSLType) -> Self {
        match t {
            HLSLType::Concrete(c) => c.into(),
            HLSLType::Hole(mask) => mask,
        }
    }
}
impl HLSLHoleTypeMask {
    pub fn try_concretize(&self) -> Option<HLSLConcreteType> {
        match *self {
            HLSLHoleTypeMask::NUMERIC_FLOAT => Some(HLSLNumericType::Float.into()),
            HLSLHoleTypeMask::NUMERIC_SINT => Some(HLSLNumericType::SignedInt.into()),
            HLSLHoleTypeMask::NUMERIC_UINT => Some(HLSLNumericType::UnsignedInt.into()),
            HLSLHoleTypeMask::TEXTURE2D => Some(HLSLConcreteType::Texture2D),
            _ => None,
        }
    }
}

// TODO rename
#[derive(Debug)]
pub enum TypeCoersionError {
    /// Intersection of X and Y had no common ground :(
    IntersectionFail(HLSLType, HLSLType),
}

// pub fn simple_type_coerce<const N_IN: usize, T: OperatorTarget>(
//     input_kinds: &[T; N_IN],
//     allowed_input_kinds: &[HLSLType; N_IN],
//     allowed_output_kind: HLSLType,
// ) -> Result<([HLSLType; N_IN], HLSLType), TypeCoersionError> {
//     // Notes:
//     // First step has to be to resolve type holes.
//     // If the input we're given has holes [ Hole { id: 0, NUMERIC } ]
//     let final_holes = ;
// }
