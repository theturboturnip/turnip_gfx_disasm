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

/// A type for HLSL values - an [HLSLTypeMask] with at least one bit set
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HLSLType(HLSLHoleTypeMask);
impl HLSLType {
    pub fn try_concretize(&self) -> Option<HLSLConcreteType> {
        self.0.try_concretize()
    }
    /// Return an intersection of this and other, as long as that intersection has at least one bit set
    pub fn intersection(self, other: Self) -> Option<Self> {
        let i = self.0.intersection(other.0);
        if i.is_empty() {
            None
        } else {
            Some(Self(i))
        }
    }
}
impl From<HLSLHoleTypeMask> for HLSLType {
    fn from(m: HLSLHoleTypeMask) -> Self {
        if m.is_empty() {
            panic!("From(empty mask) attempted");
        }
        Self(m)
    }
}
impl From<HLSLConcreteType> for HLSLType {
    fn from(c: HLSLConcreteType) -> Self {
        Self(c.into())
    }
}
impl From<HLSLNumericType> for HLSLType {
    fn from(n: HLSLNumericType) -> Self {
        Self(n.into())
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
            Self::Concrete(c) => (*c).into(),
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
    /// Return an intersection of this and other, as long as that intersection has at least one bit set
    pub fn valid_intersection(self, other: Self) -> Option<Self> {
        let i = self.intersection(other);
        if i.is_empty() {
            None
        } else {
            Some(i)
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
