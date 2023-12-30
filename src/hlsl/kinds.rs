use bitflags::bitflags;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HLSLNumericKind {
    Float,
    UnsignedInt,
    SignedInt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HLSLConcreteKind {
    Numeric(HLSLNumericKind),
    Texture2D,
    Texture3D,
    TextureCube,
}
impl From<HLSLNumericKind> for HLSLConcreteKind {
    fn from(num: HLSLNumericKind) -> Self {
        Self::Numeric(num)
    }
}

pub enum KindRefinementResult {// = Result<HLSLKind, HLSLKind>;
    RefinedTo(HLSLKind),
    ValidNoChange(HLSLKind),
    InvalidNoChange,
}
impl KindRefinementResult {
    pub fn was_valid(&self) -> bool {
        match self {
            Self::RefinedTo(_) | Self::ValidNoChange(_) => true,
            Self::InvalidNoChange => false,
        }
    }
    pub fn did_refine(&self) -> bool {
        match self {
            Self::RefinedTo(_) => true,
            Self::ValidNoChange(_) | Self::InvalidNoChange => false,
        }
    }
}

/// A type for HLSL values - an [HLSLKindMask] with at least one bit set
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HLSLKind(HLSLKindBitmask);
impl HLSLKind {
    pub const NUMERIC_FLOAT: Self = Self(HLSLKindBitmask::NUMERIC_FLOAT);
    pub const NUMERIC_UINT: Self = Self(HLSLKindBitmask::NUMERIC_UINT);
    pub const NUMERIC_SINT: Self = Self(HLSLKindBitmask::NUMERIC_SINT);
    pub const TEXTURE2D: Self = Self(HLSLKindBitmask::TEXTURE2D);
    pub const TEXTURE3D: Self = Self(HLSLKindBitmask::TEXTURE3D);
    pub const TEXTURECUBE: Self = Self(HLSLKindBitmask::TEXTURECUBE);

    pub const NUMERIC: Self = Self(HLSLKindBitmask::NUMERIC);
    pub const INTEGER: Self = Self(HLSLKindBitmask::INTEGER);
    pub const TEXTURE: Self = Self(HLSLKindBitmask::TEXTURE);

    pub const ALL: Self = Self(HLSLKindBitmask::ALL);

    pub fn try_concretize(&self) -> Option<HLSLConcreteKind> {
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
    pub fn mask(&self) -> HLSLKindBitmask {
        self.0
    }
    pub fn refine_if_possible(&mut self, other: Self) -> KindRefinementResult {
        match self.intersection(other) {
            Some(valid) => {
                if valid != *self {
                    *self = valid;
                    KindRefinementResult::RefinedTo(valid)
                } else {
                    KindRefinementResult::ValidNoChange(valid)
                }
            },
            None => KindRefinementResult::InvalidNoChange,
        }
    }
}
impl From<HLSLKindBitmask> for HLSLKind {
    fn from(m: HLSLKindBitmask) -> Self {
        if m.is_empty() {
            panic!("From(empty mask) attempted");
        }
        Self(m)
    }
}
impl From<HLSLConcreteKind> for HLSLKind {
    fn from(c: HLSLConcreteKind) -> Self {
        Self(c.into())
    }
}
impl From<HLSLNumericKind> for HLSLKind {
    fn from(n: HLSLNumericKind) -> Self {
        Self(n.into())
    }
}

/// A type for operands passed to [HLSLOperator]s.
/// Equivalent to [HLSLKind] but holds a hole ID instead of the actual hole mask.
///
/// [HLSLOperator]s need to keep track of type holes separately,
/// because there may be correlations between different argument's type holes,
/// so this enum uses zero-indexed IDs for correlated holes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HLSLOperandKind {
    Concrete(HLSLConcreteKind),
    Hole(usize),
}
impl From<HLSLConcreteKind> for HLSLOperandKind {
    fn from(c: HLSLConcreteKind) -> Self {
        Self::Concrete(c)
    }
}
impl From<HLSLNumericKind> for HLSLOperandKind {
    fn from(num: HLSLNumericKind) -> Self {
        Self::Concrete(num.into())
    }
}
impl HLSLOperandKind {
    pub fn as_hlslkind(&self, holes: &[HLSLKind]) -> HLSLKind {
        match self {
            Self::Concrete(c) => (*c).into(),
            Self::Hole(idx) => holes[*idx],
        }
    }

    pub fn concretize(&self, holes: &[HLSLConcreteKind]) -> HLSLConcreteKind {
        match self {
            Self::Concrete(c) => *c,
            Self::Hole(idx) => holes[*idx],
        }
    }
}

bitflags! {
    pub struct HLSLKindBitmask: u32 {
        const NUMERIC_FLOAT = 0b0000_0001;
        const NUMERIC_UINT = 0b0000_0010;
        const NUMERIC_SINT = 0b0000_0100;
        const TEXTURE2D = 0b0001_0000;
        const TEXTURE3D = 0b0010_0000;
        const TEXTURECUBE = 0b0100_0000;

        const NUMERIC = Self::NUMERIC_FLOAT.bits | Self::NUMERIC_SINT.bits | Self::NUMERIC_UINT.bits;
        const INTEGER = Self::NUMERIC_SINT.bits | Self::NUMERIC_UINT.bits;
        const TEXTURE = Self::TEXTURE2D.bits | Self::TEXTURE3D.bits | Self::TEXTURECUBE.bits;

        const ALL = Self::TEXTURE.bits | Self::NUMERIC.bits;
    }
}
impl From<HLSLNumericKind> for HLSLKindBitmask {
    fn from(num: HLSLNumericKind) -> Self {
        match num {
            HLSLNumericKind::Float => HLSLKindBitmask::NUMERIC_FLOAT,
            HLSLNumericKind::UnsignedInt => HLSLKindBitmask::NUMERIC_UINT,
            HLSLNumericKind::SignedInt => HLSLKindBitmask::NUMERIC_SINT,
        }
    }
}
impl From<HLSLConcreteKind> for HLSLKindBitmask {
    fn from(t: HLSLConcreteKind) -> Self {
        match t {
            HLSLConcreteKind::Numeric(num) => num.into(),
            HLSLConcreteKind::Texture2D => HLSLKindBitmask::TEXTURE2D,
            HLSLConcreteKind::Texture3D => HLSLKindBitmask::TEXTURE3D,
            HLSLConcreteKind::TextureCube => HLSLKindBitmask::TEXTURECUBE,
        }
    }
}
impl HLSLKindBitmask {
    pub fn try_concretize(&self) -> Option<HLSLConcreteKind> {
        match *self {
            HLSLKindBitmask::NUMERIC_FLOAT => Some(HLSLNumericKind::Float.into()),
            HLSLKindBitmask::NUMERIC_SINT => Some(HLSLNumericKind::SignedInt.into()),
            HLSLKindBitmask::NUMERIC_UINT => Some(HLSLNumericKind::UnsignedInt.into()),
            HLSLKindBitmask::TEXTURE2D => Some(HLSLConcreteKind::Texture2D),
            HLSLKindBitmask::TEXTURE3D => Some(HLSLConcreteKind::Texture3D),
            HLSLKindBitmask::TEXTURECUBE => Some(HLSLConcreteKind::TextureCube),
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
