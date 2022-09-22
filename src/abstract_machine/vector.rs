//! Utilities for vector-based abstract machines.

use std::ops::Index;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VectorComponent {
    X,
    Y,
    Z,
    W,
}
pub const VECTOR_COMPONENTS: [VectorComponent; 4] = [
    VectorComponent::X,
    VectorComponent::Y,
    VectorComponent::Z,
    VectorComponent::W,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MaskedSwizzle(pub [Option<VectorComponent>; 4]);
impl MaskedSwizzle {
    pub fn new(x: [Option<VectorComponent>; 4]) -> Self {
        Self(x)
    }
    /// Based on up to four components from vec, create a new masked swizzle
    pub fn new_from_vec(vec: Vec<VectorComponent>) -> Self {
        Self([
            if vec.len() > 0 { Some(vec[0]) } else { None },
            if vec.len() > 1 { Some(vec[1]) } else { None },
            if vec.len() > 2 { Some(vec[2]) } else { None },
            if vec.len() > 3 { Some(vec[3]) } else { None },
        ])
    }
    pub fn identity(num: usize) -> Self {
        use VectorComponent::*;
        match num {
            0 => Self([None; 4]),
            1 => Self([Some(X), None, None, None]),
            2 => Self([Some(X), Some(Y), None, None]),
            3 => Self([Some(X), Some(Y), Some(Z), None]),
            4 => Self([Some(X), Some(Y), Some(Z), Some(W)]),
            _ => panic!("MaskedSwizzle with {} (> 4) components requested", num),
        }
    }
    pub fn masked_identity(x: bool, y: bool, z: bool, w: bool) -> Self {
        use VectorComponent::*;
        Self([
            if x { Some(X) } else { None },
            if y { Some(Y) } else { None },
            if z { Some(Z) } else { None },
            if w { Some(W) } else { None },
        ])
    }
    /// If the MaskedSwizzle is a simple mask of contiguous elements beginning in x
    ///
    /// i.e. x___, xy__, xyz_, or xyzw
    ///
    /// returns the length of that contiguous run. Else returns None
    pub fn as_nonzero_length(&self) -> Option<u8> {
        use VectorComponent::*;
        match self {
            Self([Some(X), None, None, None]) => Some(1),
            Self([Some(X), Some(Y), None, None]) => Some(2),
            Self([Some(X), Some(Y), Some(Z), None]) => Some(3),
            Self([Some(X), Some(Y), Some(Z), Some(W)]) => Some(4),
            _ => None,
        }
    }

    /// Return a copy of self where each element is set to `None` if the corresponding element of `mask` is `None`
    ///
    /// e.g. `xyww.masked_out(xxx_) = xyw_`
    pub fn masked_out(&self, mask: Self) -> Self {
        Self([
            mask.0[0].and(self.0[0]),
            mask.0[1].and(self.0[1]),
            mask.0[2].and(self.0[2]),
            mask.0[3].and(self.0[3]),
        ])
    }

    /// Returns a copy of self equivalent to `identity(4).masked_out(self)`
    ///
    /// e.g. transfers the mask to the copy while keeping correct component order
    pub fn copy_mask(&self) -> Self {
        Self::masked_identity(
            self.0[0].is_some(),
            self.0[1].is_some(),
            self.0[2].is_some(),
            self.0[3].is_some(),
        )
    }

    pub fn truncated(&self, len: u8) -> Self {
        Self([
            if len >= 1 { self.0[0] } else { None },
            if len >= 2 { self.0[1] } else { None },
            if len >= 3 { self.0[2] } else { None },
            if len >= 4 { self.0[3] } else { None },
        ])
    }

    pub fn num_used_components(&self) -> u8 {
        self.0.iter().fold(0, |num_used, comp| {
            if comp.is_some() {
                num_used + 1
            } else {
                num_used
            }
        })
    }
}
impl Index<usize> for MaskedSwizzle {
    type Output = Option<VectorComponent>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}
impl std::fmt::Display for MaskedSwizzle {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, ".")?;
        for c in self.0 {
            match c {
                None => write!(f, "_"),
                Some(VectorComponent::X) => write!(f, "x"),
                Some(VectorComponent::Y) => write!(f, "y"),
                Some(VectorComponent::Z) => write!(f, "z"),
                Some(VectorComponent::W) => write!(f, "w"),
            }?
        }
        Ok(())
    }
}
