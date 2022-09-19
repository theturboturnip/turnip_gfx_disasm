//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use super::DataRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VectorComponent {
    X,
    Y,
    Z,
    W,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MaskedSwizzle([Option<VectorComponent>; 4]);
impl MaskedSwizzle {
    pub fn new(x: [Option<VectorComponent>; 4]) -> Self {
        MaskedSwizzle(x)
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VectorDataRef {
    Register(u64, MaskedSwizzle),
    Literal([u64; 4]),
    NamedLiteral(String, MaskedSwizzle),
    NamedBuffer {
        name: String,
        idx: u64,
        swizzle: MaskedSwizzle,
    },
    NamedInputRegister(String, MaskedSwizzle),
    NamedOutputRegister(String, MaskedSwizzle),
}
impl DataRef for VectorDataRef {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VectorDeclaration {
    NamedLiteral(String, [u64; 4]),
    NamedBuffer {
        name: String,
        len: u64,
    },
    NamedInputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
    NamedOutputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
}
