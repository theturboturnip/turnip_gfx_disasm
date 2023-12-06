//! Utilities for vector-based abstract machines.

use std::ops::Index;

use crate::hlsl::kinds::{HLSLKind, HLSLKindBitmask};

use super::{VMScalar, VMVector, VMName};

/*
impl<T: VMVectorNameRef> VMDataRef<T> for RefinableVMDataRef<VMScalarNameRef<T>> {
    fn name(&self) -> &T {
        &self.data.0
    }

    fn hlsl_kind(&self) -> HLSLKind {
        self.kind
    }
}
impl<T: VMVectorNameRef> VMScalarDataRef<T> for RefinableVMDataRef<VMScalarNameRef<T>> {
    fn comp(&self) -> VectorComponent {
        self.data.1
    }

    fn scalar_name(&self) -> VMScalarNameRef<T> {
        self.data.clone()
    }
}
impl<T: VMVectorNameRef> From<VMScalarNameRef<T>> for RefinableVMDataRef<VMScalarNameRef<T>> {
    fn from(data: VMScalarNameRef<T>) -> Self {
        Self {
            kind: data.0.base_hlsl_kind(),
            data,
        }
    }
}

/// Trait for types referencing the *name* of a VM element, e.g. a vector which could be subscripted.
/// May only be able to store a subset of [HLSLKind], but does not have any information about
///
/// For scalar machines, the same type may implement [VMVectorNameRef] and [VMDataRef].
pub trait VMVectorName: VMName {
    /// Number of components in the vector
    fn n_components(&self) -> u8;
    /// Base type - the lowest common denominator [HLSLKind] that could this name could possibly hold.
    fn base_hlsl_kind(&self) -> HLSLKind;
}
impl VMRef for [u64; 4] {
    fn is_pure_input(&self) -> bool {
        true
    }
}
impl VMVectorNameRef for [u64; 4] {
    fn n_components(&self) -> u8 {
        4
    }
    fn base_hlsl_kind(&self) -> HLSLKind {
        HLSLKindBitmask::NUMERIC.into()
    }
}

/// A VMRef referring to a specific scalar within a VM
pub type VMScalarNameRef<T: VMVectorNameRef> = (T, VectorComponent);
impl<T: VMVectorNameRef> VMRef for VMScalarNameRef<T> {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}

/// Marker trait for types referencing a scalar component within a named vector
pub trait VMScalarDataRef<T: VMVectorNameRef>: VMDataRef<T> {
    fn comp(&self) -> VectorComponent;
    fn scalar_name(&self) -> VMScalarNameRef<T>;
}

/// A VMDataRef that represents a VM's "element" - the main unit of computation for instructions.
pub trait VMVectorDataRef<T: VMVectorNameRef>: VMDataRef<T> {
    fn swizzle(&self) -> MaskedSwizzle;
    /// Returns a list of the components that were actually used from self
    ///
    /// e.g. for r0.x_w_, (r0, x) and (r0, w) will be returned
    fn decompose(&self) -> Vec<VMScalarNameRef<T>> {
        self.swizzle()
            .0
            .iter()
            .filter_map(|comp| match comp {
                Some(comp) => Some((self.name().clone(), *comp)),
                None => None,
            })
            .collect()
    }
}
impl<T: VMVectorNameRef> VMRef for (T, MaskedSwizzle) {
    fn is_pure_input(&self) -> bool {
        self.0.is_pure_input()
    }
}
impl<T: VMVectorNameRef> VMDataRef<T> for (T, MaskedSwizzle) {
    fn name(&self) -> &T {
        &self.0
    }

    fn hlsl_kind(&self) -> HLSLKind {
        self.name().base_hlsl_kind()
    }
}
impl<T: VMVectorNameRef> VMVectorDataRef<T> for (T, MaskedSwizzle) {
    fn swizzle(&self) -> MaskedSwizzle {
        self.1
    }
}
*/

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VectorOf<T: VMScalar> {
    pub ts: Vec<T>,
    kind: HLSLKind
}
impl<T: VMScalar> VectorOf<T> {
    pub fn new(ts: &[T]) -> Option<Self> {
        assert!(ts.len() > 0);
        let kind_mask = ts.iter().fold(Some(HLSLKindBitmask::all().into()), |kind, t| {
            HLSLKind::intersection(kind?, t.toplevel_kind())
        });
        Some(Self { ts: ts.iter().map(|t| t.clone()).collect(), kind: kind_mask?.into() })
    }
}
impl<T: VMScalar> VMName for VectorOf<T> {
    fn is_pure_input(&self) -> bool {
        self.ts.iter().all(T::is_pure_input)
    }

    fn is_output(&self) -> bool {
        self.ts.iter().all(T::is_output)
    }

    fn toplevel_kind(&self) -> HLSLKind {
        self.kind
    }
}
impl<T: VMScalar> VMVector for VectorOf<T> {
    fn n_components(&self) -> usize {
        self.ts.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HoleyVectorOf<T: VMScalar> {
    pub ts: Vec<Option<T>>,
    kind: HLSLKind
}
impl<T: VMScalar> HoleyVectorOf<T> {
    pub fn new(ts: &[Option<T>]) -> Option<Self> {
        let kind_mask = ts.iter().fold(Some(HLSLKindBitmask::all().into()), |kind, t| {
            if let Some(t) = t {
                HLSLKind::intersection(kind?, t.toplevel_kind())
            } else {
                kind
            }
        });
        Some(Self { ts: ts.iter().map(|t| t.clone()).collect(), kind: kind_mask?.into() })
    }
}
impl<T: VMScalar> VMName for HoleyVectorOf<T> {
    fn is_pure_input(&self) -> bool {
        self.ts.iter().all(|t| match t {
            Some(t) => t.is_pure_input(),
            None => true
        })
    }

    fn is_output(&self) -> bool {
        self.ts.iter().all(|t| match t {
            Some(t) => t.is_output(),
            None => true
        })
    }

    fn toplevel_kind(&self) -> HLSLKind {
        self.kind
    }
}
impl<T: VMScalar> VMVector for HoleyVectorOf<T> {
    fn n_components(&self) -> usize {
        self.ts.len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ComponentOf<T: VMVector> {
    pub vec: T,
    pub comp: VectorComponent
}
impl<T: VMVector> ComponentOf<T> {
    pub fn new(vec: T, comp: VectorComponent) -> Self {
        assert!(comp.into_index() < vec.n_components());
        Self {
            vec, comp
        }
    }
}
impl<T: VMVector> VMName for ComponentOf<T> {
    fn is_pure_input(&self) -> bool {
        self.vec.is_pure_input()
    }

    fn is_output(&self) -> bool {
        self.vec.is_output()
    }

    fn toplevel_kind(&self) -> HLSLKind {
        self.vec.toplevel_kind()
    }
}
impl<T: VMVector> VMScalar for ComponentOf<T> {}

impl<T: VMVector> HoleyVectorOf<ComponentOf<T>> {
    pub fn from_vector_swizzle(v: T, swizzle: MaskedSwizzle) -> Self {
        let ts: Vec<_> = swizzle.0.iter().map(|comp| {
            match comp {
                Some(comp) => Some(ComponentOf::new(v.clone(), *comp)),
                None => None,
            }
        }).collect();
        Self::new(&ts).expect("Using the same base vector should always result in a consistent kind.")
    }
}

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
impl VectorComponent {
    pub fn into_index(self) -> usize {
        match self {
            VectorComponent::X => 0,
            VectorComponent::Y => 1,
            VectorComponent::Z => 2,
            VectorComponent::W => 3,
        }
    }
}
impl From<usize> for VectorComponent {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::X,
            1 => Self::Y,
            2 => Self::Z,
            3 => Self::W,
            _ => panic!("Invalid usize to VectorComponent: {}", value)
        }
    }
}

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

    pub fn num_used_components(&self) -> usize {
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
