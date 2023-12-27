use arrayvec::ArrayVec;

use crate::{abstract_machine::{VMName, vector::VectorComponent, VMScalar, VMVector}, hlsl::{syntax::{HLSLOperator, Operator}, kinds::HLSLKind, HLSLRegister}};

use super::find_common;

pub type ContigSwizzle = ArrayVec<VectorComponent, 4>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UntypedVector<TReg: Clone + PartialEq> {
    Construction(Vec<UntypedScalar<TReg>>),
    PureSwizzle(TReg, ContigSwizzle),
    PerCompExpr {
        op: HLSLOperator,
        n_comps: usize,
        inputs: Vec<(Self, HLSLKind)>,
    },
    AllToAllExpr {
        op: HLSLOperator,
        inputs: Vec<(Self, HLSLKind)>,
    }
}

impl<TReg: Clone + PartialEq> UntypedVector<TReg> {
    /// Given a set of scalars, create an UntypedVector from them.
    /// 
    /// If they're all Components of the same register, create PureSwizzle.
    /// Else, create a Construction
    pub fn of_scalars(scalars: Vec<UntypedScalar<TReg>>) -> Self {
        if let Some(reg) = find_common(scalars.iter(), |scalar| {
            match scalar {
                UntypedScalar::Literal(_) | UntypedScalar::Expr { .. } => None,
                UntypedScalar::Component(reg, _) => Some(reg),
            }
        }) {
            let comps = scalars.into_iter().map(|s| match s {
                UntypedScalar::Literal(_) | UntypedScalar::Expr { .. } => unreachable!(),
                UntypedScalar::Component(_, c) => c,
            }).collect();
            Self::PureSwizzle(reg.clone(), comps)
        } else {
            Self::Construction(scalars)
        }
    }

    pub fn of_expr(op: HLSLOperator, inputs: Vec<(Self, HLSLKind)>) -> Self {
        assert_eq!(op.n_inputs(), inputs.len());
        
        if op == HLSLOperator::Assign {
            inputs[0].0 // TODO this loses kind information
        } else {
            match op.dep_rel() {
                super::instructions::SimpleDependencyRelation::PerComponent => {
                    // inputs must all be the same length
                    let n_comps = find_common(inputs.iter(), |(v, _)| v.n_components()).unwrap();
                    Self::PerCompExpr { op, n_comps, inputs }
                },
                super::instructions::SimpleDependencyRelation::AllToAll => {
                    // No restrictions on input
                    Self::AllToAllExpr { op, inputs }
                },
            }
        }
    }

    pub fn map_reg<TOtherReg: Clone + PartialEq, F: FnMut(&TReg) -> TOtherReg>(&self, mut f: F) -> UntypedVector<TOtherReg> {
        match self {
            Self::Construction(scalars) => UntypedVector::Construction(
                scalars.iter().map(|s| s.map_reg(f)).collect()
            ),
            Self::PureSwizzle(reg, comps) => UntypedVector::PureSwizzle(
                f(reg),
                comps.clone()
            ),
            Self::PerCompExpr { op, n_comps, inputs } => UntypedVector::PerCompExpr {
                op: *op,
                n_comps: *n_comps,
                inputs: inputs.iter().map(|(v, kind)| (v.map_reg(f), *kind)).collect()
            },
            Self::AllToAllExpr { op, inputs } => UntypedVector::AllToAllExpr {
                op: *op,
                inputs: inputs.iter().map(|(v, kind)| (v.map_reg(f), *kind)).collect()
            },
        }
    }

    pub fn map_scalar<TOtherReg: Clone + PartialEq, F: FnMut(&TReg, VectorComponent) -> (TOtherReg, VectorComponent)>(&self, mut f: F) -> UntypedVector<TOtherReg> {
        match self {
            Self::Construction(scalars) => {
                UntypedVector::of_scalars(
                    scalars.iter().map(|scalar| scalar.map_scalar(f)).collect()
                )
            }
            Self::PureSwizzle(reg, comps) => {
                UntypedVector::of_scalars(
                    comps.iter().map(|comp| {
                        let (reg, comp) = f(reg, *comp);
                        UntypedScalar::Component(reg, comp)
                    }).collect()
                )
            }
            Self::PerCompExpr { op, n_comps, inputs } => {
                UntypedVector::PerCompExpr {
                    op: *op,
                    n_comps: *n_comps,
                    inputs: inputs.iter().map(|(v, v_kind)| (v.map_scalar(f), *v_kind)).collect()
                }
            },
            Self::AllToAllExpr { op, inputs } => {
                UntypedVector::AllToAllExpr {
                    op: *op,
                    inputs: inputs.iter().map(|(v, v_kind)| (v.map_scalar(f), *v_kind)).collect()
                }
            },
        }
    }

    pub fn n_components(&self) -> Option<usize> {
        match self {
            Self::Construction(scalars) => Some(scalars.len()),
            Self::PureSwizzle(_, comps) => Some(comps.len()),
            Self::PerCompExpr { n_comps, .. } => Some(*n_comps),
            Self::AllToAllExpr { .. } => None,
        }
    }

    pub fn scalar_deps(&self, dst: Vec<UntypedScalar<TReg>>, my_kind: HLSLKind) -> Vec<(UntypedScalar<TReg>, Vec<(UntypedScalar<TReg>, HLSLKind)>)> {
        match self {
            Self::Construction(scalars) => {
                assert_eq!(dst.len(), scalars.len());
                scalars
                    .iter()
                    .zip(dst)
                    .map(|(scalar, dst_scalar)| {
                        (dst_scalar, vec![(scalar.clone(), my_kind)])
                    })
                    .collect()
            }
            Self::PureSwizzle(reg, comps) => {
                assert_eq!(dst.len(), comps.len());
                comps
                    .iter()
                    .zip(dst)
                    .map(|(comp, dst_scalar)| {
                        (dst_scalar, vec![(UntypedScalar::Component(reg.clone(), *comp), my_kind)])
                    })
                    .collect()
            },
            Self::PerCompExpr { op, n_comps, inputs } => {
                assert_eq!(dst.len(), *n_comps);
                
                let decomposed_inputs: Vec<_> = inputs
                    .iter()
                    .map(|(v, v_kind)| v.all_involved_scalars(*v_kind))
                    .collect();

                dst
                    .into_iter()
                    .enumerate()
                    .map(|(i, dst_scalar)| {
                        (
                            dst_scalar,
                            vec![(UntypedScalar::Expr {
                                op: *op,
                                inputs: decomposed_inputs
                                    .iter()
                                    .map(|decomposed_input| decomposed_input[i].clone())
                                    .collect()
                            }, my_kind)]
                        )
                    })
                    .collect()
            },
            Self::AllToAllExpr { op, inputs } => {
                let all_scalars: Vec<_> = inputs.iter().map( |(v, v_kind)| v.all_involved_scalars(*v_kind)).flatten().collect();
                
                dst.into_iter().map(|dst_scalar| (dst_scalar, all_scalars.clone())).collect()
            },
        }
    }

    pub fn all_involved_scalars(&self, my_kind: HLSLKind) -> Vec<(UntypedScalar<TReg>, HLSLKind)> {
        match self {
            Self::Construction(scalars) => scalars.iter().map(|s| (s.clone(), my_kind)).collect(),
            Self::PureSwizzle(reg, comps) => {
                comps
                    .iter()
                    .map(|comp| (UntypedScalar::Component(reg.clone(), *comp), my_kind))
                    .collect()
            },
            Self::PerCompExpr { inputs, .. } | Self::AllToAllExpr { inputs, .. }=> {
                inputs.iter().map( |(v, v_kind)| v.all_involved_scalars(*v_kind)).flatten().collect()
            },
        }
    }
}

pub type UntypedHLSLVector = UntypedVector<HLSLRegister>;
pub type HLSLVector = (UntypedHLSLVector, HLSLKind);

impl VMName for UntypedHLSLVector {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => todo!(),
            Self::Construction { .. } => false,
            Self::PureSwizzle(reg, _) => reg.is_pure_input(),
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => todo!(),
            Self::Construction { .. } => false,
            Self::PureSwizzle(reg, _) => reg.is_output(),
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        todo!()
    }
}
impl VMVector for UntypedHLSLVector {
    fn n_components(&self) -> usize {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => *n_comps,
            Self::Construction(items) => items.len(),
            Self::PureSwizzle(_, comps) => comps.len(),            
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UntypedScalar<TReg: Clone + PartialEq> {
    Literal(u64),
    Component(TReg, VectorComponent),
    Expr {
        op: HLSLOperator,
        inputs: Vec<(Self, HLSLKind)>,
    }
}

impl<TReg: Clone + PartialEq> UntypedScalar<TReg> {
    pub fn map_reg<TOtherReg: Clone + PartialEq, F: FnMut(&TReg) -> TOtherReg>(&self, mut f: F) -> UntypedScalar<TOtherReg> {
        match self {
            Self::Literal(lit) => UntypedScalar::Literal(*lit),
            Self::Component(reg, comp) => UntypedScalar::Component(
                f(reg),
                *comp
            ),
            Self::Expr { op, inputs } => UntypedScalar::Expr {
                op: *op,
                inputs: inputs.iter().map(|(v, kind)| (v.map_reg(f), *kind)).collect()
            },
        }
    }

    pub fn map_scalar<TOtherReg: Clone + PartialEq, F: FnMut(&TReg, VectorComponent) -> (TOtherReg, VectorComponent)>(&self, mut f: F) -> UntypedScalar<TOtherReg> {
        match self {
            Self::Literal(lit) => UntypedScalar::Literal(*lit),
            Self::Component(reg, comp) => {
                let (reg, comp) = f(reg, *comp);
                UntypedScalar::Component(reg, comp)
            }
            Self::Expr { op, inputs } => UntypedScalar::Expr {
                op: *op,
                inputs: inputs.iter().map(|(s, kind)| (s.map_scalar(f), *kind)).collect()
            },
            Self::Expr { op, inputs } => UntypedScalar::Expr {
                op: *op,
                inputs: inputs.iter().map(|(v, kind)| (v.map_scalar(f), *kind)).collect()
            },
        }
    }

    pub fn deps(&self, my_kind: HLSLKind) -> Vec<(Self, HLSLKind)> {
        match self {
            UntypedScalar::Expr { op, inputs } => {
                inputs.iter().map(|i| {
                    i.0.deps(i.1)
                }).flatten().collect()
            },
            other => vec![(other.clone(), my_kind)],
        }
    }
}

pub type UntypedHLSLScalar = UntypedScalar<HLSLRegister>;
pub type HLSLScalar = (UntypedHLSLScalar, HLSLKind);

impl<TReg: VMVector> UntypedScalar<TReg> {
    pub fn new(reg: TReg, comp: VectorComponent) -> Self {
        assert!(comp.into_index() < reg.n_components());
        Self::Component(reg, comp)
    }
    pub fn lit(bits: u32) -> Self {
        Self::Literal(bits as u64)
    }
}

impl<TReg: VMVector> VMName for UntypedScalar<TReg> {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Expr { .. } => false,
            Self::Component(reg, _) => reg.is_pure_input(),
            Self::Literal(_) => true,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::Expr { .. } => false,
            Self::Component(reg, _) => reg.is_output(),
            Self::Literal(_) => false,
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        todo!()
    }
}
impl<TReg: VMVector> VMScalar for UntypedScalar<TReg> {}
