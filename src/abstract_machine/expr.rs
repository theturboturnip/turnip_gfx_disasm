use arrayvec::ArrayVec;

use crate::{abstract_machine::{VMName, vector::VectorComponent, VMScalar, VMVector}, hlsl::{syntax::{HLSLOperator, Operator}, kinds::HLSLKind, HLSLRegister}};

use super::find_common;

pub type ContigSwizzle = ArrayVec<VectorComponent, 4>;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Vector<TReg: Clone + PartialEq> {
    Construction(Vec<Scalar<TReg>>, HLSLKind),
    PureSwizzle(TReg, ContigSwizzle, HLSLKind),
    PerCompExpr {
        op: HLSLOperator,
        n_comps: usize,
        inputs: Vec<Self>,
        output_kind: HLSLKind,
    },
    AllToAllExpr {
        op: HLSLOperator,
        inputs: Vec<Self>,
        output_kind: HLSLKind, // TODO separate output_kind and usage_kind for expressions? bit_cast_int(1.0f + 2.0f)
    }
}

impl<TReg: Clone + PartialEq> Vector<TReg> {
    pub fn usage_kind(&self) -> HLSLKind {
        match self {
            Vector::Construction(_, usage_kind) => *usage_kind,
            Vector::PureSwizzle(_, _, usage_kind) => *usage_kind,
            Vector::PerCompExpr { op, n_comps, inputs, output_kind } => *output_kind,
            Vector::AllToAllExpr { op, inputs, output_kind } => *output_kind,
        }
    }

    /// Given a set of scalars, create an UntypedVector from them.
    /// 
    /// If they're all Components of the same register, create PureSwizzle.
    /// Else, create a Construction
    pub fn of_scalars(scalars: Vec<Scalar<TReg>>, usage_kind: HLSLKind) -> Self {
        if let Some(reg) = find_common(scalars.iter(), |scalar| {
            match scalar {
                Scalar::Literal(..) | Scalar::Expr { .. } => None,
                Scalar::Component(reg, ..) => Some(reg),
            }
        }) {
            let comps = scalars.into_iter().map(|s| match s {
                Scalar::Literal(..) | Scalar::Expr { .. } => unreachable!(),
                Scalar::Component(_, c, _) => c,
            }).collect();
            Self::PureSwizzle(reg.clone(), comps, usage_kind)
        } else {
            Self::Construction(scalars, usage_kind)
        }
    }

    pub fn of_expr(op: HLSLOperator, inputs: Vec<Self>, usage_kind: HLSLKind) -> Self {
        assert_eq!(op.n_inputs(), inputs.len());
        
        if op == HLSLOperator::Assign {
            inputs[0] // TODO this loses kind information?
        } else {
            match op.dep_rel() {
                super::instructions::SimpleDependencyRelation::PerComponent => {
                    // inputs must all be the same length
                    let n_comps = find_common(inputs.iter(), |v| v.n_components()).unwrap();
                    Self::PerCompExpr { op, n_comps, inputs, output_kind: usage_kind }
                },
                super::instructions::SimpleDependencyRelation::AllToAll => {
                    // No restrictions on input
                    Self::AllToAllExpr { op, inputs, output_kind: usage_kind }
                },
            }
        }
    }

    pub fn map_reg<TOtherReg: Clone + PartialEq, F: FnMut(&TReg) -> TOtherReg>(&self, mut f: F) -> Vector<TOtherReg> {
        match self {
            Self::Construction(scalars, usage_kind) => Vector::Construction(
                scalars.iter().map(|s| s.map_reg(f)).collect(),
                *usage_kind,
            ),
            Self::PureSwizzle(reg, comps, usage_kind) => Vector::PureSwizzle(
                f(reg),
                comps.clone(),
                *usage_kind,
            ),
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => Vector::PerCompExpr {
                op: *op,
                n_comps: *n_comps,
                inputs: inputs.iter().map(|v| v.map_reg(f)).collect(),
                output_kind: *output_kind,
            },
            Self::AllToAllExpr { op, inputs, output_kind } => Vector::AllToAllExpr {
                op: *op,
                inputs: inputs.iter().map(|v| v.map_reg(f)).collect(),
                output_kind: *output_kind,
            },
        }
    }

    pub fn map_scalar<TOtherReg: Clone + PartialEq, F: FnMut(&TReg, VectorComponent) -> (TOtherReg, VectorComponent)>(&self, mut f: F) -> Vector<TOtherReg> {
        match self {
            Self::Construction(scalars, usage_kind) => {
                Vector::of_scalars(
                    scalars.iter().map(|scalar| scalar.map_scalar(f)).collect(),
                    *usage_kind
                )
            }
            Self::PureSwizzle(reg, comps, usage_kind) => {
                Vector::of_scalars(
                    comps.iter().map(|comp| {
                        let (reg, comp) = f(reg, *comp);
                        Scalar::Component(reg, comp, *usage_kind) // TODO ughhhh is this the right kind information? Maybe apply reg.toplevel_kind and expect of_scalars to mutate them?
                    }).collect(),
                    *usage_kind
                )
            }
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => {
                Vector::PerCompExpr {
                    op: *op,
                    n_comps: *n_comps,
                    inputs: inputs.iter().map(|v| v.map_scalar(f)).collect(),
                    output_kind: *output_kind,
                }
            },
            Self::AllToAllExpr { op, inputs, output_kind } => {
                Vector::AllToAllExpr {
                    op: *op,
                    inputs: inputs.iter().map(|v| v.map_scalar(f)).collect(),
                    output_kind: *output_kind,
                }
            },
        }
    }

    pub fn n_components(&self) -> Option<usize> {
        match self {
            Self::Construction(scalars, ..) => Some(scalars.len()),
            Self::PureSwizzle(_, comps, _) => Some(comps.len()),
            Self::PerCompExpr { n_comps, .. } => Some(*n_comps),
            Self::AllToAllExpr { .. } => None,
        }
    }

    pub fn scalar_deps(&self, dst: Vec<(TReg, VectorComponent)>) -> Vec<((TReg, VectorComponent), Vec<Scalar<TReg>>)> {
        match self {
            Self::Construction(scalars, usage_kind) => {
                assert_eq!(dst.len(), scalars.len());
                scalars
                    .iter()
                    .zip(dst)
                    .map(|(scalar, dst_scalar)| {
                        (dst_scalar, vec![scalar.clone()]) // TODO intersect type with usage_kind? 
                    })
                    .collect()
            }
            Self::PureSwizzle(reg, comps, usage_kind) => {
                assert_eq!(dst.len(), comps.len());
                comps
                    .iter()
                    .zip(dst)
                    .map(|(comp, dst_scalar)| {
                        (dst_scalar, vec![Scalar::Component(reg.clone(), *comp, *usage_kind)]) // TODO intersect reg base type with usage_kind? 
                    })
                    .collect()
            },
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => {
                assert_eq!(dst.len(), *n_comps);
                
                let decomposed_inputs: Vec<_> = inputs
                    .iter()
                    .map(|v| v.all_involved_scalars())
                    .collect();

                dst
                    .into_iter()
                    .enumerate()
                    .map(|(i, dst_scalar)| {
                        (
                            dst_scalar,
                            vec![Scalar::Expr {
                                op: *op,
                                inputs: decomposed_inputs
                                    .iter()
                                    .map(|decomposed_input| decomposed_input[i].clone())
                                    .collect(),
                                output_kind: *output_kind
                            }]
                        )
                    })
                    .collect()
            },
            Self::AllToAllExpr { op, inputs, output_kind } => {
                let all_scalars: Vec<_> = inputs.iter().map( |v| v.all_involved_scalars()).flatten().collect();
                
                dst.into_iter().map(|dst_scalar| (dst_scalar, all_scalars.clone())).collect()
            },
        }
    }

    pub fn all_involved_scalars(&self) -> Vec<Scalar<TReg>> {
        // TODO expand scalar expressions?
        match self {
            Self::Construction(scalars, usage_kind) => scalars.clone(), // TODO this throws away type info
            Self::PureSwizzle(reg, comps, usage_kind) => {
                comps
                    .iter()
                    .map(|comp| (Scalar::Component(reg.clone(), *comp, *usage_kind))) // TODO intersect with register usage kind
                    .collect()
            },
            Self::PerCompExpr { inputs, .. } | Self::AllToAllExpr { inputs, .. }=> {
                inputs.iter().map( |v| v.all_involved_scalars()).flatten().collect()
            },
        }
    }
}

pub type HLSLVector = Vector<HLSLRegister>;

impl VMName for HLSLVector {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => todo!(),
            Self::Construction { .. } => false,
            Self::PureSwizzle(reg, ..) => reg.is_pure_input(),
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => todo!(),
            Self::Construction { .. } => false,
            Self::PureSwizzle(reg, ..) => reg.is_output(),
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        todo!()
    }
}
impl VMVector for HLSLVector {
    fn n_components(&self) -> usize {
        match self {
            Self::AllToAllExpr { .. } => todo!(),
            Self::PerCompExpr { n_comps, .. } => *n_comps,
            Self::Construction(items, _) => items.len(),
            Self::PureSwizzle(_, comps, _) => comps.len(),            
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Scalar<TReg: Clone + PartialEq> {
    Literal(u64, HLSLKind),
    Component(TReg, VectorComponent, HLSLKind),
    Expr {
        op: HLSLOperator,
        inputs: Vec<Self>,
        output_kind: HLSLKind,
    }
}

impl<TReg: Clone + PartialEq> Scalar<TReg> {
    pub fn usage_kind(&self) -> HLSLKind {
        todo!()
    }

    pub fn map_reg<TOtherReg: Clone + PartialEq, F: FnMut(&TReg) -> TOtherReg>(&self, mut f: F) -> Scalar<TOtherReg> {
        match self {
            Self::Literal(lit, usage_kind) => Scalar::Literal(*lit, *usage_kind),
            Self::Component(reg, comp, usage_kind) => Scalar::Component(
                f(reg),
                *comp,
                *usage_kind,
            ),
            Self::Expr { op, inputs, output_kind } => Scalar::Expr {
                op: *op,
                inputs: inputs.iter().map(|s| s.map_reg(f)).collect(),
                output_kind: *output_kind,
            },
        }
    }

    pub fn map_scalar<TOtherReg: Clone + PartialEq, F: FnMut(&TReg, VectorComponent) -> (TOtherReg, VectorComponent)>(&self, mut f: F) -> Scalar<TOtherReg> {
        match self {
            Self::Literal(lit, usage_kind) => Scalar::Literal(*lit, *usage_kind),
            Self::Component(reg, comp, usage_kind) => {
                let (reg, comp) = f(reg, *comp);
                Scalar::Component(reg, comp, *usage_kind)
            }
            Self::Expr { op, inputs, output_kind } => Scalar::Expr {
                op: *op,
                inputs: inputs.iter().map(|s| s.map_scalar(f)).collect(),
                output_kind: *output_kind,
            },
        }
    }

    pub fn deps(&self) -> Vec<Self> {
        match self {
            Self::Expr { op, inputs, output_kind } => {
                inputs.iter().map(|i| {
                    i.deps()
                }).flatten().collect()
            },
            other => vec![other.clone()], // Loses my type information?
        }
    }
}

pub type HLSLScalar = Scalar<HLSLRegister>;

// impl<TReg: VMVector> UntypedScalar<TReg> {
//     pub fn new(reg: TReg, comp: VectorComponent) -> Self {
//         assert!(comp.into_index() < reg.n_components());
//         Self::Component(reg, comp)
//     }
//     pub fn lit(bits: u32) -> Self {
//         Self::Literal(bits as u64)
//     }
// }

impl<TReg: VMVector> VMName for Scalar<TReg> {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Expr { .. } => false,
            Self::Component(reg, ..) => reg.is_pure_input(),
            Self::Literal(..) => true,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::Expr { .. } => false,
            Self::Component(reg, ..) => reg.is_output(),
            Self::Literal(..) => false,
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        todo!()
    }
}
impl<TReg: VMVector> VMScalar for Scalar<TReg> {}
