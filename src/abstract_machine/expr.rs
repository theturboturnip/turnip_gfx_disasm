//! # Thoughts on kinds
//! 
//! There's a difference between how an expression is used and what the actual output of it is.
//! For example: I could say int x = bit_cast_to_int(1.0f + 2.0f).
//! 1.0f + 2.0f has an output_kind of FLOAT, but x *uses* it as INT.
//! If output_kind does not intersect with usage_kind that results in a bitwise cast.
//! If output_kind DOES intersect with usage_kind that means we may be able to refine output_kind to the intersection with usage_kind, and vice versa.
//! Refining output_kind means propagating changes inwards - refining any types that were involved as input
//!     - e.g. if the output_kind of `x + y` refined from FLOAT | SINT to FLOAT, we can now refine the kinds of x and y to FLOAT.
//! Refining usage_kind means propagating changes backwards - up through expressions this one is nested inside until we reach a top-level output.
//!     - e.g. if `z = x + y` and we refine the usage_kind of (x + y) from FLOAT | SINT to FLOAT (presumbly from some information about the operation e.g. if x and y are known to be FLOAT), then we're effectively stating Z must be a float.
//!     - e.g. if `z = (x + y) + a` and we refine the usage_kind of (x + y) from FLOAT | SINT to FLOAT, then the next-level expression learns that A must be used as a float too (and if this intersects with A's output_kind mask then we can refine the type of A to be a float as well!)
//! 
//! AAAH FUCK
//! A MACHINE CAN NEVER PRODUCE AN OUTPUT THROUGH A MANIPULATION WITH AN AMBIGUOUS KIND?
//! no, not true. Consider signed/unsigned integer negate. Same instruction for two's complement values
//! 
//! This matters for kind inference.
//! Inside AMDIL registers are typeless so effectively output_kind = ANY in most cases, but 

use arrayvec::ArrayVec;

use crate::{abstract_machine::{VMName, vector::VectorComponent, VMScalar, VMVector}, hlsl::{syntax::{HLSLOperator, Operator}, kinds::{HLSLKind, HLSLOperandKind, HLSLKindBitmask, KindRefinementResult}, HLSLRegister}};

use super::find_common;

pub type ContigSwizzle = ArrayVec<VectorComponent, 4>;

pub trait Reg: Clone + PartialEq {
    fn output_kind(&self) -> HLSLKind;
    fn refine_output_kind_if_possible(&mut self, constraint: HLSLKind) -> Option<KindRefinementResult> {
        None
    }
}

/// Vectors keep their usage_kind or output_kind inside.
/// Scalars do not, because it creates contradiction risk.
/// A Construction (the only place where scalars are actually involved) has one consistent usage_kind for all the scalars - the scalars are not individually cast to different kinds.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Vector<TReg: Reg> {
    /// A vector created from a set of scalars.
    /// 
    /// output_kind is stored alongside the scalars, and counts as the usage_kind for each scalar.
    /// usage_kind is stored externally.
    Construction(Vec<Scalar<TReg>>, HLSLKind),
    /// A vector created by swizzling a register.
    /// 
    /// output_kind is stored internally, and counts as the usage_kind for the register.
    /// usage_kind is stored externally.
    PureSwizzle(TReg, ContigSwizzle, HLSLKind),
    /// An expression using a per-component dependency relation.
    /// e.g. adding two vectors together (r0.xyz + r1.xyz) makes each output component depend on the corresponding inputs
    /// x -> (r0.x, r1.x), y -> (r0.y, r1.y), etc.
    /// 
    /// Requires all the inputs to have the same number of components.
    /// 
    /// Stores the usage_kind for each input, and the output_kind of the expression,
    /// all of which are calculated through type inference.
    /// usage_kind of the expression is stored externally.
    PerCompExpr {
        op: HLSLOperator,
        inputs: Vec<(Self, HLSLKind)>,
        output_kind: HLSLKind,
        n_comps: usize,
    },
    /// An expression using an all-to-all dependency relation. 
    /// e.g. sampling a texture with a UV makes each output component depend on (texture, u, v).
    /// The output is not of known length (!) so this cannot be directly used inside a PerCompExpr.
    /// 
    /// Does not place any restrictions on the inputs.
    /// 
    /// Stores the usage_kind for each input, and the output_kind of the expression,
    /// all of which are calculated through type inference.
    /// usage_kind of the expression is stored externally.
    AllToAllExpr {
        op: HLSLOperator,
        inputs: Vec<(Self, HLSLKind)>,
        output_kind: HLSLKind,
    }
}

impl<TReg: Reg> Vector<TReg> {
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
    pub fn of_scalars(scalars: Vec<Scalar<TReg>>) -> Self {
        // TODO refine output_kind with the output_kind of the scalars
        let mut v = if let Some(reg) = find_common(scalars.iter(), |scalar| {
            match scalar {
                Scalar::Literal(..) | Scalar::Expr { .. } => None,
                Scalar::Component(reg, ..) => Some(reg.clone()),
            }
        }) {
            let comps = scalars.into_iter().map(|s| match s {
                Scalar::Literal(..) | Scalar::Expr { .. } => unreachable!(),
                Scalar::Component(_, c) => c,
            }).collect();
            Self::PureSwizzle(reg.clone(), comps, HLSLKindBitmask::ALL.into())
        } else {
            Self::Construction(scalars, HLSLKindBitmask::ALL.into())
        };

        v.recompute_output_kind_from_internal_output_kinds();

        v
    }

    pub fn of_expr(op: HLSLOperator, inputs: Vec<(Self, HLSLKind)>, output_kind: HLSLKind) -> Self {
        assert_eq!(op.n_inputs(), inputs.len());
        
        let mut v = match op.dep_rel() {
            super::instructions::SimpleDependencyRelation::PerComponent => {
                // inputs must all be the same length
                let n_comps = find_common(inputs.iter(), |(v, usage)| v.n_components()).unwrap();
                Self::PerCompExpr { op, inputs, output_kind, n_comps }
            },
            super::instructions::SimpleDependencyRelation::AllToAll => {
                // No restrictions on input
                Self::AllToAllExpr { op, inputs, output_kind }
            },
        };

        v.recompute_output_kind_from_internal_output_kinds();

        v
    }

    // /// For every available pair of (reg, usage_kind) (even in scalars) call a function on them that can mutate the register.
    // /// 
    // /// This is used to 
    // pub fn map_kind<F: FnMut(&mut TReg, HLSLKind)>(&mut self, f: &mut F) {
    //     match self {
    //         Vector::Construction(scalars, usage_kind) => todo!(),
    //         Vector::PureSwizzle(_, _, _) => todo!(),
    //         Vector::PerCompExpr { op, n_comps, inputs, output_kind } => todo!(),
    //         Vector::AllToAllExpr { op, inputs, output_kind } => todo!(),
    //     }
    // }

    pub fn map_reg<TOtherReg: Reg, F: FnMut(&TReg, HLSLKind) -> TOtherReg>(&self, f: &mut F) -> Vector<TOtherReg> {
        match self {
            Self::Construction(scalars, usage_kind) => Vector::Construction(
                scalars.iter().map(|s| s.map_reg(f, *usage_kind)).collect(),
                *usage_kind,
            ),
            Self::PureSwizzle(reg, comps, usage_kind) => Vector::PureSwizzle(
                f(reg, *usage_kind),
                comps.clone(),
                *usage_kind,
            ),
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => Vector::PerCompExpr {
                op: *op,
                n_comps: *n_comps,
                inputs: inputs.iter().map(|(v, usage)| (v.map_reg(f), *usage)).collect(),
                output_kind: *output_kind,
            },
            Self::AllToAllExpr { op, inputs, output_kind } => Vector::AllToAllExpr {
                op: *op,
                inputs: inputs.iter().map(|(v, usage)| (v.map_reg(f), *usage)).collect(),
                output_kind: *output_kind,
            },
        }
    }

    pub fn map_scalar<TOtherReg: Reg, F: FnMut(&TReg, VectorComponent, HLSLKind) -> (TOtherReg, VectorComponent)>(&self, f: &mut F) -> Vector<TOtherReg> {
        match self {
            Self::Construction(scalars, output_kind) => {
                Vector::of_scalars(
                    scalars.iter().map(|scalar| scalar.map_scalar(f, *output_kind)).collect(),
                )
            }
            Self::PureSwizzle(reg, comps, output_kind) => {
                Vector::of_scalars(
                    comps.iter().map(|comp| {
                        let (reg, comp) = f(reg, *comp, *output_kind);
                        Scalar::Component(reg, comp)
                    }).collect(),
                )
            }
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => {
                let mut v = Vector::PerCompExpr {
                    op: *op,
                    n_comps: *n_comps,
                    inputs: inputs.iter().map(|(v, usage)| (v.map_scalar(f), *usage)).collect(),
                    output_kind: *output_kind,
                };
                v.recompute_output_kind_from_internal_output_kinds();
                v
            },
            Self::AllToAllExpr { op, inputs, output_kind } => {
                let mut v = Vector::AllToAllExpr {
                    op: *op,
                    inputs: inputs.iter().map(|(v, usage)| (v.map_scalar(f), *usage)).collect(),
                    output_kind: *output_kind,
                };
                v.recompute_output_kind_from_internal_output_kinds();
                v
            },
        }
    }

    pub fn output_kind(&self) -> HLSLKind {
        match self {
            Vector::Construction(_, output_kind) => *output_kind,
            Vector::PureSwizzle(_, _, output_kind) => *output_kind,
            Vector::PerCompExpr { output_kind, .. } => *output_kind,
            Vector::AllToAllExpr { output_kind, .. } => *output_kind,
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

    pub fn scalar_deps(&self, n_dst_comps: usize) -> Vec<Vec<(TReg, VectorComponent, HLSLKind)>> {
        match self {
            Self::Construction(scalars, output_kind) => {
                assert_eq!(n_dst_comps, scalars.len());
                scalars
                    .iter()
                    .map(|scalar| {
                        scalar.deps(*output_kind) // TODO intersect type with usage_kind? 
                    })
                    .collect()
            }
            Self::PureSwizzle(reg, comps, output_kind) => {
                assert_eq!(n_dst_comps, comps.len());
                comps
                    .iter()
                    .map(|comp| {
                        vec![(reg.clone(), *comp, *output_kind)]
                    })
                    .collect()
            },
            Self::PerCompExpr { op, n_comps, inputs, output_kind } => {
                assert_eq!(n_dst_comps, *n_comps);

                // Decompose each input into a vector of n_dst_comps dependencies
                let decomposed_inputs: Vec<_> = inputs
                    .iter()
                    .map(|(v, usage)| v.scalar_deps(n_dst_comps))
                    .collect();

                // For i in 0..n_dst_comps, collect the dependencies from scalar #i of each decomposed input
                (0..n_dst_comps)
                    .into_iter()
                    .map(|i| {
                        let mut v = vec![];
                        for decomposed_input in decomposed_inputs.iter() {
                            v.extend(decomposed_input[i].iter().map(|x| x.clone())) // TODO this is probably unnecessary
                        }
                        v
                    })
                    .collect()
            },
            Self::AllToAllExpr { op, inputs, output_kind } => {
                let all_scalars: Vec<_> = inputs.iter().map( |(v, usage)| v.all_involved_scalars()).flatten().collect();
                
                // Each scalar i from 0..n_dst_comps depends on *all* output scalars
                (0..n_dst_comps)
                    .into_iter()
                    .map(|i| all_scalars.clone())
                    .collect()
            },
        }
    }

    // TODO keep kind information around
    pub fn all_involved_scalars(&self) -> Vec<(TReg, VectorComponent, HLSLKind)> {
        match self {
            Self::Construction(scalars, output_kind) => {
                // TODO expand scalar expressions
                todo!()
            },
            Self::PureSwizzle(reg, comps, output_kind) => {
                comps
                    .iter()
                    .map(|comp| (reg.clone(), *comp, *output_kind)) // The usage kind of this scalar = the output kind of this swizzle
                    .collect()
            },
            Self::PerCompExpr { inputs, .. } | Self::AllToAllExpr { inputs, .. }=> {
                inputs.iter().map( |(v, usage)| v.all_involved_scalars()).flatten().collect()
            },
        }
    }

    pub fn recompute_output_kind_from_internal_output_kinds(&mut self) -> Option<KindRefinementResult> {
        match self {
            Vector::Construction(scalars, output_kind) => {
                // Progressively try to refine output_kind based on the output_kind of each scalar.
                // if we succeed at any point, immediately loop over the others and try to refine their 
                // output kind based on the new usage.
                let old_output_kind = *output_kind;
                for i in 0..scalars.len() {
                    if output_kind.refine_if_possible(scalars[i].output_kind()).did_refine() {
                        for i in 0..scalars.len() {
                            scalars[i].refine_output_kind_from_usage(*output_kind)
                        }
                    }
                }
                // What I want to do:
                // for s in scalars.iter_mut() {
                //     if output_kind.refine_if_possible(s.output_kind()).did_refine() {
                //         for s in scalars.iter_mut() {
                //             s.refine_output_kind_from_usage(*output_kind)
                //         }
                //     }
                // }
                if *output_kind != old_output_kind {
                    Some(KindRefinementResult::RefinedTo(*output_kind))
                } else {
                    None
                }
            },
            Vector::PureSwizzle(reg, _, output_kind) => {
                let old_output_kind = *output_kind;
                output_kind.refine_if_possible(reg.output_kind());
                if *output_kind != old_output_kind {
                    Some(KindRefinementResult::RefinedTo(*output_kind))
                } else {
                    None
                }
            },
            Vector::PerCompExpr { op, inputs, output_kind, .. } |
            Vector::AllToAllExpr { op, inputs, output_kind } => {
                let old_output_kind = *output_kind;
                for (input, input_usage) in inputs.iter_mut() {
                    // If any of our inputs have had their output kinds changed, try to refine their usage
                    if let Some(KindRefinementResult::RefinedTo(new_output_kind_for_input)) = input.recompute_output_kind_from_internal_output_kinds() {
                        if input_usage.refine_if_possible(new_output_kind_for_input).did_refine() {
                            // If we succeed in refinine one input's usage, immediately apply that to all other inputs and outputs with kind inference
                            let mut kindspec = op.get_kindspec();
                            // The kindspec gives us either a concrete kind or a hole index for each input and output.
                            // Apply constraints to the holes in the kindspec
                            kindspec.apply_input_constraints(inputs.iter().map(|(v, usage_kind)| *usage_kind));
                            todo!("actually apply the new output kinds!");
                            kindspec.apply_output_constraint(*output_kind);

                            // We know that the kindspec will respect all current constraints and can only tighten them.

                            // We now have complete holes or concrete types.
                            // Apply them to the input/output.
                            let holes = kindspec.holes();
                            for ((input_vec, current_usage), inferred_usage) in inputs.iter_mut().zip(kindspec.input_kinds().iter()) {
                                let inferred_usage = match inferred_usage {
                                    HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                                    HLSLOperandKind::Hole(idx) => holes[*idx],
                                };
                                // TODO recheck this comment
                                // This cannot have recursive effects.
                                // The kind inference model we have is limited.
                                // If two elements have a related kind, they must have the *same* kind.
                                // Furthermore, operand_usage_constraint has already been intersected with input_vec.usage_kind().
                                // If input_vec were an expression, and our type inference system was more complex, then refining the output kind could then cause a domino effect
                                // with other input kinds which then *further* reduce the output kind.
                                // Right now, refining the output_kind of input_vec will reduce input_vec.usage_kind() to exactly what it was before, or to operand_usage_constraint.
                                // If input_vec were an expression, and input_vec.usage_kind() were connected to a type hole, refining input_vec.usage_kind() can only further reduce 
                                // the inputs to input_vec, which cannot have domino effects onto the output of input_vec.

                                current_usage.refine_if_possible(inferred_usage);

                                // TODO do we need to check the outcome of this? Does this require us to re-type-check?
                                input_vec.refine_output_kind_from_usage(inferred_usage)
                            }
                            {
                                let final_kind = match kindspec.output_kind() {
                                    HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                                    HLSLOperandKind::Hole(idx) => holes[*idx],
                                };
                                // These *should* match
                                *output_kind = output_kind.intersection(final_kind).unwrap();
                            }
                        }
                    }
                }
                if *output_kind != old_output_kind {
                    Some(KindRefinementResult::RefinedTo(*output_kind))
                } else {
                    None
                }
            }
        }
    }

    /// Given a new constraint to how this value is *used*, refine the *output_kind* of this Vector if possible.
    /// Refining the output kind might ripple up through expressions by refining how the inputs to that expression were used,
    /// and may end up rippling out to refining the output_kinds of registers.
    pub fn refine_output_kind_from_usage(&mut self, usage_constraint: HLSLKind) {
        match self {
            Vector::Construction(scalars, output_kind) => {
                // The usage type for each scalar has been reduced to usage_constraint.
                if output_kind.refine_if_possible(usage_constraint).was_valid() {
                    // Run refine_output_kind_from_usage on each scalar using this new constraint.
                    // Even if our output_kind didn't change, some of our scalars might have output_kinds greater than that.
                    for s in scalars {
                        s.refine_output_kind_from_usage(*output_kind)
                    }
                }
            },
            Vector::PureSwizzle(reg, _swizzle, output_kind) => {
                if output_kind.refine_if_possible(usage_constraint).was_valid() {
                    reg.refine_output_kind_if_possible(*output_kind);         
                }
            },
            Vector::PerCompExpr { op, inputs, output_kind, .. } |
            Vector::AllToAllExpr { op, inputs, output_kind } => 
                if output_kind.refine_if_possible(usage_constraint).did_refine() {
                    let mut kindspec = op.get_kindspec();
                    // The kindspec gives us either a concrete kind or a hole index for each input and output.
                    // Apply constraints to the holes in the kindspec
                    kindspec.apply_input_constraints(inputs.iter().map(|(v, usage_kind)| *usage_kind));
                    kindspec.apply_output_constraint(*output_kind);

                    // We know that the kindspec will respect all current constraints and can only tighten them.

                    // eprintln!("\n--------------------------\nKindspec for {:?}: {:?}", op, &kindspec);
                    // eprintln!("Inputs: {:?}\nOutput: {:?}", inputs.iter().map(|(_, kind)| kind).collect::<Vec<_>>(), output.1);

                    // eprintln!("After inference, holes = {:?}", &inferred_holes);
                    // We now have complete holes or concrete types.
                    // Apply them to the input/output.
                    let holes = kindspec.holes();
                    for ((input_vec, current_usage), inferred_usage) in inputs.iter_mut().zip(kindspec.input_kinds().iter()) {
                        let inferred_usage = match inferred_usage {
                            HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                            HLSLOperandKind::Hole(idx) => holes[*idx],
                        };
                        // TODO recheck this comment
                        // This cannot have recursive effects.
                        // The kind inference model we have is limited.
                        // If two elements have a related kind, they must have the *same* kind.
                        // Furthermore, operand_usage_constraint has already been intersected with input_vec.usage_kind().
                        // If input_vec were an expression, and our type inference system was more complex, then refining the output kind could then cause a domino effect
                        // with other input kinds which then *further* reduce the output kind.
                        // Right now, refining the output_kind of input_vec will reduce input_vec.usage_kind() to exactly what it was before, or to operand_usage_constraint.
                        // If input_vec were an expression, and input_vec.usage_kind() were connected to a type hole, refining input_vec.usage_kind() can only further reduce 
                        // the inputs to input_vec, which cannot have domino effects onto the output of input_vec.
                        current_usage.refine_if_possible(inferred_usage);
                        // TODO do we need to check the outcome of this? Does this require us to re-type-check?
                        input_vec.refine_output_kind_from_usage(inferred_usage)
                    }
                    todo!("Should we reduce usage_constraint here as well? ");
                    {
                        let final_kind = match kindspec.output_kind() {
                            HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                            HLSLOperandKind::Hole(idx) => holes[*idx],
                        };
                        // These *should* match
                        *output_kind = output_kind.intersection(final_kind).unwrap();
                    }
                }
        }
    }
}

pub type HLSLVector = Vector<HLSLRegister>;

impl<TReg: VMVector + Reg> VMName for Vector<TReg> {
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
        self.output_kind()
    }
}
impl<TReg: VMVector + Reg> VMVector for Vector<TReg> {
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
pub enum Scalar<TReg: Reg> {
    /// A 64-bit literal (although AMDIL only uses 32-bit as far as I know.)
    /// 
    /// output_kind must be strictly equal to usage_type.
    Literal(u64),
    /// A component of a register.
    /// 
    /// output_kind = register.kind 
    Component(TReg, VectorComponent),
    /// A scalar expression. Stores the usage_kind of each of it's inputs
    /// 
    /// output_kind is stored inside the expression, and is calculated from type inference.
    Expr {
        op: HLSLOperator,
        inputs: Vec<(Self, HLSLKind)>,
        output_kind: HLSLKind,
    }
}

impl<TReg: Reg> Scalar<TReg> {
    pub fn output_kind(&self) -> HLSLKind {
        match self {
            Scalar::Literal(_) => HLSLKindBitmask::NUMERIC.into(),
            Scalar::Component(reg, _) => reg.output_kind(),
            Scalar::Expr { op, inputs, output_kind } => *output_kind,
        }
    }

    pub fn refine_output_kind_from_usage(&mut self, usage: HLSLKind) {
        match self {
            Scalar::Literal(..) => {},
            Scalar::Component(reg, _) => {
                reg.refine_output_kind_if_possible(usage);
            },
            Scalar::Expr { op, inputs, output_kind } => 
            if output_kind.refine_if_possible(usage).did_refine() {
                let mut kindspec = op.get_kindspec();
                // The kindspec gives us either a concrete kind or a hole index for each input and output.
                // Apply constraints to the holes in the kindspec
                kindspec.apply_input_constraints(inputs.iter().map(|(_s, s_kind)| *s_kind));
                kindspec.apply_output_constraint(*output_kind);

                // eprintln!("\n--------------------------\nKindspec for {:?}: {:?}", op, &kindspec);
                // eprintln!("Inputs: {:?}\nOutput: {:?}", inputs.iter().map(|(_, kind)| kind).collect::<Vec<_>>(), output.1);

                // eprintln!("After inference, holes = {:?}", &inferred_holes);
                // We now have complete holes or concrete types.
                // Apply them to the input/output.
                let holes = kindspec.holes();
                for ((input_s, input_s_kind), operand_kind) in inputs.iter_mut().zip(kindspec.input_kinds().iter()) {
                    let usage_constraint = match operand_kind {
                        HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                        HLSLOperandKind::Hole(idx) => holes[*idx],
                    };
                    // TODO what happens if this is an error? Really this shouldn't be possible.
                    input_s_kind.refine_if_possible(usage_constraint);//.expect("inputs to a scalar expression had contradictory kinds despite using the same hole");
                }
                {
                    let final_kind = match kindspec.output_kind() {
                        HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                        HLSLOperandKind::Hole(idx) => holes[*idx],
                    };
                    // These *should* match
                    *output_kind = output_kind.intersection(final_kind).unwrap();
                }
            },
        }
    }

    // TODO this doesn't detect changes, because the previous output kind isn't stored anywhere.
    pub fn recompute_output_kind_from_internal_output_kinds(&mut self) -> HLSLKind {
        match self {
            Scalar::Component(reg, _) => reg.output_kind(),
            Scalar::Literal(_) => HLSLKindBitmask::NUMERIC.into(),
            Scalar::Expr { op, inputs, output_kind } => {
                let old_output_kind = *output_kind;
                for (input, input_usage) in inputs.iter_mut() {
                    // If any of our inputs have had their output kinds changed, try to refine their usage
                    let new_output_kind_for_input = input.recompute_output_kind_from_internal_output_kinds();
                    if input_usage.refine_if_possible(new_output_kind_for_input).did_refine() {
                        // If we succeed in refinine one input's usage, immediately apply that to all other inputs and outputs with kind inference
                        let mut kindspec = op.get_kindspec();
                        // The kindspec gives us either a concrete kind or a hole index for each input and output.
                        // Apply constraints to the holes in the kindspec
                        kindspec.apply_input_constraints(inputs.iter().map(|(s, usage_kind)| *usage_kind));
                        // TODO actually apply the new output kind!!
                        todo!("actually apply the new output kinds!");
                        kindspec.apply_output_constraint(*output_kind);

                        // We know that the kindspec will respect all current constraints and can only tighten them.

                        // We now have complete holes or concrete types.
                        // Apply them to the input/output.
                        let holes = kindspec.holes();
                        for ((input_vec, current_usage), inferred_usage) in inputs.iter_mut().zip(kindspec.input_kinds().iter()) {
                            let inferred_usage = match inferred_usage {
                                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                                HLSLOperandKind::Hole(idx) => holes[*idx],
                            };
                            // TODO recheck this comment
                            // This cannot have recursive effects.
                            // The kind inference model we have is limited.
                            // If two elements have a related kind, they must have the *same* kind.
                            // Furthermore, operand_usage_constraint has already been intersected with input_vec.usage_kind().
                            // If input_vec were an expression, and our type inference system was more complex, then refining the output kind could then cause a domino effect
                            // with other input kinds which then *further* reduce the output kind.
                            // Right now, refining the output_kind of input_vec will reduce input_vec.usage_kind() to exactly what it was before, or to operand_usage_constraint.
                            // If input_vec were an expression, and input_vec.usage_kind() were connected to a type hole, refining input_vec.usage_kind() can only further reduce 
                            // the inputs to input_vec, which cannot have domino effects onto the output of input_vec.

                            current_usage.refine_if_possible(inferred_usage);
                            input_vec.refine_output_kind_from_usage(inferred_usage)
                        }
                        {
                            let final_kind = match kindspec.output_kind() {
                                HLSLOperandKind::Concrete(conc_kind) => (*conc_kind).into(), // Already applied
                                HLSLOperandKind::Hole(idx) => holes[*idx],
                            };
                            // These *should* match
                            *output_kind = output_kind.intersection(final_kind).unwrap();
                        }
                    }
                
                }
                *output_kind
            }
        }
    }

    pub fn map_reg<TOtherReg: Reg, F: FnMut(&TReg, HLSLKind) -> TOtherReg>(&self, f: &mut F, usage_kind: HLSLKind) -> Scalar<TOtherReg> {
        match self {
            Self::Literal(lit) => Scalar::Literal(*lit),
            Self::Component(reg, comp) => Scalar::Component(
                f(reg, usage_kind),
                *comp,
            ),
            Self::Expr { op, inputs, output_kind } => {
                // let mut v = vec![];
                // for (s, usage) in inputs {
                //     v.push((s.map_reg(f, *usage), *Z))
                // }
                let mut s = Scalar::Expr {
                    op: *op,
                    inputs: inputs.iter().map(|(s, usage)| (s.map_reg(f, *usage), *usage)).collect(),
                    output_kind: *output_kind,
                };
                s.recompute_output_kind_from_internal_output_kinds();
                s
            }
        }
    }

    pub fn map_scalar<TOtherReg: Reg, F: FnMut(&TReg, VectorComponent, HLSLKind) -> (TOtherReg, VectorComponent)>(&self, f: &mut F, usage: HLSLKind) -> Scalar<TOtherReg> {
        match self {
            Self::Literal(lit) => Scalar::Literal(*lit),
            Self::Component(reg, comp) => {
                let (reg, comp) = f(reg, *comp, usage);
                Scalar::Component(reg, comp)
            }
            Self::Expr { op, inputs, output_kind } => {
                let mut s = Scalar::Expr {
                    op: *op,
                    inputs: inputs.iter().map(|(s, usage)| (s.map_scalar(f, *usage), *usage)).collect(),
                    output_kind: *output_kind,
                };
                s.recompute_output_kind_from_internal_output_kinds();
                s
            }
        }
    }

    pub fn deps(&self, usage: HLSLKind) -> Vec<(TReg, VectorComponent, HLSLKind)> {
        match self {
            Self::Expr { op, inputs, output_kind } => {
                inputs.iter().map(|(i, usage)| {
                    i.deps(*usage)
                }).flatten().collect()
            },
            Self::Component(reg, comp) => vec![(reg.clone(), *comp, usage)],
            Self::Literal(_) => vec![],
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

impl<TReg: VMVector + Reg> VMName for Scalar<TReg> {
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
        self.output_kind()
    }
}
impl<TReg: VMVector + Reg> VMScalar for Scalar<TReg> {}
