//! This module maps [super::MatchableArg] values to [crate::abstract_machine::vector::VectorDataRef].
//!

use std::collections::HashMap;

use crate::{abstract_machine::{vector::{MaskedSwizzle, VectorComponent}, expr::{ContigSwizzle, Scalar, Reg}}, amdil_text::vm::{AMDILRegister, AMDILVector}, hlsl::{kinds::{HLSLKind, HLSLNumericKind, HLSLKindBitmask}, syntax::{HLSLOperator, ArithmeticOp, UnaryOp}}};

use super::{grammar::{Src, RegId, SrcMod, RegRelativeAddr, Dst, SrcMods}, error::AMDILError};

pub type LiteralVec = Box<[u64; 4]>;

pub struct AMDILContext {
    named_literals: HashMap<String, LiteralVec>,
}

pub enum InstructionInput { 
    Src(Src),
    Texture(u64),
}

#[derive(Debug, Clone)]
enum RegOrLiteral<'a> {
    Reg(AMDILRegister),
    Literal(&'a LiteralVec)
}

impl AMDILContext {
    pub fn new() -> Self {
        AMDILContext { named_literals: HashMap::new() }
    }

    pub fn push_named_literal(&mut self, name: String, literal: LiteralVec) {
        self.named_literals.insert(name, literal);
    }

    pub fn input_to_vector(&self, input: InstructionInput, mask: MaskedSwizzle) -> Result<AMDILVector, AMDILError> {
        match input {
            InstructionInput::Src(mut src) => {
                src.apply_mask(mask);
                self.src_to_vector(src)
            },
            InstructionInput::Texture(idx) => Ok(amdil_vec_of(AMDILRegister::Texture(idx), MaskedSwizzle::identity(1))),
        }
    }

    pub fn input_to_scalar(&self, input: InstructionInput) -> Result<Scalar<AMDILRegister>, AMDILError> {
        match input {
            InstructionInput::Src(src) => {
                let reg = self.regid_to_register(src.regid)?;
        
                let comp = match src.mods.swizzle {
                    Some(swizzle) => swizzle.0[0].expect("Complex swizzle encountered for scalar"),
                    None => VectorComponent::X,
                };

                Ok(Self::build_scalar(0, reg, comp, &src.mods))
            },
            InstructionInput::Texture(idx) => Ok(Scalar::Component(
                AMDILRegister::Texture(idx),
                VectorComponent::X,
            )),
        }
    }

    fn build_scalar(i: usize, reg: RegOrLiteral, comp: VectorComponent, mods: &SrcMods) -> Scalar<AMDILRegister> {
        const FLOAT: HLSLKind = HLSLKind::NUMERIC_FLOAT;

        // Apply the modifiers in order:
        // 1. swizzle  -  rearranges  and/or  replicates  components - assumed to have already been applied
        let s = match reg {
            RegOrLiteral::Literal(lit) => Scalar::Literal(lit[comp.into_index()]),
            RegOrLiteral::Reg(reg) => Scalar::Component(reg, comp),
        };
        // 2. _invert  -  inverts  components  1  -  x
        let s = if mods.invert {
            Scalar::Expr {
                op: HLSLOperator::Arithmetic(ArithmeticOp::Minus),
                inputs: vec![
                    (Scalar::Literal(1.0f32.to_bits() as u64), FLOAT),
                    (s, FLOAT),
                ],
                output_kind: FLOAT
            }
        } else { s };
        // 3. _bias  -  biases  components  x  -  0.5
        let s = if mods.bias {
            Scalar::Expr {
                op: HLSLOperator::Arithmetic(ArithmeticOp::Minus),
                inputs: vec![
                    (s, FLOAT),
                    (Scalar::Literal(0.5f32.to_bits() as u64), FLOAT),
                ],
                output_kind: FLOAT
            }
        } else { s };
        // 4. _x2  -  multiplies  components  by  2.0
        // _bx2  -  signed  scaling:  combines  _bias  and  _x2  modifiers
        let s = if mods.x2 {
            Scalar::Expr {
                op: HLSLOperator::Arithmetic(ArithmeticOp::Times),
                inputs: vec![
                    (s, FLOAT),
                    (Scalar::Literal(2.0f32.to_bits() as u64), FLOAT),
                ],
                output_kind: FLOAT
            }
        } else { s };
        // 5. _sign  -  signs  components:  components  <  0  become  -1;  
        // components = 0  become  0;  components  >  1  become  1
        let s = if mods.sign {
            todo!("Signof")
        } else { s };
        // 6. _divComp(type) - performs division based on divcomp value; type 
        // y,  z,  w  unknown
        let s = match mods.div_comp {
            Some(_) => todo!("divcomp"),
            None => s,
        };
        // 7. _abs  -  takes  the  absolute  value  of  components
        let s = if mods.abs {
            todo!("abs")
        } else { s };
        // 8. neg(comp)  -  provides  per-component  negate
        let s = if mods.neg[i] {
            Scalar::Expr {
                op: HLSLOperator::Unary(UnaryOp::Negate),
                inputs: vec![
                    (s, HLSLKindBitmask::NUMERIC.into()),
                ],
                output_kind: HLSLKindBitmask::NUMERIC.into()
            }
        } else { s };
        // 9. clamp  -  clamps  the  value
        let s = if mods.clamp {
            todo!("clamp")
        } else { s };

        s   
    }

    pub fn src_to_vector(&self, src: Src) -> Result<AMDILVector, AMDILError> {
        let reg = self.regid_to_register(src.regid)?;
        
        let comps: ContigSwizzle = match src.mods.swizzle {
            Some(swizzle) => swizzle.0.iter().filter_map(|c| *c).collect(),
            None => [VectorComponent::X, VectorComponent::Y, VectorComponent::Z, VectorComponent::W].into(),
        };

        let scalars = comps.iter()
            .enumerate()
            .map(|(i, comp)| {
                Self::build_scalar(i, reg.clone(), *comp, &src.mods)
            }).collect();

        Ok(AMDILVector::of_scalars(scalars))
    }

    pub fn dst_to_vector(&self, dst: Dst) -> Result<(AMDILRegister, ContigSwizzle), AMDILError> {
        if dst.mods.is_empty() {
            let reg = match self.regid_to_register(dst.regid)? {
                RegOrLiteral::Reg(reg) => reg,
                RegOrLiteral::Literal(_) => panic!("Tried to use a literal as a dst, this is never possible"),
            };

            let comps: MaskedSwizzle = dst.write_mask.into();
            let comps: ContigSwizzle = comps.0.iter().filter_map(|c| *c).collect();

            Ok((reg, comps))
        } else {
            todo!("more complex parsing, need to handle dstmods {:?}", dst.mods)
        }
    }

    fn regid_to_register(
        &self,
        regid: RegId,
    ) -> Result<RegOrLiteral, AMDILError> {
        let name = regid.name;
        let base_reg = match (name.chars().nth(0), &regid.rel_addrs[..]) {
            (Some('l'), &[]) => {
                return Ok(RegOrLiteral::Literal(self.named_literals.get(&name).unwrap()));
            },
            (Some('v'), &[]) => AMDILRegister::NamedInputRegister(name),
            (Some('o'), &[]) => AMDILRegister::NamedOutputRegister(name),
            (Some('r'), &[]) => AMDILRegister::NamedRegister(name),
            (Some('c'), &[RegRelativeAddr::Literal(idx)]) => AMDILRegister::NamedBuffer { name, idx },
            (_, &[]) => {
                return Err(AMDILError::Generic(format!(
                    "unexpected argument name '{}'",
                    name
                )))
            }
            _ => todo!("more complex parsing, need to handle relative addressing {:?}", regid.rel_addrs)
        };
        Ok(RegOrLiteral::Reg(base_reg))
    }
}

fn swizzle_to_contig(swizzle: MaskedSwizzle) -> ContigSwizzle {
    swizzle.0.iter().filter_map(|comp_opt| *comp_opt).collect()
}

fn amdil_vec_of(reg: AMDILRegister, swizzle: MaskedSwizzle) -> AMDILVector {
    let kind = reg.output_kind();
    AMDILVector::PureSwizzle(reg, swizzle_to_contig(swizzle), kind)
}