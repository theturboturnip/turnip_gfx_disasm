//! This module maps [super::MatchableArg] values to [crate::abstract_machine::vector::VectorDataRef].
//!

use std::collections::HashMap;

use crate::{abstract_machine::{vector::{MaskedSwizzle, VectorComponent}, expr::{ContigSwizzle, Scalar, Reg, IndexedReg, INDEX_KIND, Vector}}, amdil_text::vm::{AMDILRegister, AMDILVector, AMDILDeclaration}, hlsl::{kinds::{HLSLKind, HLSLNumericKind, HLSLKindBitmask}, syntax::{HLSLOperator, ArithmeticOp, UnaryOp, NumericIntrinsic}}};

use super::{grammar::{Src, RegId, SrcMod, RegRelativeAddr, Dst, SrcMods}, error::AMDILError, Instruction};

pub type LiteralVec = Box<[u64; 4]>;

enum IfInProgress {
    ParsingTrue{
        cond: Scalar<AMDILRegister>, 
        if_true: Vec<Instruction>,
    },
    ParsingFals {
        cond: Scalar<AMDILRegister>,
        if_true: Vec<Instruction>,
        if_fals: Vec<Instruction>,
    }
}

pub struct AMDILContext {
    named_literals: HashMap<String, LiteralVec>,
    named_reg_declarations: HashMap<String, AMDILRegister>,
    textures: HashMap<u64, AMDILRegister>,
    if_stack: Vec<IfInProgress>,
    instructions: Vec<Instruction>,
}
pub enum InstructionInput { 
    Src(Src),
    Texture(u64),
}

#[derive(Debug, Clone)]
enum RegOrLiteral<'a> {
    Reg(IndexedReg<AMDILRegister>),
    Literal(&'a LiteralVec)
}

impl AMDILContext {
    pub fn new() -> Self {
        AMDILContext {
            named_literals: HashMap::new(),
            named_reg_declarations: HashMap::new(),
            textures: HashMap::new(),
            instructions: vec![],
            if_stack: vec![],
        }
    }

    pub fn if_depth(&self) -> usize {
        self.if_stack.len()
    }

    pub fn push_instruction(&mut self, i: Instruction) {
        let current_consumer = match self.if_stack.last_mut() {
            Some(IfInProgress::ParsingTrue{ if_true, .. }) => if_true,
            Some(IfInProgress::ParsingFals{ if_fals, .. }) => if_fals,
            None => &mut self.instructions,
        };
        current_consumer.push(i);
    }

    pub fn finalize(self) -> (Vec<AMDILRegister>, Vec<Instruction>) {
        if !self.if_stack.is_empty() {
            panic!("Program had inbalanced ifs and endifs")
        }
        let mut io_declarations: Vec<_> = self.named_reg_declarations.into_values().collect();
        io_declarations.extend(self.textures.into_values());
        (io_declarations, self.instructions)
    }

    pub fn start_if(&mut self, cond: Scalar<AMDILRegister>) {
        self.if_stack.push(IfInProgress::ParsingTrue{
            cond,
            if_true: vec![]
        })
    }
    pub fn encounter_else(&mut self) {
        let current_if = self.if_stack
            .pop()
            .expect("Encountered an else when not inside an if");
        let else_if = match current_if {
            IfInProgress::ParsingTrue{ cond, if_true } => IfInProgress::ParsingFals {
                cond,
                if_true,
                if_fals: vec![],
            },
            IfInProgress::ParsingFals { .. } => panic!("Encountered an else when already else-ing"),
        };
        self.if_stack.push(else_if);
    }
    pub fn end_if(&mut self) -> Instruction {
        match self.if_stack.pop().expect("Encountered and endif when not inside an if") {
            IfInProgress::ParsingTrue { cond, if_true } => {
                Instruction::If { cond, if_true, if_fals: vec![] }
            },
            IfInProgress::ParsingFals { cond, if_true, if_fals } => {
                Instruction::If { cond, if_true, if_fals }
            },
        }
    }

    pub fn push_declaration(&mut self, reg: AMDILDeclaration) {
        match reg {
            AMDILDeclaration::Texture2D(id) => {
                self.textures.insert(id, AMDILRegister::Texture2D(id));
            },
            AMDILDeclaration::Texture3D(id) => {
                self.textures.insert(id, AMDILRegister::Texture3D(id));
            },
            AMDILDeclaration::TextureCube(id) => {
                self.textures.insert(id, AMDILRegister::TextureCube(id));
            },
            AMDILDeclaration::NamedLiteral(name, literal) => {
                self.named_literals.insert(name, Box::new(literal));
            },
            AMDILDeclaration::ConstBuffer { name, dims } => {
                self.named_reg_declarations.insert(name.clone(), AMDILRegister::ConstBuffer { name, dims });
            },
            AMDILDeclaration::NamedInputRegister { name, reg_type: _ } => {
                self.named_reg_declarations.insert(name.clone(), AMDILRegister::NamedInputRegister(name));
            },
            AMDILDeclaration::NamedOutputRegister { name, reg_type: _ } => {
                self.named_reg_declarations.insert(name.clone(), AMDILRegister::NamedOutputRegister(name));
            },
        }
    }

    pub fn input_to_vector(&self, input: InstructionInput, mask: MaskedSwizzle) -> Result<AMDILVector, AMDILError> {
        match input {
            InstructionInput::Src(mut src) => {
                src.apply_mask(mask);
                self.src_to_vector(src)
            },
            InstructionInput::Texture(idx) => Ok(Vector::of_reg(self.textures.get(&idx).unwrap().clone().into())),
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
                IndexedReg::new_direct(self.textures.get(&idx).unwrap().clone()),
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
            Scalar::Expr {
                // In this context it forces float, even though HLSL abs() can work on sint too
                op: HLSLOperator::NumericI(NumericIntrinsic::Abs),
                inputs: vec![
                    (s, HLSLKind::NUMERIC_FLOAT),
                ],
                output_kind: HLSLKind::NUMERIC_FLOAT
            }
        } else { s };
        // 8. neg(comp)  -  provides  per-component  negate
        let s = if mods.neg[i] {
            Scalar::Expr {
                op: HLSLOperator::Unary(UnaryOp::Negate),
                inputs: vec![
                    (s, HLSLKind::NUMERIC),
                ],
                output_kind: HLSLKind::NUMERIC
            }
        } else { s };
        // 9. clamp  -  clamps  the  value
        let s = if mods.clamp {
            Scalar::Expr {
                op: HLSLOperator::NumericI(NumericIntrinsic::Saturate),
                inputs: vec![
                    (s, HLSLKind::NUMERIC_FLOAT),
                ],
                output_kind: HLSLKind::NUMERIC_FLOAT
            }
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

    pub fn dst_to_vector(&self, dst: Dst) -> Result<(IndexedReg<AMDILRegister>, ContigSwizzle), AMDILError> {
        let reg = match self.regid_to_register(dst.regid)? {
            RegOrLiteral::Reg(reg) => reg,
            RegOrLiteral::Literal(_) => panic!("Tried to use a literal as a dst, this is never possible"),
        };

        let comps: MaskedSwizzle = dst.write_mask.into();
        let comps: ContigSwizzle = comps.0.iter().filter_map(|c| *c).collect();

        Ok((reg, comps))
    }

    fn regid_to_register(
        &self,
        regid: RegId,
    ) -> Result<RegOrLiteral, AMDILError> {
        let name = regid.name;
        let base_reg = match name.chars().nth(0) {
            Some('l') => {
                return Ok(RegOrLiteral::Literal(self.named_literals.get(&name).unwrap()))
            },
            Some('v') | Some('o') | Some('c') => self.named_reg_declarations.get(&name).unwrap().clone(),
            Some('r') => AMDILRegister::NamedRegister(name),
            _ => {
                return Err(AMDILError::Generic(format!(
                    "unexpected argument name '{}'",
                    name
                )))
            }
        };
        let idxs = regid.rel_addrs.into_iter().map(|r| match r {
            RegRelativeAddr::Literal(l) => Scalar::Literal(l),
            RegRelativeAddr::Reg(name, comp) => {
                let reg = AMDILRegister::NamedRegister(name);
                Scalar::Component(IndexedReg::new_direct(reg), comp)
            },
            RegRelativeAddr::RegPlusLiteral(name, comp, offset) => {
                let reg = AMDILRegister::NamedRegister(name);
                let reg_s = Scalar::Component(IndexedReg::new_direct(reg), comp);
                let offset_s = Scalar::Literal(offset);
                Scalar::Expr {
                    op: HLSLOperator::Arithmetic(ArithmeticOp::Plus),
                    inputs: vec![
                        (reg_s, INDEX_KIND),
                        (offset_s, INDEX_KIND),
                    ],
                    output_kind: INDEX_KIND
                }
            },
        }).collect();
        Ok(RegOrLiteral::Reg(IndexedReg::new(base_reg, idxs)))
    }
}

fn swizzle_to_contig(swizzle: MaskedSwizzle) -> ContigSwizzle {
    swizzle.0.iter().filter_map(|comp_opt| *comp_opt).collect()
}
