//! This module maps [super::MatchableArg] values to [crate::abstract_machine::vector::VectorDataRef].
//!

use std::collections::HashMap;

use crate::{abstract_machine::vector::MaskedSwizzle, amdil_text::vm::AMDILRegister};

use super::{grammar::{Src, RegId, SrcMod, RegRelativeAddr, Dst}, error::AMDILError};

pub type LiteralVec = Box<[u64; 4]>;

pub struct AMDILContext {
    named_literals: HashMap<String, LiteralVec>,
}

impl AMDILContext {
    pub fn new() -> Self {
        AMDILContext { named_literals: HashMap::new() }
    }

    pub fn push_named_literal(&mut self, name: String, literal: LiteralVec) {
        self.named_literals.insert(name, literal);
    }

    pub fn src_to_vector(&self, src: &Src) -> Result<(AMDILRegister, MaskedSwizzle), AMDILError> {
        match &src.mods[..] {
            &[] => self.swizzled_regid_to_maskswizvector(&src.regid, None),
            &[SrcMod::Swizzled(swizzle)] => self.swizzled_regid_to_maskswizvector(&src.regid, Some(swizzle)),
            _ => todo!("more complex parsing, need to handle mods {:?}", src.mods)
        }
    }

    pub fn dst_to_vector(&self, dst: &Dst) -> Result<(AMDILRegister, MaskedSwizzle), AMDILError> {
        if dst.mods.is_empty() {
            self.swizzled_regid_to_maskswizvector(&dst.regid, Some(dst.write_mask.into()))
        } else {
            todo!("more complex parsing, need to handle dstmods {:?}", dst.mods)
        }
    }

    fn swizzled_regid_to_maskswizvector(&self, regid: &RegId, swizzle: Option<MaskedSwizzle>) -> Result<(AMDILRegister, MaskedSwizzle), AMDILError> {
        let swizzle = swizzle.unwrap_or(MaskedSwizzle::identity(4));
        match &regid.rel_addrs[..] {
            &[] => self.swizzled_name_to_named_vector_data_ref(&regid.name, swizzle),
            &[RegRelativeAddr::Literal(idx)] => self.indexed_swizzled_name_to_named_vector_data_ref(&regid.name, idx, swizzle),
            _ => todo!("more complex parsing, need to handle relative addressing {:?}", regid.rel_addrs)
        }
    }
    
    fn swizzled_name_to_named_vector_data_ref(
        &self, 
        name: &String,
        swizzle: MaskedSwizzle,
    ) -> Result<(AMDILRegister, MaskedSwizzle), AMDILError> {
        let vdr = match name.chars().nth(0) {
            Some('l') => {
                let literal = self.named_literals.get(name).unwrap();
                (AMDILRegister::Literal(literal.clone()), MaskedSwizzle::identity(4))
            },
            Some('v') => (AMDILRegister::NamedInputRegister(name.clone()), swizzle),
            Some('o') => (AMDILRegister::NamedOutputRegister(name.clone()), swizzle),
            Some('r') => (AMDILRegister::NamedRegister(name.clone()), swizzle),
            _ => {
                return Err(AMDILError::Generic(format!(
                    "unexpected argument name '{}'",
                    name
                )))
            }
        };
        Ok(vdr)
    }
    
    fn indexed_swizzled_name_to_named_vector_data_ref(
        &self, 
        name: &String,
        idx: u64,
        swizzle: MaskedSwizzle,
    ) -> Result<(AMDILRegister, MaskedSwizzle), AMDILError> {
        if name.starts_with("cb") {
            Ok((AMDILRegister::NamedBuffer{ name: name.clone(), idx }, swizzle))
        } else {
            Err(AMDILError::Generic(format!(
                "unexpected indexable argument name '{}'",
                name
            )))
        }
    }
}