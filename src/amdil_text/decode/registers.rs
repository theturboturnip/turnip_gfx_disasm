//! This module maps [super::MatchableArg] values to [crate::abstract_machine::vector::VectorDataRef].
//!

use crate::abstract_machine::vector::{MaskedSwizzle, VectorDataRef};

use super::{AMDILTextDecodeError, MatchableArg};

pub fn arg_as_vector_data_ref(arg: &MatchableArg) -> Result<VectorDataRef, AMDILTextDecodeError> {
    let vdr = match arg {
        MatchableArg::Named(name) => {
            swizzled_name_to_named_vector_data_ref(name, MaskedSwizzle::identity(4))?
        }
        MatchableArg::NamedSwizzled(name, swizzle) => {
            swizzled_name_to_named_vector_data_ref(name, *swizzle)?
        }
        MatchableArg::NamedIndexed(name, idx) => {
            indexed_swizzled_name_to_named_vector_data_ref(name, *idx, MaskedSwizzle::identity(4))?
        }
        MatchableArg::NamedIndexedSwizzled(name, idx, swizzle) => {
            indexed_swizzled_name_to_named_vector_data_ref(name, *idx, *swizzle)?
        }
        MatchableArg::Complex(arg) => {
            todo!("more complex parsing, need to handle {:?}", arg)
        }
        MatchableArg::HexLiteral(_) => {
            todo!("scalar hex literal passed to arg_as_vector_data_ref. How to handle?")
        }
    };

    Ok(vdr)
}

fn swizzled_name_to_named_vector_data_ref(
    name: &String,
    swizzle: MaskedSwizzle,
) -> Result<VectorDataRef, AMDILTextDecodeError> {
    let vdr = match name.chars().nth(0) {
        Some('l') => VectorDataRef::named_literal(name.clone(), swizzle),
        Some('v') => VectorDataRef::named_input_register(name.clone(), swizzle),
        Some('o') => VectorDataRef::named_output_register(name.clone(), swizzle),
        Some('r') => VectorDataRef::named_register(name.clone(), swizzle),
        _ => {
            return Err(AMDILTextDecodeError::Generic(format!(
                "unexpected argument name '{}'",
                name
            )))
        }
    };
    Ok(vdr)
}

fn indexed_swizzled_name_to_named_vector_data_ref(
    name: &String,
    idx: u64,
    swizzle: MaskedSwizzle,
) -> Result<VectorDataRef, AMDILTextDecodeError> {
    if name.starts_with("cb") {
        Ok(VectorDataRef::named_buffer(name.clone(), idx, swizzle))
    } else {
        Err(AMDILTextDecodeError::Generic(format!(
            "unexpected indexable argument name '{}'",
            name
        )))
    }
}
