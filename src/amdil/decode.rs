pub enum AMDILDecodeError {
    BadValue(&'static str, u64),
    NotEnoughData,
    MajorVersionMismatch { expected: u64, actual: u64 },
}

use std::convert::TryInto;

use num_traits::FromPrimitive;

use super::tokens::{IL_Lang, IL_Version};

pub fn decode_enum<T: FromPrimitive>(bits: u32) -> Result<T, AMDILDecodeError> {
    T::from_u32(bits)
        .ok_or_else(|| AMDILDecodeError::BadValue(std::any::type_name::<T>(), bits.into()))
}

enum TokenGroup {
    Language(IL_Lang),
}

fn decode_raw_token<'a>(data: &'a [u8]) -> Result<(&'a [u8], u32), AMDILDecodeError> {
    if data.len() < 4 {
        Err(AMDILDecodeError::NotEnoughData)
    } else {
        // little-endian u32
        let val = data[0] as u32
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24);
        Ok((&data[4..], val))
    }
}

fn decode_token<'a, T: TryFrom<u32, Error = AMDILDecodeError>>(
    data: &'a [u8],
) -> Result<(&'a [u8], T), AMDILDecodeError> {
    let (data, raw_tok) = decode_raw_token(data)?;
    Ok((data, raw_tok.try_into()?))
}

fn decode_amdil_token_stream<'a>(data: &'a [u8]) -> Result<Vec<TokenGroup>, AMDILDecodeError> {
    let (data, lang) = decode_token::<IL_Lang>(data)?;
    let (data, version) = decode_token::<IL_Version>(data)?;
    todo!()
}
