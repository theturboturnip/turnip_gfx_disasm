use super::DecodeError;

pub fn extract_u32(data: &[u8]) -> Result<u32, DecodeError> {
    if data.len() < 4 {
        Err(DecodeError::NotEnoughData)
    } else {
        Ok((data[0] as u32)
            | ((data[1] as u32) << 8)
            | ((data[2] as u32) << 16)
            | ((data[3] as u32) << 24))
    }
}
