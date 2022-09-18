use std::convert::TryFrom;

use bitutils::bits;

use super::{
    decode::{decode_enum, AMDILDecodeError},
    enums::{
        ILComponentSelect, ILDivComp, ILModDstComponent, ILOpCode, ILRegType, ILShiftScale,
        IL_Language_Type, IL_Shader_Type, IL_MAJOR_VERSION,
    },
};

pub struct IL_Lang {
    client_type: IL_Language_Type,
}
impl TryFrom<u32> for IL_Lang {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            client_type: decode_enum(bits!(tok, 0:7))?,
        })
    }
}

pub struct IL_Version {
    minor_version: u8,
    major_version: u8,
    shader_type: IL_Shader_Type,
    multipass: bool,
    realtime: bool,
}
impl TryFrom<u32> for IL_Version {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        let tok = Self {
            minor_version: bits!(tok, 0:7) as u8,
            major_version: bits!(tok, 8:15) as u8,
            shader_type: decode_enum(bits!(tok, 16:23))?,
            multipass: bits!(tok, 24:24) == 1,
            realtime: bits!(tok, 25:25) == 1,
        };
        if IL_MAJOR_VERSION != tok.major_version.into() {
            Err(AMDILDecodeError::MajorVersionMismatch {
                expected: IL_MAJOR_VERSION,
                actual: tok.major_version.into(),
            })
        } else {
            Ok(tok)
        }
    }
}

pub struct IL_Opcode {
    code: ILOpCode,
    control: u16,
    sec_modifier_present: bool,
    pri_modifier_present: bool,
}
impl TryFrom<u32> for IL_Opcode {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            code: decode_enum(bits!(tok, 0:15))?,
            control: bits!(tok, 16:29) as u16,
            sec_modifier_present: bits!(tok, 30:30) == 1,
            pri_modifier_present: bits!(tok, 31:31) == 1,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, FromPrimitive)]
pub enum RelativeAddressing {
    Absolute = 0,
    Relative = 1,
    RegisterRelative = 2,
}
pub struct IL_Dst {
    register_num: u16,
    register_type: ILRegType,
    modifier_present: bool,
    relative_address: RelativeAddressing,
    dimension: bool,
    immediate_present: bool,
    extended: bool,
}
impl TryFrom<u32> for IL_Dst {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            register_num: bits!(tok, 0:15) as u16,
            register_type: decode_enum(bits!(tok, 16:21))?,
            modifier_present: bits!(tok, 22:22) == 1,
            relative_address: decode_enum(bits!(tok, 23:24))?,
            dimension: bits!(tok, 25:25) == 1,
            immediate_present: bits!(tok, 26:26) == 1,
            extended: bits!(tok, 31:31) == 1,
        })
    }
}

pub struct IL_Dst_Mod {
    component_x_r: ILModDstComponent,
    component_y_g: ILModDstComponent,
    component_z_b: ILModDstComponent,
    component_w_a: ILModDstComponent,
    clamp: bool,
    shift_scale: ILShiftScale,
}
impl TryFrom<u32> for IL_Dst_Mod {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            component_x_r: decode_enum(bits!(tok, 0:1))?,
            component_y_g: decode_enum(bits!(tok, 2:3))?,
            component_z_b: decode_enum(bits!(tok, 4:5))?,
            component_w_a: decode_enum(bits!(tok, 6:7))?,
            clamp: bits!(tok, 8:8) == 1,
            shift_scale: decode_enum(bits!(tok, 9:12))?,
        })
    }
}

pub struct IL_Src {
    register_num: u16,
    register_type: ILRegType,
    modifier_present: bool,
    relative_address: RelativeAddressing,
    dimension: bool,
    immediate_present: bool,
    extended: bool,
}
impl TryFrom<u32> for IL_Src {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            register_num: bits!(tok, 0:15) as u16,
            register_type: decode_enum(bits!(tok, 16:21))?,
            modifier_present: bits!(tok, 22:22) == 1,
            relative_address: decode_enum(bits!(tok, 23:24))?,
            dimension: bits!(tok, 25:25) == 1,
            immediate_present: bits!(tok, 26:26) == 1,
            extended: bits!(tok, 31:31) == 1,
        })
    }
}

/// See Section 2.2.1 (p27) for precedence of operations
pub struct IL_Src_Mod {
    swizzle_x_r: ILComponentSelect,
    negate_x_r: bool,
    swizzle_y_g: ILComponentSelect,
    negate_y_g: bool,
    swizzle_z_b: ILComponentSelect,
    negate_z_b: bool,
    swizzle_w_a: ILComponentSelect,
    negate_w_a: bool,
    /// s = 1.0 - s
    invert: bool,
    /// s = s - 0.5
    bias: bool,
    /// s = 2.0 * s
    x2: bool,
    /// (s = (s < 0) ? –1 : ((s == 0) ? 0 : 1))
    sign: bool,
    /// (s = (s < 0) ? –s : s)
    abs: bool,
    divComp: ILDivComp,
    clamp: bool,
}
impl TryFrom<u32> for IL_Src_Mod {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            swizzle_x_r: decode_enum(bits!(tok, 0:2))?,
            negate_x_r: bits!(tok, 3:3) == 1,
            swizzle_y_g: decode_enum(bits!(tok, 4:6))?,
            negate_y_g: bits!(tok, 7:7) == 1,
            swizzle_z_b: decode_enum(bits!(tok, 8:10))?,
            negate_z_b: bits!(tok, 11:11) == 1,
            swizzle_w_a: decode_enum(bits!(tok, 12:14))?,
            negate_w_a: bits!(tok, 15:15) == 1,
            invert: bits!(tok, 16:16) == 1,
            bias: bits!(tok, 17:17) == 1,
            x2: bits!(tok, 18:18) == 1,
            sign: bits!(tok, 19:19) == 1,
            abs: bits!(tok, 20:20) == 1,
            divComp: decode_enum(bits!(tok, 21:23))?,
            clamp: bits!(tok, 24:24) == 1,
        })
    }
}
