use std::convert::TryFrom;

use bitutils::bits;

use super::{
    decode::{decode_enum, AMDILDecodeError},
    enums::{
        ILComponentSelect, ILDivComp, ILModDstComponent, ILOpCode, ILRegType, ILShiftScale,
        IL_Language_Type, IL_Shader_Type, IL_MAJOR_VERSION,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Lang {
    pub client_type: IL_Language_Type,
}
impl TryFrom<u32> for IL_Lang {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            client_type: decode_enum(bits!(tok, 0:7))?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Version {
    pub minor_version: u8,
    pub major_version: u8,
    pub shader_type: IL_Shader_Type,
    pub multipass: bool,
    pub realtime: bool,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Opcode {
    pub code: ILOpCode,
    pub control: u16,
    pub sec_modifier_present: bool,
    pub pri_modifier_present: bool,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Dst {
    pub register_num: u16,
    pub register_type: ILRegType,
    pub modifier_present: bool,
    pub relative_address: RelativeAddressing,
    pub dimension: bool,
    pub immediate_present: bool,
    pub extended: bool,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Dst_Mod {
    pub component_x_r: ILModDstComponent,
    pub component_y_g: ILModDstComponent,
    pub component_z_b: ILModDstComponent,
    pub component_w_a: ILModDstComponent,
    pub clamp: bool,
    pub shift_scale: ILShiftScale,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Src {
    pub register_num: u16,
    pub register_type: ILRegType,
    pub modifier_present: bool,
    pub relative_address: RelativeAddressing,
    pub dimension: bool,
    pub immediate_present: bool,
    pub extended: bool,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Src_Mod {
    pub swizzle_x_r: ILComponentSelect,
    pub negate_x_r: bool,
    pub swizzle_y_g: ILComponentSelect,
    pub negate_y_g: bool,
    pub swizzle_z_b: ILComponentSelect,
    pub negate_z_b: bool,
    pub swizzle_w_a: ILComponentSelect,
    pub negate_w_a: bool,
    /// s = 1.0 - s
    pub invert: bool,
    /// s = s - 0.5
    pub bias: bool,
    /// s = 2.0 * s
    pub x2: bool,
    /// (s = (s < 0) ? –1 : ((s == 0) ? 0 : 1))
    pub sign: bool,
    /// (s = (s < 0) ? –s : s)
    pub abs: bool,
    pub divComp: ILDivComp,
    pub clamp: bool,
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

/// Based on [a Mesa patch](https://cgit.freedesktop.org/mesa/mesa/commit/?id=a75c6163e605f35b14f26930dd9227e4f337ec9e), because AMD don't specify this token anywhere!!
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IL_Rel_Addr {
    pub address_register: u16,
    /// ??? don't know what this means
    pub loop_relative: bool,
    /// Can another IL_Rel_Addr packet follow this one?? what does this mean
    pub component: RelativeAddressing,
}
impl TryFrom<u32> for IL_Rel_Addr {
    type Error = AMDILDecodeError;

    fn try_from(tok: u32) -> Result<Self, Self::Error> {
        Ok(Self {
            address_register: decode_enum(bits!(tok, 0:15))?,
            loop_relative: bits!(tok, 16:16) == 1,
            component: decode_enum(bits!(tok, 17:19))?,
        })
    }
}
