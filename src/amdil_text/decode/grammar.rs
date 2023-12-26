use nom::{
    branch::alt,
    bytes::complete::{tag, take_until1, take_while, take_while1},
    character::complete::anychar,
    combinator::{complete, opt},
    multi::{many0, many_m_n, separated_list1, many1},
    sequence::{delimited, tuple},
};

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent};

use super::error::{NomGrammarResult, GrammarError, AMDILError};

#[derive(Debug, Clone)]
pub struct CtrlSpec {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct RegId {
    pub name: String,
    pub rel_addrs: Vec<RegRelativeAddr>,
}

/// Table 2.8 Relative addressing
#[derive(Debug, Clone)]
pub enum RegRelativeAddr {
    Literal(u64),
    Reg(String, VectorComponent),
    RegPlusLiteral(String, VectorComponent, u64),
}

#[derive(Debug, Clone)]
pub struct Dst {
    pub regid: RegId,
    pub write_mask: [DstWrite; 4],
    pub mods: Vec<DstMod>
}

/// Section 2.2.5
/// 
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DstWrite {
    /// The corresponding component is written with the output of the calculation
    Write,
    /// The corresponding component is not written with anything
    NoWrite,
    // /// The corresponding component is written with a literal zero, regardless of the calculation output
    // Write0,
    // /// The corresponding component is written with a 1.0f literal, regardless of the calculation output
    // Write1
}

impl From<[DstWrite; 4]> for MaskedSwizzle {
    fn from(value: [DstWrite; 4]) -> Self {
        MaskedSwizzle::masked_identity(
            value[0] == DstWrite::Write,
            value[1] == DstWrite::Write,
            value[2] == DstWrite::Write,
            value[3] == DstWrite::Write,
        )
    }
}

#[derive(Debug, Clone)]
pub struct Src {
    pub regid: RegId,
    pub mods: Vec<SrcMod>
}

/// Section 3.4 "Destination Modifiers"
#[derive(Debug, Clone)]
pub enum DstMod {
    /// Multiply value by 2 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    X2,
    /// Multiply value by 4 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    X4,
    /// Multiply value by 8 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    X8,
    /// Divide value by 2 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    D2,
    /// Divide value by 4 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    D4,
    /// Divide value by 8 before storing
    /// 
    /// Can be applied to float (or double) but not integer or unsigned operands
    D8,
    /// Saturate or clamp result to [0,1].
    Sat
}

/// Section 3.6 "Source Modifiers"
#[derive(Debug, Clone)]
pub enum SrcMod {
    /// Rearrange and/or replicate components.
    /// 
    /// `.[xyzw_]{1,4}`
    /// 
    /// TODO could include 0 or 1 to force components to 0 or 1
    /// Section 2.2.7 says `1` inserts the bit pattern for floating-point 1 even in an integer context.
    Swizzled(MaskedSwizzle),
    /// Invert components (`1.0 - x`).
    /// 
    /// Table 2.10: single-float only
    Invert,
    /// Components are biased (`x – 0.5`).
    /// 
    /// Table 2.10: single-float only
    Bias,
    /// Multiply components by 2.0.
    /// 
    /// Table 2.10: single-float only
    X2,
    /// Signed scaling. Combined bias and x2 modifiers.
    /// 
    /// Effectively scales the value from [0, 1] to [-1, 1]
    /// 
    /// Table 2.10: single-float only
    Bx2,
    /// Signs Components:
    /// - Components less then 0 become -1.
    /// - Components equal to 0 become 0.
    /// - Components greater then 0 become 1.
    /// 
    /// Table 2.10: single-float only
    Sign,
    /// Performs division based on type.
    /// type: x (no divide), y (divide x by y), z (divide xy by z), w (divide xyz by w)
    /// 
    /// Note: the above is directly from the AMDIL Spec v2,
    /// I think "unknown" is supposed to be x
    /// 
    /// Table 2.10: single-float only
    DivComp(VectorComponent),
    /// Takes the absolute value of components.
    /// 
    /// Section 2.2.7 says this forces the sign bit positive for floats.
    /// Table 2.10: single-float only
    Abs,
    /// Provides per component negate.
    /// 
    /// Note: these components are post-swizzle.
    /// So if you do r0.yzwx_neg(x), that's (-r0.y, r0.z, r0.w, r0.x)
    /// 
    /// Section 2.2.7: This flips the sign of floats, including INF, flips the sign of doubles (thus only touching the first half?),
    /// and takes 2s complement of integers
    Neg(Vec<VectorComponent>),
    /// Saturate to range [0.0, 1.0]
    /// 
    /// Not mentioned in Table 2.10.
    Clamp
}

/// Parse the name of an instruction, plus control specifiers and destination modifiers AND the following space if one was present.
pub fn parse_instruction_name(
    data: &str,
) -> NomGrammarResult<(String, Vec<CtrlSpec>, Vec<DstMod>)> {
    let (data, full_name) = take_while1(|c| c != ' ')(data)?;

    // The components of the instruction name are underscore-separated
    let (_, components) =
        complete(separated_list1(tag("_"), take_while1(|c| c != '_')))(full_name)?;
    let mut instr = components[0].to_owned();
    let mut ctrl_specifiers = vec![];
    let mut dst_mods = vec![];
    for comp in components[1..].into_iter() {
        match *comp {
            "x2" => dst_mods.push(DstMod::X2),
            "x4" => dst_mods.push(DstMod::X4),
            "x8" => dst_mods.push(DstMod::X8),
            "d2" => dst_mods.push(DstMod::D2),
            "d4" => dst_mods.push(DstMod::D4),
            "d8" => dst_mods.push(DstMod::D8),
            "sat" => dst_mods.push(DstMod::Sat),
            _ => if let Ok((_, instr_mod)) = parse_ctrlspec(*comp) {
                ctrl_specifiers.push(instr_mod);
            } else if ctrl_specifiers.len() == 0 {
                instr.push('_');
                instr.push_str(*comp);
            } else {
                // Bad instruction name: non-modifier encountered after modifier
                // TODO return error
                panic!(
                    "bad instruction name '{}': non-modifier '{}' encountered after modifiers {:?}",
                    full_name, *comp, ctrl_specifiers
                );
            }
        }
    }

    let (data, _) = opt(tag(" "))(data)?;

    Ok((data, (instr, ctrl_specifiers, dst_mods)))
}
fn parse_ctrlspec(data: &str) -> NomGrammarResult<CtrlSpec> {
    use nom::character::complete::char;
    let (data, (name, value)) = complete(tuple((
        take_until1("("),
        delimited(char('('), take_until1(")"), char(')')),
    )))(data)?;

    Ok((
        data,
        CtrlSpec {
            name: name.to_owned(),
            value: value.to_owned(),
        },
    ))
}
/// Parse a source (input) register, consuming a following ", " if present
pub fn parse_src(data: &str) -> NomGrammarResult<Src> {
    // args are either a hex literal or a named arg
    let (data, regid) = parse_regid(data)?;
    let (data, mods) = many0(parse_named_arg_mod)(data)?;
    let (data, _) = opt(tag(", "))(data)?;
    Ok((data, Src{ regid, mods }))
}
pub fn parse_many1_src(data: &str) -> NomGrammarResult<Vec<Src>> {
    many1(parse_src)(data)
}
/// Parse a hex literal, sometimes used in place of an input, consuming a following ", " if present
pub fn parse_hex_literal(data: &str) -> NomGrammarResult<u64> {
    let (data, _) = tag("0x")(data)?;
    let (data, hex_str) = take_while1(|c: char| c.is_ascii_hexdigit())(data)?;
    let hex = u64::from_str_radix(hex_str, 16)
        .map_err(|e| AMDILError::ParseIntError(e))?;
    let (data, _) = opt(tag(", "))(data)?;
    Ok((data, hex))
}
/// Parse a destination (output) register, consuming a following ", " if present
pub fn parse_dst<'a>(data: &'a str, mods: Vec<DstMod>) -> NomGrammarResult<Dst> {
    let (data, regid) = parse_regid(data)?;

    let parse_comp = |expected_write: char, data: &'a str| {
        let (data, c) = anychar(data)?;
        if c == expected_write {
            Ok((data, DstWrite::Write))
        } else if c == '_' {
            Ok((data, DstWrite::NoWrite))
        } else {
            Err(nom::Err::Error(AMDILError::BadVectorComponent(c).into()))
        }
        // TODO 0 or 1
    };

    let (data, write_mask) = match tag::<&str, &str, GrammarError<&str>>(".")(data) {
        Ok((data, _)) => {
            let (data, x) = parse_comp('x', data)?;
            let (data, y) = parse_comp('y', data)?;
            let (data, z) = parse_comp('z', data)?;
            let (data, w) = parse_comp('w', data)?;
            (data, [x, y, z, w])
        }
        _ => (data, [DstWrite::Write; 4])
    };

    let (data, _) = opt(tag(", "))(data)?;

    Ok((data, Dst {
        regid,
        write_mask,
        mods,
    }))
}
fn parse_regname(data: &str) -> NomGrammarResult<String> {
    // name: [a-zA-Z]+[0-9]*
    // [a-zA-Z]+
    let (data, name_start) = take_while1(|c: char| c.is_alphabetic())(data)?;
    // [0-9]*
    let (data, name_end) = take_while(|c: char| c.is_numeric())(data)?;

    Ok((data, name_start.to_owned() + name_end))
}
fn parse_reg_rel_literal(data: &str) -> NomGrammarResult<RegRelativeAddr> {
    let (data, index_str) = take_while1(|c: char| c.is_numeric())(data)?;
    let index = u64::from_str_radix(index_str, 10)
        .map_err(|e| AMDILError::ParseIntError(e))?;
    Ok((data, RegRelativeAddr::Literal(index)))
}
fn parse_reg_rel_reg(data: &str) -> NomGrammarResult<RegRelativeAddr> {
    let (data, reg_name) = parse_regname(data)?;
    let (data, _) = tag(".")(data)?;
    let (data, c) = anychar(data)?;
    let comp = match c {
        'x' => VectorComponent::X,
        'y' => VectorComponent::Y,
        'z' => VectorComponent::Z,
        'w' => VectorComponent::W,
        _ => return Err(nom::Err::Error(AMDILError::BadVectorComponent(c).into())),
    };

    match tag::<&str, &str, GrammarError<&str>>("+")(data) {
        Ok((data, _)) => {
            let (data, index_str) = take_while1(|c: char| c.is_numeric())(data)?;
            let index = u64::from_str_radix(index_str, 10)
                .map_err(|e| AMDILError::ParseIntError(e))?;
            Ok((data, RegRelativeAddr::RegPlusLiteral(reg_name, comp, index)))
        },
        Err(_) => Ok((data, RegRelativeAddr::Reg(reg_name, comp)))
    }
} 
fn parse_regid(data: &str) -> NomGrammarResult<RegId> {
    let (data, name) = parse_regname(data)?;

    let (data, rel_addrs) = many0(
        delimited(tag("["), alt((parse_reg_rel_literal, parse_reg_rel_reg)), tag("]"))
    )(data)?;

    Ok((data, RegId{
        name,
        rel_addrs,
    }))
}
fn parse_named_arg_mod(data: &str) -> NomGrammarResult<SrcMod> {
    alt((parse_swizzle_srcmod, parse_plaintext_srcmod, parse_negate_srcmod, parse_divcomp_srcmod))(data)
}
fn parse_swizzle_srcmod(data: &str) -> NomGrammarResult<SrcMod> {
    // `.[xyzw_]{1,4}`
    let (data, _) = tag(".")(data)?;
    let swizzle_parser = |data| {
        let (data, c) = anychar(data)?;
        let comp = match c {
            'x' => Some(VectorComponent::X),
            'y' => Some(VectorComponent::Y),
            'z' => Some(VectorComponent::Z),
            'w' => Some(VectorComponent::W),
            '_' => None,
            // TODO 0 and 1 are also allowed, but not included in VectorComponent yet
            _ => return Err(nom::Err::Error(AMDILError::BadVectorComponent(c).into())),
        };
        Ok((data, comp))
    };
    let (data, comps) = many_m_n(1, 4, swizzle_parser)(data)?;

    let comps = match comps.len() {
        // "r0.x" is equivalent to "r0.xxxx"
        1 => [comps[0], comps[0], comps[0], comps[0]],
        // TODO should these have similar overrides
        2 => [comps[0], comps[1], None, None],
        3 => [comps[0], comps[1], comps[2], None],
        // When they're all specified it's easy
        4 => [comps[0], comps[1], comps[2], comps[3]],
        other => panic!(
            "impossible length returned by nom parser for swizzle: {}",
            other
        ),
    };
    Ok((data, SrcMod::Swizzled(MaskedSwizzle::new(comps))))
}
fn parse_plaintext_srcmod(data: &str) -> NomGrammarResult<SrcMod> {
    let (data, _) = tag("_")(data)?;
    let (data, modname) = take_while1(|c: char| c.is_alphanumeric())(data)?;
    let m = match modname {
        "abs" => SrcMod::Abs,
        "bias" => SrcMod::Bias,
        "bx2" => SrcMod::Bx2,
        "invert" => SrcMod::Invert,
        "sign" => SrcMod::Sign,
        "x2" => SrcMod::X2,
        _ => return Err(nom::Err::Error(AMDILError::BadSrcModifier(modname.to_owned()).into()))
    };
    Ok((data, m))
}
fn parse_divcomp_srcmod(data: &str) -> NomGrammarResult<SrcMod> {
    let (data, _) = tag("_divcomp(")(data)?;
    let (data, c) = anychar(data)?;
    let comp = match c {
        // 'x' doesn't work in divcomp
        'y' => VectorComponent::Y,
        'z' => VectorComponent::Z,
        'w' => VectorComponent::W,
        _ => return Err(nom::Err::Error(AMDILError::BadVectorComponent(c).into())),
    };
    let (data, _) = tag(")")(data)?;
    
    Ok((data, SrcMod::DivComp(comp)))
}
fn parse_negate_srcmod(data: &str) -> NomGrammarResult<SrcMod> {
    let (data, _) = tag("_neg(")(data)?;

    let swizzle_parser = |data| {
        let (data, c) = anychar(data)?;
        let comp = match c {
            'x' => VectorComponent::X,
            'y' => VectorComponent::Y,
            'z' => VectorComponent::Z,
            'w' => VectorComponent::W,
            _ => return Err(nom::Err::Error(AMDILError::BadVectorComponent(c).into())),
        };
        Ok((data, comp))
    };
    let (data, comps) = many_m_n(1, 4, swizzle_parser)(data)?;

    let (data, _) = tag(")")(data)?;
    
    Ok((data, SrcMod::Neg(comps)))
}