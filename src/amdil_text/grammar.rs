use std::num::ParseIntError;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until1, take_while, take_while1},
    character::complete::{anychar, line_ending},
    combinator::{complete, opt},
    error::{ErrorKind, ParseError},
    multi::{many0, many_m_n, separated_list0, separated_list1},
    sequence::{delimited, tuple},
    IResult,
};

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent};

#[derive(Debug, Clone)]
pub struct Instruction {
    /// Instruction name
    ///
    /// ```text
    /// [a-zA-Z][a-zA-Z0-9]+           // Base name
    /// (                              // Modifier-or-extra-name-part
    ///     _                          // Modifier separator (or just part of the base name)
    ///     [a-zA-Z0-9]+               // Modifier name (or just part of the base name)
    ///     (                          // Modifier argument (if present, preceding separator+name were a modifier)
    ///         \([a-zA-Z0-9]+\)
    ///     )?
    /// )*
    /// ```
    pub instr: String,
    pub ctrl_specifiers: Vec<CtrlSpec>,
    // pub dst: 
    pub args: Vec<Src>,
}

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
    id: RegId,
    write_mask: [DstWrite; 4],
    mods: Vec<DstMod>
}

/// Section 2.2.5
/// 
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Src {
    /// a name = ([a-zA-Z]+[0-9]*)
    Named(RegId, Vec<SrcMod>),
    /// a hex literal = 0x[0-9A-Fa-f]+
    HexLiteral(u64),
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

#[derive(Debug, PartialEq)]
pub enum AMDILTextParseError<I> {
    Nom(I, ErrorKind),
    ParseIntError(ParseIntError),
    BadVectorComponent(char),
}
impl<I> ParseError<I> for AMDILTextParseError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        AMDILTextParseError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}

pub fn parse_lines(data: &str) -> IResult<&str, Vec<Instruction>, AMDILTextParseError<&str>> {
    let (data, lines) = separated_list0(
        line_ending,
        take_while(|c| c != '\r' && c != '\n' && c != '\0'),
    )(data)?;
    let instrs: Result<Vec<Instruction>, nom::Err<AMDILTextParseError<&str>>> = lines
        .into_iter()
        // Filter out empty lines
        .filter(|data| data.len() > 0)
        .map(|data| Ok(parse_line(data)?.1))
        .collect();

    Ok((data, instrs?))
}
fn parse_line(data: &str) -> IResult<&str, Instruction, AMDILTextParseError<&str>> {
    let (data, (instr, instr_mods)) = parse_instruction_name(data)?;

    // If there is a space
    let (data, args) = if let (data, Some(_)) = opt(tag(" "))(data)? {
        // Take arguments
        separated_list0(tag(", "), parse_arg)(data)?
    } else {
        (data, vec![])
    };

    if data.len() != 0 {
        println!("warning: didn't parse '{}'", data);
    }

    Ok((
        data,
        Instruction {
            instr: instr.to_owned(),
            ctrl_specifiers: instr_mods,
            args,
        },
    ))
}
fn parse_instruction_name(
    data: &str,
) -> IResult<&str, (String, Vec<CtrlSpec>), AMDILTextParseError<&str>> {
    let (remaining_data, full_name) = take_while1(|c| c != ' ')(data)?;

    // The components of the instruction name are underscore-separated
    let (_, components) =
        complete(separated_list1(tag("_"), take_while1(|c| c != '_')))(full_name)?;
    let mut instr = components[0].to_owned();
    let mut instr_mods = vec![];
    for comp in components[1..].into_iter() {
        if let Ok((_, instr_mod)) = parse_instr_mod(*comp) {
            instr_mods.push(instr_mod);
        } else if instr_mods.len() == 0 {
            instr.push('_');
            instr.push_str(*comp);
        } else {
            // Bad instruction name: non-modifier encountered after modifier
            // TODO return error
            panic!(
                "bad instruction name '{}': non-modifier '{}' encountered after modifiers {:?}",
                full_name, *comp, instr_mods
            );
        }
    }

    Ok((remaining_data, (instr, instr_mods)))
}
fn parse_instr_mod(data: &str) -> IResult<&str, CtrlSpec, AMDILTextParseError<&str>> {
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
fn parse_arg(data: &str) -> IResult<&str, Src, AMDILTextParseError<&str>> {
    // args are either a hex literal or a named arg
    alt((parse_hex_literal, parse_named_arg))(data)
}
fn parse_regname(data: &str) -> IResult<&str, String, AMDILTextParseError<&str>> {
    // name: [a-zA-Z]+[0-9]*
    // [a-zA-Z]+
    let (data, name_start) = take_while1(|c: char| c.is_alphabetic())(data)?;
    // [0-9]*
    let (data, name_end) = take_while(|c: char| c.is_numeric())(data)?;

    Ok((data, name_start.to_owned() + name_end))
}
fn parse_reg_rel_literal(data: &str) -> IResult<&str, RegRelativeAddr, AMDILTextParseError<&str>> {
    let (data, index_str) = take_while1(|c: char| c.is_numeric())(data)?;
    let index = u64::from_str_radix(index_str, 10)
        .map_err(|e| nom::Err::Error(AMDILTextParseError::ParseIntError(e)))?;
    Ok((data, RegRelativeAddr::Literal(index)))
}
fn parse_reg_rel_reg(data: &str) -> IResult<&str, RegRelativeAddr, AMDILTextParseError<&str>> {
    let (data, reg_name) = parse_regname(data)?;
    let (data, _) = tag(".")(data)?;
    let (data, c) = anychar(data)?;
    let comp = match c {
        'x' => VectorComponent::X,
        'y' => VectorComponent::Y,
        'z' => VectorComponent::Z,
        'w' => VectorComponent::W,
        _ => return Err(nom::Err::Error(AMDILTextParseError::BadVectorComponent(c))),
    };

    match tag::<&str, &str, AMDILTextParseError<&str>>("+")(data) {
        Ok((data, _)) => {
            let (data, index_str) = take_while1(|c: char| c.is_numeric())(data)?;
            let index = u64::from_str_radix(index_str, 10)
                .map_err(|e| nom::Err::Error(AMDILTextParseError::ParseIntError(e)))?;
            Ok((data, RegRelativeAddr::RegPlusLiteral(reg_name, comp, index)))
        },
        Err(_) => Ok((data, RegRelativeAddr::Reg(reg_name, comp)))
    }
} 
fn parse_regid(data: &str) -> IResult<&str, RegId, AMDILTextParseError<&str>> {

    let (data, name) = parse_regname(data)?;

    let (data, rel_addrs) = many0(
        delimited(tag("["), alt((parse_reg_rel_literal, parse_reg_rel_reg)), tag("]"))
    )(data)?;

    Ok((data, RegId{
        name,
        rel_addrs,
    }))
}
fn parse_named_arg(data: &str) -> IResult<&str, Src, AMDILTextParseError<&str>> {
    let (data, regid) = parse_regid(data)?;
    let (data, mods) = many0(parse_named_arg_mod)(data)?;
    Ok((data, Src::Named(regid, mods)))
}
fn parse_hex_literal(data: &str) -> IResult<&str, Src, AMDILTextParseError<&str>> {
    let (data, _) = tag("0x")(data)?;
    let (data, hex_str) = take_while1(|c: char| c.is_ascii_hexdigit())(data)?;
    let hex = u64::from_str_radix(hex_str, 16)
        .map_err(|e| nom::Err::Error(AMDILTextParseError::ParseIntError(e)))?;
    Ok((data, Src::HexLiteral(hex)))
}
fn parse_named_arg_mod(data: &str) -> IResult<&str, SrcMod, AMDILTextParseError<&str>> {
    // alt((parse_index_mod, parse_swizzle_mod))(data)
    parse_swizzle_mod(data)
}
fn parse_swizzle_mod(data: &str) -> IResult<&str, SrcMod, AMDILTextParseError<&str>> {
    // `.[xyzw_]{1,3}`
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
            _ => return Err(nom::Err::Error(AMDILTextParseError::BadVectorComponent(c))),
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
