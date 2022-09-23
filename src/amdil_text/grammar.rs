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
    pub instr_mods: Vec<InstrMod>,
    pub args: Vec<Arg>,
}

#[derive(Debug, Clone)]
pub struct InstrMod {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum Arg {
    /// a name = ([a-zA-Z]+[0-9]*)
    Named(String, Vec<ArgMod>),
    /// a hex literal = 0x[0-9A-Fa-f]+
    HexLiteral(u64),
}

#[derive(Debug, Clone)]
pub enum ArgMod {
    /// `\[\d+\]`
    Indexed(u64),
    /// `.[xyzw_]{1,4}`
    Swizzled(MaskedSwizzle),
    // TODO suffixes, more modifiers? e.g. _neg(xyzw)
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
            instr_mods,
            args,
        },
    ))
}
fn parse_instruction_name(
    data: &str,
) -> IResult<&str, (String, Vec<InstrMod>), AMDILTextParseError<&str>> {
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
fn parse_instr_mod(data: &str) -> IResult<&str, InstrMod, AMDILTextParseError<&str>> {
    use nom::character::complete::char;
    let (data, (name, value)) = complete(tuple((
        take_until1("("),
        delimited(char('('), take_until1(")"), char(')')),
    )))(data)?;

    Ok((
        data,
        InstrMod {
            name: name.to_owned(),
            value: value.to_owned(),
        },
    ))
}
fn parse_arg(data: &str) -> IResult<&str, Arg, AMDILTextParseError<&str>> {
    // args are either a hex literal or a named arg
    alt((parse_hex_literal, parse_named_arg))(data)
}
fn parse_named_arg(data: &str) -> IResult<&str, Arg, AMDILTextParseError<&str>> {
    // [a-zA-Z]+[0-9]*

    // [a-zA-Z]+
    let (data, name_start) = take_while1(|c: char| c.is_alphabetic())(data)?;
    // [0-9]*
    let (data, name_end) = take_while(|c: char| c.is_numeric())(data)?;

    let (data, mods) = many0(parse_named_arg_mod)(data)?;

    Ok((data, Arg::Named(name_start.to_owned() + name_end, mods)))
}
fn parse_hex_literal(data: &str) -> IResult<&str, Arg, AMDILTextParseError<&str>> {
    let (data, _) = tag("0x")(data)?;
    let (data, hex_str) = take_while1(|c: char| c.is_ascii_hexdigit())(data)?;
    let hex = u64::from_str_radix(hex_str, 16)
        .map_err(|e| nom::Err::Error(AMDILTextParseError::ParseIntError(e)))?;
    Ok((data, Arg::HexLiteral(hex)))
}
fn parse_named_arg_mod(data: &str) -> IResult<&str, ArgMod, AMDILTextParseError<&str>> {
    alt((parse_index_mod, parse_swizzle_mod))(data)
}
fn parse_index_mod(data: &str) -> IResult<&str, ArgMod, AMDILTextParseError<&str>> {
    let (data, index_str) =
        delimited(tag("["), take_while1(|c: char| c.is_numeric()), tag("]"))(data)?;
    let index = u64::from_str_radix(index_str, 10)
        .map_err(|e| nom::Err::Error(AMDILTextParseError::ParseIntError(e)))?;
    Ok((data, ArgMod::Indexed(index)))
}
fn parse_swizzle_mod(data: &str) -> IResult<&str, ArgMod, AMDILTextParseError<&str>> {
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
    Ok((data, ArgMod::Swizzled(MaskedSwizzle::new(comps))))
}
