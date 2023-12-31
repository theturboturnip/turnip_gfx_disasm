use nom::{
    branch::alt,
    bytes::complete::{tag, take_until1, take_while, take_while1, take},
    character::complete::anychar,
    combinator::{complete, opt},
    multi::{many0, many_m_n, separated_list1, many1},
    sequence::{delimited, tuple},
};

use crate::abstract_machine::vector::{MaskedSwizzle, VectorComponent};

use super::{error::{NomGrammarResult, GrammarError, AMDILError}, Instruction, registers::InstructionInput};

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
    pub mods: SrcMods,
}
impl Src {
    pub fn apply_mask(&mut self, mask: MaskedSwizzle) {
        self.mods.swizzle = match self.mods.swizzle {
            Some(current_swizz) => {
                Some(current_swizz.masked_out(mask))
            },
            None => {
                Some(MaskedSwizzle::identity(4).masked_out(mask))
            }
        }
    }
}

pub struct DstMods {
    pub saturate: bool,
    pub shift_scale: Option<DstMul>,
}
impl Default for DstMods {
    fn default() -> Self {
        Self { saturate: false, shift_scale: None }
    }
}

pub enum DstMul {
    Mul(f32),
    Div(f32),
}

/// Section 3.4 "Destination Modifiers"
#[derive(Debug, Clone)]
enum DstMod {
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

/// Encodes all the modifiers for a source in a single small struct
#[derive(Debug, Clone, Default)]
pub struct SrcMods {
    pub swizzle: Option<MaskedSwizzle>,
    pub invert: bool,
    pub bias: bool,
    pub x2: bool,
    // no bx2 field - that's equivalent to setting bias and x2 both to true
    pub sign: bool,
    pub div_comp: Option<VectorComponent>,
    pub abs: bool,
    pub neg: [bool; 4],
    pub clamp: bool,
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
/// Also, return the leading whitespace!
pub fn parse_instruction_name(
    data: &str,
) -> NomGrammarResult<(usize, String, Vec<CtrlSpec>, DstMods)> {
    let (data, leading_whitespace) = take_while(|c: char| c.is_whitespace())(data)?;

    let (data, full_name) = take_while1(|c| c != ' ')(data)?;

    // The components of the instruction name are underscore-separated
    let (_, components) =
        complete(separated_list1(tag("_"), take_while1(|c| c != '_')))(full_name)?;
    let mut instr = components[0].to_owned();
    let mut ctrl_specifiers = vec![];
    let mut dst_mods = DstMods::default();
    for comp in components[1..].into_iter() {
        match *comp {
            "x2" => dst_mods.shift_scale = Some(DstMul::Mul(2.0f32)),
            "x4" => dst_mods.shift_scale = Some(DstMul::Mul(4.0f32)),
            "x8" => dst_mods.shift_scale = Some(DstMul::Mul(8.0f32)),
            "d2" => dst_mods.shift_scale = Some(DstMul::Div(2.0f32)),
            "d4" => dst_mods.shift_scale = Some(DstMul::Div(4.0f32)),
            "d8" => dst_mods.shift_scale = Some(DstMul::Div(8.0f32)),
            "sat" => dst_mods.saturate = true,
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

    Ok((data, (leading_whitespace.len(), instr, ctrl_specifiers, dst_mods)))
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
pub fn parse_src(data: &str) -> NomGrammarResult<InstructionInput> {
    // args are either a hex literal or a named arg
    let (data, regid) = parse_regid(data)?;
    let (data, mods) = many0(parse_named_arg_mod)(data)?;

    let mut combined_mods = SrcMods::default();
    for m in mods {
        match m {
            SrcMod::Swizzled(swizzle) => combined_mods.swizzle = Some(swizzle),
            SrcMod::Invert => combined_mods.invert = true,
            SrcMod::Bias => combined_mods.bias = true,
            SrcMod::X2 => combined_mods.x2 = true,
            SrcMod::Bx2 => {
                combined_mods.bias = true;
                combined_mods.x2 = true;
            },
            SrcMod::Sign => combined_mods.sign = true,
            SrcMod::DivComp(c) => combined_mods.div_comp = Some(c),
            SrcMod::Abs => combined_mods.abs = true,
            SrcMod::Neg(comps) => combined_mods.neg = [
                comps.contains(&VectorComponent::X),
                comps.contains(&VectorComponent::Y),
                comps.contains(&VectorComponent::Z),
                comps.contains(&VectorComponent::W),
            ],
            SrcMod::Clamp => combined_mods.clamp = true
        }
    }

    let (data, _) = opt(tag(", "))(data)?;
    Ok((data, InstructionInput::Src(Src{ regid, mods: combined_mods })))
}
pub fn parse_many1_src(data: &str) -> NomGrammarResult<Vec<InstructionInput>> {
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
pub fn parse_dst<'a>(data: &'a str) -> NomGrammarResult<Dst> {
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
    // Initially this seems simple. A source swizzle is `.[xyzw_01]{4}`, right?
    // Well, no. Some instructions choose to omit certain parts!
    // Examples from the AMDIL doc (Section 3.5) (note these are actually destinations but I have seen instances of these outside):
    // mov r0.x, r1
    // mov r0.y r1
    // mov r0.z r1
    // mov r0.x_zw, r1
    // mov r0.y_w, r1
    // mov r0.__w, r1
    // mov r0._1_w, r1
    // mov r0.01z, r1
    // mov r0.01z1, r1
    // mov r0.11, r1
    // mov r0._11, r1
    // mov r0.__11, r1
    // mov r0.xyz1, r1
    // mov r0_1_0, r1
    // mov r0.11_0, r1
    // mov r0.y00, r1
    // mov r0.yz1, r1
    // mov r0.y_1, r1
    //
    // Note that r0.x is possible - the AMDIL binary spec implies that that means the .x is equivalent to .xxxx
    // r0.11 implies that a two-spread is possible - I'm assuming that means the other components don't exist and must not be used.
    // r0.yz1 implies a three-spread is possible - again I don't see any clear reasoning for what the fourth component would be.
    // 
    // The other issue is underscore ambiguity.
    // cb2[4].y_neg(xyzw)
    // is the _ part of the swizzle or the neg modifier?
    // We assume when parsing modifiers that the _ is included in front of the `neg`.
    // So when we do the parsing here we have to ignore the final _... *sometimes*.
    // I make the assumption that any swizzle .xyz_ will always be represented with .xyz in AMDIL,
    // and indeed that any swizzle with a final _ will never use it.
    // r0.x___ -> r0.x (although this does allow .x to be used elsewhere...)
    // r0.xy__ -> r0.xy
    // r0.xyz_ -> r0.xyz
    //
    // If we make this assumption then we can special-case any time we see a swizzle with a final _.

    // `.[xyzw_]{1,4}`
    let (post_dot_data, _) = tag(".")(data)?;
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
    let (_, mut comps) = many_m_n(1, 4, swizzle_parser)(post_dot_data)?;
    // Pop off any final _s
    while comps.last().unwrap().is_none() {
        comps.pop().unwrap();
    }
    // Calculate the new position in the stream as (number of components actually taken) after the dot
    let (data, _) = take(comps.len())(post_dot_data)?;

    let comps = match comps.len() {
        // r0.x -> r0.xxxx
        1 => [comps[0], comps[0], comps[0], comps[0]],
        // r0.xy -> r0.xy__
        2 => [comps[0], comps[1], None, None],
        // r0.xyz -> r0.xyz_
        3 => [comps[0], comps[1], comps[2], None],
        // r0.xyzw = r0.xyzw
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