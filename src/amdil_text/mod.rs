use self::grammar::AMDILTextError;

pub mod grammar;
pub use grammar::Instruction;

pub fn decode_amdil_text_program(data: &str) -> Result<Vec<Instruction>, AMDILTextError<&str>> {
    let (_, instrs) = grammar::parse_lines(data).expect("couldn't decode");
    Ok(instrs)
}
