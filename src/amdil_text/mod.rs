pub mod decode;
pub use decode::Instruction;
mod grammar;

// TODO error handling here
pub fn decode_amdil_text_program(data: &str) -> Vec<Instruction> {
    let (_, instrs) = grammar::parse_lines(data).expect("couldn't parse");

    instrs
        .into_iter()
        .map(decode::decode_instruction)
        .collect::<Result<Vec<Instruction>, _>>()
        .expect("couldn't decode parsed")
}
