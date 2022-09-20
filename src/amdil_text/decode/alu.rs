use crate::{abstract_machine::vector::VectorDataRef, amdil_text::grammar};
use phf::phf_map;

use super::{decode_args, registers::arg_as_vector_data_ref, AMDILTextDecodeError};

struct ALUInstructionDef {
    n_out: usize,
    n_in: usize,
}

#[derive(Debug, Clone)]
pub struct ALUInstruction {
    name: &'static str,
    dsts: Vec<VectorDataRef>,
    srcs: Vec<VectorDataRef>,
}

const ALU_INSTR_DEFS: phf::Map<&'static str, ALUInstructionDef> = phf_map! {
    "mov" => ALUInstructionDef{ n_out: 1, n_in: 1 },
    "dp4_ieee" => ALUInstructionDef{ n_out: 1, n_in: 2 },
    "dp3_ieee" => ALUInstructionDef{ n_out: 1, n_in: 2 },
    "max_ieee" => ALUInstructionDef{ n_out: 1, n_in: 2 },
    "add" => ALUInstructionDef{ n_out: 1, n_in: 2 },
};

pub fn decode_alu(
    g_instr: &grammar::Instruction,
) -> Result<Option<ALUInstruction>, AMDILTextDecodeError> {
    match (
        ALU_INSTR_DEFS.get_entry(g_instr.instr.as_str()),
        decode_args(&g_instr.args).as_slice(),
    ) {
        (Some((static_name, instr_def)), matchable_args) => {
            // Map matchable_args into VectorDataRefs
            let args: Result<Vec<VectorDataRef>, AMDILTextDecodeError> =
                matchable_args.iter().map(arg_as_vector_data_ref).collect();

            // Take n_out destination items, n_in src items
            let mut args = args?.into_iter();
            let dsts: Vec<_> = args.by_ref().take(instr_def.n_out).collect();
            let srcs: Vec<_> = args.take(instr_def.n_in).collect();

            // if there aren't enough items, take() will just return how many there were - check we aren't missing any
            if dsts.len() < instr_def.n_out || srcs.len() < instr_def.n_in {
                return Err(AMDILTextDecodeError::Generic(format!(
                    "Not enough arguments for '{}' - expected {} dst and {} src, got {:?}",
                    static_name, instr_def.n_out, instr_def.n_in, matchable_args
                )));
            }

            // ok, produce the instruction
            Ok(Some(ALUInstruction {
                name: *static_name,
                dsts,
                srcs,
            }))
        }
        (None, _) => Ok(None),
    }
}
