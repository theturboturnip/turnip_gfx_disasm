use crate::{
    abstract_machine::vector::{MaskedSwizzle, Vector2ScalarAbstractVM, VectorDataRef},
    amdil_text::grammar,
    Action,
};
use phf::phf_map;

use super::{decode_args, registers::arg_as_vector_data_ref, AMDILTextDecodeError};

#[derive(Debug, Clone, Copy)]
enum InputMask {
    /// Inherit the mask from the output
    InheritFromOutput,
    /// Truncate all vector masks to N components
    TruncateTo(u8),
}
impl InputMask {
    fn apply(&self, output: &VectorDataRef, input: VectorDataRef) -> VectorDataRef {
        let swizzle = match self {
            Self::InheritFromOutput => input.swizzle.masked_out(output.swizzle),
            Self::TruncateTo(x) => input.swizzle.truncated(*x),
        };
        VectorDataRef {
            name: input.name,
            swizzle,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OutputDep {
    /// Each output's component is only affected by the corresponding input components
    ///
    /// e.g. `add r0.xyz r1.xyz r2.xyz` has dependencies `[r1.x, r2.x] -> r0.x`, `[r1.y, r2.y] -> r0.y`, `[r1.z, r2.z] -> r0.z`
    PerComponent,
    /// Each output's component is affected by all input components
    ///
    /// e.g. `dp3_ieee r0.x, r1.xyz, r2.xyz` has dependency `[r1.xyz, r2.xyz] -> r0.x`
    All,
}

struct ALUInstructionDef {
    n_in: usize,
    input_mask: InputMask,
    output_dep: OutputDep,
}

#[derive(Debug, Clone)]
pub struct ALUInstruction {
    name: &'static str,
    dst: VectorDataRef,
    srcs: Vec<VectorDataRef>,
    output_dep: OutputDep,
}

const ALU_INSTR_DEFS: phf::Map<&'static str, ALUInstructionDef> = phf_map! {
    "mov" => ALUInstructionDef{ n_in: 1, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent },
    "dp4_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::TruncateTo(4), output_dep: OutputDep::All },
    "dp3_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::TruncateTo(3), output_dep: OutputDep::All },
    "max_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent },
    "add" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent },
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
            // TODO error handling
            let dst = args.by_ref().take(1).next().unwrap();

            // Apply input mask preference
            let input_mask = instr_def.input_mask;
            let srcs: Vec<_> = args
                .take(instr_def.n_in)
                .map(|src| input_mask.apply(&dst, src))
                .collect();

            // if there aren't enough items, take() will just return how many there were - check we aren't missing any
            if srcs.len() < instr_def.n_in {
                return Err(AMDILTextDecodeError::Generic(format!(
                    "Not enough arguments for '{}' - expected 1 dst and {} src, got {:?}",
                    static_name, instr_def.n_in, matchable_args
                )));
            }

            // ok, produce the instruction
            Ok(Some(ALUInstruction {
                name: *static_name,
                dst,
                srcs,
                output_dep: instr_def.output_dep,
            }))
        }
        (None, _) => Ok(None),
    }
}

impl Action<Vector2ScalarAbstractVM> for ALUInstruction {
    fn outcomes(&self) -> Vec<crate::Outcome<Vector2ScalarAbstractVM>> {
        todo!()
    }
}
