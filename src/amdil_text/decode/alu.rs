use crate::{
    abstract_machine::{
        vector::{Vector2ScalarAbstractVM, VectorDataRef},
        DataKind, DataWidth, ElementAction, ElementOutcome, TypedRef,
    },
    amdil_text::grammar,
    Action, Outcome,
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
    data_kind: DataKind,
}

#[derive(Debug, Clone)]
pub struct ALUInstruction {
    dst: VectorDataRef,
    srcs: Vec<VectorDataRef>,
    output_dep: OutputDep,
    data_kind: DataKind,
}

const ALU_INSTR_DEFS: phf::Map<&'static str, ALUInstructionDef> = phf_map! {
    "mov" => ALUInstructionDef{ n_in: 1, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent, data_kind: DataKind::Hole },
    "dp4_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::TruncateTo(4), output_dep: OutputDep::All, data_kind: DataKind::Float },
    "dp3_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::TruncateTo(3), output_dep: OutputDep::All, data_kind: DataKind::Float },
    "max_ieee" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent, data_kind: DataKind::Float },
    "add" => ALUInstructionDef{ n_in: 2, input_mask: InputMask::InheritFromOutput, output_dep: OutputDep::PerComponent, data_kind: DataKind::Float },
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
                dst,
                srcs,
                output_dep: instr_def.output_dep,
                data_kind: instr_def.data_kind,
            }))
        }
        (None, _) => Ok(None),
    }
}

impl Action<Vector2ScalarAbstractVM> for ALUInstruction {
    fn outcomes(&self) -> Vec<Outcome<Vector2ScalarAbstractVM>> {
        let mut outcomes = vec![];
        for i in 0..4 {
            match (self.output_dep, self.dst.swizzle[i]) {
                (OutputDep::PerComponent, Some(dst_comp)) => outcomes.push(Outcome::Dependency {
                    output: TypedRef {
                        data: (self.dst.name.clone(), dst_comp),
                        kind: self.data_kind,
                        width: DataWidth::E32,
                    },
                    inputs: self
                        .srcs
                        .iter()
                        .map(|src| {
                            let src_comp = src.swizzle[i].expect(
                                "src component which mapped to dst component wasn't available",
                            );
                            TypedRef {
                                data: (src.name.clone(), src_comp),
                                kind: self.data_kind,
                                width: DataWidth::E32,
                            }
                        })
                        .collect(),
                }),
                (OutputDep::All, Some(dst_comp)) => outcomes.push(Outcome::Dependency {
                    output: TypedRef {
                        data: (self.dst.name.clone(), dst_comp),
                        kind: self.data_kind,
                        width: DataWidth::E32,
                    },
                    inputs: self
                        .srcs
                        .iter()
                        .map(|src| {
                            src.swizzle
                                .0
                                .iter()
                                // Get rid of None values from the source component
                                .filter_map(|comp| *comp)
                                // Map non-None source components -> TypedRef
                                .map(|src_comp| TypedRef {
                                    data: (src.name.clone(), src_comp),
                                    kind: self.data_kind,
                                    width: DataWidth::E32,
                                })
                            // Collect a set of TypedRefs for each input
                        })
                        // Flatten Iterator<Iterator<TypedRef>> into Iterator<TypedRef>
                        .flatten()
                        .collect(),
                }),
                (_, None) => {}
            }
        }
        outcomes
    }
}
impl ElementAction<Vector2ScalarAbstractVM> for ALUInstruction {
    fn per_element_outcomes(&self) -> Vec<ElementOutcome<Vector2ScalarAbstractVM>> {
        let comp_outcomes = Self::outcomes(&self);
        vec![ElementOutcome::Dependency {
            output_elem: TypedRef {
                data: self.dst.clone(),
                kind: self.data_kind,
                width: DataWidth::E32,
            },
            input_elems: self
                .srcs
                .iter()
                .map(|src| TypedRef {
                    data: src.clone(),
                    kind: self.data_kind,
                    width: DataWidth::E32,
                })
                .collect(),
            component_deps: comp_outcomes
                .into_iter()
                .map(|out| match out {
                    Outcome::Dependency {
                        output: output_comps,
                        inputs: inputs_comps,
                    } => (output_comps, inputs_comps),
                    _ => panic!("ALUInstruction::outcomes returned a not-dependency"),
                })
                .collect(),
        }]
    }
}
