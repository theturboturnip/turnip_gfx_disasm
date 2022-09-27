use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::{ArgsSpec, DependencyRelation, InstrArgs, SimpleDependencyRelation},
        vector::MaskedSwizzle,
        DataKind, DataWidth, TypedVMRef,
    },
    amdil_text::{
        grammar,
        vm::{AMDILAbstractVM, AMDILDataRef},
    },
    hlsl::{
        compat::{HLSLCompatibleAction, HLSLCompatibleOutcome},
        syntax::{
            ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic,
            UnconcreteOpResult,
        },
    },
    ScalarAction, ScalarOutcome,
};
use lazy_static::lazy_static;

use super::{decode_args, registers::arg_as_vector_data_ref, AMDILTextDecodeError};

#[derive(Debug, Clone, Copy)]
enum InputMask {
    /// Inherit the mask from the output
    InheritFromFirstOutput,
    /// Truncate all vector masks to N components
    TruncateTo(u8),
}

#[derive(Debug, Clone)]
pub struct ALUInstruction {
    args: InstrArgs<AMDILAbstractVM>,
    dep_relation: SimpleDependencyRelation,
    op: HLSLOperator,
}

#[derive(Debug, Clone)]
struct ALUArgsSpec {
    input_kinds: Vec<DataKind>,
    input_mask: InputMask,
    output_kinds: Vec<DataKind>,
}
impl ArgsSpec<AMDILAbstractVM> for ALUArgsSpec {
    fn sanitize_arguments(&self, args: Vec<AMDILDataRef>) -> InstrArgs<AMDILAbstractVM> {
        let (output_elems, input_elems) = args.split_at(self.output_kinds.len());
        assert_eq!(output_elems.len(), self.output_kinds.len());
        assert_eq!(input_elems.len(), self.input_kinds.len());

        let mask_to_apply_to_input = match self.input_mask {
            InputMask::InheritFromFirstOutput => output_elems[0].swizzle.copy_mask(),
            InputMask::TruncateTo(n) => MaskedSwizzle::identity(n.into()),
        };
        const WIDTH: DataWidth = DataWidth::E32;

        let outputs: Vec<_> = output_elems
            .into_iter()
            .zip(self.output_kinds.iter())
            .map(|(data, kind)| TypedVMRef {
                // TODO wish we didn't need to clone here :(
                data: data.clone(),
                kind: *kind,
                width: WIDTH,
            })
            .collect();

        let inputs: Vec<_> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, kind)| {
                let swizzle = data.swizzle.masked_out(mask_to_apply_to_input);
                TypedVMRef {
                    // TODO wish we didn't need to clone here :(
                    data: AMDILDataRef {
                        name: data.name.clone(),
                        swizzle,
                    },
                    kind: *kind,
                    width: WIDTH,
                }
            })
            .collect();

        assert_eq!(outputs.len(), self.output_kinds.len());
        assert_eq!(inputs.len(), self.input_kinds.len());

        InstrArgs { outputs, inputs }
    }
}
type ALUInstructionSet =
    HashMap<&'static str, (ALUArgsSpec, SimpleDependencyRelation, HLSLOperator)>;

fn float_arith(op: ArithmeticOp) -> (ALUArgsSpec, SimpleDependencyRelation, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![DataKind::Float, DataKind::Float],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kinds: vec![DataKind::Float],
        },
        SimpleDependencyRelation::PerComponent,
        HLSLOperator::Arithmetic(op),
    )
}

lazy_static! {
    static ref ALU_INSTR_DEFS: ALUInstructionSet = HashMap::from([
        ("mov", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Hole],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Hole]
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::Assign,
        )),
        ("dp4_ieee", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::TruncateTo(4),
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp3_ieee", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::TruncateTo(3),
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("min_ieee", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::NumericI(NumericIntrinsic::Min),
        )),
        ("max_ieee", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::NumericI(NumericIntrinsic::Max),
        )),

        ("add", float_arith(ArithmeticOp::Plus)),
        ("sub", float_arith(ArithmeticOp::Minus)),
        ("mul", float_arith(ArithmeticOp::Times)),
        ("div", float_arith(ArithmeticOp::Div)),

        ("lt", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Lt),
        )),
        ("le", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Le),
        )),
        ("gt", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Gt),
        )),
        ("ge", (
            ALUArgsSpec {
                input_kinds: vec![DataKind::Float, DataKind::Float],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ge),
        )),

        ("cmov_logical", (
            ALUArgsSpec {
                // first input = integer "is zero"?
                // second and third input could be anything, output kind could be anything
                input_kinds: vec![DataKind::UnsignedInt, DataKind::Hole, DataKind::Hole],
                input_mask: InputMask::TruncateTo(3),
                output_kinds: vec![DataKind::Hole],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ternary)
        )),

        ("sample", (
            ALUArgsSpec {
                // Coords (TODO texture is specified in a modifier - prob need to move sample out of this)
                input_kinds: vec![DataKind::Float],
                input_mask: InputMask::TruncateTo(2),
                output_kinds: vec![DataKind::Float],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::SampleI(SampleIntrinsic::Tex2DFakeDoesntTakeTex)
        )),
    ]);
}

pub fn decode_alu(
    g_instr: &grammar::Instruction,
) -> Result<Option<ALUInstruction>, AMDILTextDecodeError> {
    match (
        ALU_INSTR_DEFS.get_key_value(g_instr.instr.as_str()),
        decode_args(&g_instr.args).as_slice(),
    ) {
        (Some((_static_name, instr_spec)), matchable_args) => {
            // Map matchable_args into VectorDataRefs
            let args: Result<Vec<AMDILDataRef>, AMDILTextDecodeError> =
                matchable_args.iter().map(arg_as_vector_data_ref).collect();

            let args = instr_spec.0.sanitize_arguments(args?);

            // ok, produce the instruction
            Ok(Some(ALUInstruction {
                args,
                dep_relation: instr_spec.1,
                op: instr_spec.2,
            }))
        }
        (None, _) => Ok(None),
    }
}

impl ScalarAction<AMDILAbstractVM> for ALUInstruction {
    fn outcomes(&self) -> Vec<ScalarOutcome<AMDILAbstractVM>> {
        self.dep_relation
            .determine_dependencies(&self.args)
            .into_iter()
            .map(|(output, inputs)| ScalarOutcome::Dependency { output, inputs })
            .collect()
    }
}
impl HLSLCompatibleAction<AMDILAbstractVM> for ALUInstruction {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<AMDILAbstractVM>> {
        let comp_outcomes = Self::outcomes(&self);
        self.args
            .outputs
            .iter()
            .map(|output| HLSLCompatibleOutcome::Operation {
                op: UnconcreteOpResult::new(
                    self.op,
                    self.args
                        .inputs
                        .iter()
                        .map(|src| src.data.clone().into_hlsl(src.kind))
                        .collect(),
                ),
                output_dataspec: output.data.clone().into_hlsl(output.kind),
                component_deps: comp_outcomes
                    .iter()
                    .filter_map(|out| match out {
                        ScalarOutcome::Dependency {
                            output: output_comp,
                            inputs: inputs_comps,
                        } => {
                            if output_comp.data.vm_name_ref == output.data.name {
                                Some((output_comp.clone(), inputs_comps.clone()))
                            } else {
                                None
                            }
                        }
                        _ => panic!("ALUInstruction::outcomes returned a not-dependency"),
                    })
                    .collect(),
            })
            .collect()
        // vec![HLSLCompatibleOutcome::Operation {
        //     opname: self.name.to_owned(),
        //     output_dataspec: self.dst.clone().into_hlsl(self.data_kind),
        //     input_dataspecs: self
        //         .srcs
        //         .iter()
        //         .map(|src| src.clone().into_hlsl(self.data_kind))
        //         .collect(),
        //     component_deps: comp_outcomes
        //         .into_iter()
        //         .map(|out| match out {
        //             ScalarOutcome::Dependency {
        //                 output: output_comps,
        //                 inputs: inputs_comps,
        //             }, (output_comps, inputs_comps),
        //             _, panic!("ALUInstruction::outcomes returned a not-dependency"),
        //         })
        //         .collect(),
        // }]
    }
}
