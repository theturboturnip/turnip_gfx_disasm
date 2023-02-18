use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::{ArgsSpec, DependencyRelation, InstrArgs, SimpleDependencyRelation},
        vector::MaskedSwizzle,
        DataWidth, TypedVMRef,
    },
    amdil_text::{
        grammar,
        vm::{AMDILAbstractVM, AMDILDataRef, AMDILNameRef},
    },
    hlsl::{
        compat::{HLSLCompatibleAction, HLSLCompatibleOutcome},
        syntax::{
            ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic,
            UnconcreteOpResult,
        },
        types::{HLSLConcreteType, HLSLHoleTypeMask, HLSLNumericType, HLSLType},
    },
    Action, LegacyOutcome,
};
use lazy_static::lazy_static;

use super::{
    decode_args, decode_instr_mods, registers::arg_as_vector_data_ref, AMDILTextDecodeError,
};

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

// TODO Should this use HLSLOperandType and have a separate hole vector? probably not - we don't need to encode the type dependency here
#[derive(Debug, Clone)]
struct ALUArgsSpec {
    input_kinds: Vec<HLSLType>,
    input_mask: InputMask,
    output_kinds: Vec<HLSLType>,
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
            input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kinds: vec![HLSLNumericType::Float.into()],
        },
        SimpleDependencyRelation::PerComponent,
        HLSLOperator::Arithmetic(op),
    )
}

lazy_static! {
    static ref ALU_INSTR_DEFS: ALUInstructionSet = HashMap::from([
        ("mov", (
            ALUArgsSpec {
                input_kinds: vec![HLSLHoleTypeMask::all().into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLHoleTypeMask::all().into()]
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::Assign,
        )),
        ("dp4_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::TruncateTo(4),
                output_kinds: vec![HLSLNumericType::Float.into()],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp3_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::TruncateTo(3),
                output_kinds: vec![HLSLNumericType::Float.into()],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("min_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLNumericType::Float.into()],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::NumericI(NumericIntrinsic::Min),
        )),
        ("max_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLNumericType::Float.into()],
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
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLHoleTypeMask::INTEGER.into()],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Lt),
        )),
        ("le", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLHoleTypeMask::INTEGER.into()],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Le),
        )),
        ("gt", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLHoleTypeMask::INTEGER.into()],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Gt),
        )),
        ("ge", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericType::Float.into(), HLSLNumericType::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLHoleTypeMask::INTEGER.into()],
            },
            SimpleDependencyRelation::PerComponent,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ge),
        )),

        ("cmov_logical", (
            ALUArgsSpec {
                // first input = integer "is zero"?
                // second and third input could be anything, output kind could be anything
                input_kinds: vec![HLSLNumericType::UnsignedInt.into(), HLSLHoleTypeMask::all().into(), HLSLHoleTypeMask::all().into()],
                input_mask: InputMask::TruncateTo(3),
                output_kinds: vec![HLSLHoleTypeMask::all().into()],
            },
            SimpleDependencyRelation::AllToAll,
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ternary)
        )),
    ]);
}

pub fn decode_alu(
    g_instr: &grammar::Instruction,
) -> Result<Option<ALUInstruction>, AMDILTextDecodeError> {
    let matchable_args = decode_args(&g_instr.args);
    match (
        g_instr.instr.as_str(),
        &decode_instr_mods(&g_instr.instr_mods)[..],
        &matchable_args,
    ) {
        ("sample", [("resource", tex_id), ("sampler", _sampler_id)], matchable_args) => {
            let args: Result<Vec<AMDILDataRef>, AMDILTextDecodeError> =
                matchable_args.iter().map(arg_as_vector_data_ref).collect();
            let mut args = args?;
            // Insert the argument at index 1 (index 0 = the output)
            args.insert(
                1,
                AMDILDataRef {
                    name: AMDILNameRef::Texture(tex_id.parse().unwrap()),
                    swizzle: MaskedSwizzle::identity(1),
                },
            );

            let arg_spec = ALUArgsSpec {
                input_kinds: vec![
                    HLSLConcreteType::Texture2D.into(),
                    HLSLNumericType::Float.into(),
                ],
                input_mask: InputMask::TruncateTo(2),
                output_kinds: vec![HLSLNumericType::Float.into()],
            };

            return Ok(Some(ALUInstruction {
                args: arg_spec.sanitize_arguments(args),
                dep_relation: SimpleDependencyRelation::AllToAll,
                op: HLSLOperator::SampleI(SampleIntrinsic::Tex2D),
            }));
        }
        _ => {}
    };
    match (
        ALU_INSTR_DEFS.get_key_value(g_instr.instr.as_str()),
        matchable_args,
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

impl Action<AMDILAbstractVM> for ALUInstruction {
    // This implementation is really bad but it will go away once we change how Outcome works
    fn outcomes(&self) -> Vec<LegacyOutcome<AMDILAbstractVM>> {
        self.dep_relation
            .determine_dependencies(&self.args)
            .into_iter()
            .map(|(output, inputs)| {
                let (output, comp) = (&self.args.outputs[output.0], output.1);
                let output_arg = TypedVMRef {
                    data: (output.data.name.clone(), comp),
                    kind: output.kind,
                    width: output.width,
                };
                LegacyOutcome::Dependency {
                    output: output_arg,
                    inputs: inputs
                        .into_iter()
                        .map(|(in_idx, comp)| {
                            let input = &self.args.inputs[in_idx];
                            TypedVMRef {
                                data: (input.data.name.clone(), comp),
                                kind: input.kind,
                                width: input.width,
                            }
                        })
                        .collect(),
                }
            })
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
                    output.data.clone().into_hlsl(output.kind),
                ),
                component_deps: comp_outcomes
                    .iter()
                    .filter_map(|out| match out {
                        LegacyOutcome::Dependency {
                            output: output_comp,
                            inputs: inputs_comps,
                        } => {
                            if output_comp.data.0 == output.data.name {
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
        //             Outcome::Dependency {
        //                 output: output_comps,
        //                 inputs: inputs_comps,
        //             }, (output_comps, inputs_comps),
        //             _, panic!("ALUInstruction::outcomes returned a not-dependency"),
        //         })
        //         .collect(),
        // }]
    }
}
