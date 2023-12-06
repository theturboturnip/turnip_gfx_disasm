use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::InstrArgs, vector::MaskedSwizzle,
    },
    amdil_text::{
        grammar,
        vm::{AMDILAbstractVM, AMDILMaskSwizVector, AMDILRegister},
    },
    hlsl::{
        syntax::{ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic},
        kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind},
    },
    Action, Outcome,
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
    args: InstrArgs<AMDILMaskSwizVector>,
    op: HLSLOperator,
}

// TODO Should this use HLSLOperandKind and have a separate hole vector? probably not - we don't need to encode the type dependency here
#[derive(Debug, Clone)]
struct ALUArgsSpec {
    input_kinds: Vec<HLSLKind>,
    input_mask: InputMask,
    output_kinds: Vec<HLSLKind>,
}
impl ALUArgsSpec {
    fn sanitize_arguments(&self, args: Vec<AMDILMaskSwizVector>) -> InstrArgs<AMDILMaskSwizVector> {
        let (output_elems, input_elems) = args.split_at(self.output_kinds.len());
        assert_eq!(output_elems.len(), self.output_kinds.len());
        assert_eq!(input_elems.len(), self.input_kinds.len());

        let mask_to_apply_to_input = match self.input_mask {
            InputMask::InheritFromFirstOutput => output_elems[0].swizzle().copy_mask(),
            InputMask::TruncateTo(n) => MaskedSwizzle::identity(n.into()),
        };

        let outputs: Vec<_> = output_elems
            .into_iter()
            .zip(self.output_kinds.iter())
            .map(|(data, kind)| {
                (data.clone(), *kind)
            })
            .collect();

        let inputs: Vec<_> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, kind)| {
                let swizzle = data.swizzle().masked_out(mask_to_apply_to_input);
                let data = AMDILMaskSwizVector::new(data.register().clone(), swizzle);
                (data, *kind)
            })
            .collect();

        assert_eq!(outputs.len(), self.output_kinds.len());
        assert_eq!(inputs.len(), self.input_kinds.len());

        InstrArgs { outputs, inputs }
    }
}
type ALUInstructionSet = HashMap<&'static str, (ALUArgsSpec, HLSLOperator)>;

fn float_arith(op: ArithmeticOp) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kinds: vec![HLSLNumericKind::Float.into()],
        },
        HLSLOperator::Arithmetic(op),
    )
}

lazy_static! {
    static ref ALU_INSTR_DEFS: ALUInstructionSet = HashMap::from([
        ("mov", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKindBitmask::all().into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::all().into()]
            },
            HLSLOperator::Assign,
        )),
        ("dp4_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::TruncateTo(4),
                output_kinds: vec![HLSLNumericKind::Float.into()],
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp3_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::TruncateTo(3),
                output_kinds: vec![HLSLNumericKind::Float.into()],
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("min_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLNumericKind::Float.into()],
            },
            HLSLOperator::NumericI(NumericIntrinsic::Min),
        )),
        ("max_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLNumericKind::Float.into()],
            },
            HLSLOperator::NumericI(NumericIntrinsic::Max),
        )),

        ("add", float_arith(ArithmeticOp::Plus)),
        ("sub", float_arith(ArithmeticOp::Minus)),
        ("mul", float_arith(ArithmeticOp::Times)),
        ("div", float_arith(ArithmeticOp::Div)),

        ("lt", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::INTEGER.into()],
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Lt),
        )),
        ("le", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::INTEGER.into()],
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Le),
        )),
        ("gt", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::INTEGER.into()],
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Gt),
        )),
        ("ge", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::INTEGER.into()],
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ge),
        )),

        ("cmov_logical", (
            ALUArgsSpec {
                // first input = integer "is zero"?
                // second and third input could be anything, output kind could be anything
                // they should all be the same length! (page 7-154)
                // can do cmov_logical r0.xyz r1.xyz r2.xyz r3.xyz: r3.x = r2.x if r0.x else r1.x and so forth for x,y,z
                input_kinds: vec![HLSLNumericKind::UnsignedInt.into(), HLSLKindBitmask::all().into(), HLSLKindBitmask::all().into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kinds: vec![HLSLKindBitmask::all().into()],
            },
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
            let args: Result<Vec<AMDILMaskSwizVector>, AMDILTextDecodeError> =
                matchable_args.iter().map(arg_as_vector_data_ref).collect();
            let mut args = args?;
            // Insert the argument at index 1 (index 0 = the output)
            args.insert(
                1,
                AMDILMaskSwizVector::new(
                    AMDILRegister::Texture(tex_id.parse().unwrap()),
                    MaskedSwizzle::identity(1),
                ),
            );

            let arg_spec = ALUArgsSpec {
                input_kinds: vec![
                    HLSLConcreteKind::Texture2D.into(),
                    HLSLNumericKind::Float.into(),
                ],
                input_mask: InputMask::TruncateTo(2),
                output_kinds: vec![HLSLNumericKind::Float.into()],
            };

            return Ok(Some(ALUInstruction {
                args: arg_spec.sanitize_arguments(args),
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
            let args: Result<Vec<AMDILMaskSwizVector>, AMDILTextDecodeError> =
                matchable_args.iter().map(arg_as_vector_data_ref).collect();

            let args = instr_spec.0.sanitize_arguments(args?);

            // ok, produce the instruction
            Ok(Some(ALUInstruction {
                args,
                op: instr_spec.1,
            }))
        }
        (None, _) => Ok(None),
    }
}

impl Action<AMDILAbstractVM> for ALUInstruction {
    fn outcomes(&self) -> Vec<Outcome<AMDILAbstractVM>> {
        self.args
            .outputs
            .iter()
            .map(|output| Outcome::Assign {
                op: self.op,
                inputs: self.args.inputs.clone(),
                output: output.clone(),
            })
            .collect()
    }
}
