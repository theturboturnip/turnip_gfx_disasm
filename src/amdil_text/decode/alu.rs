use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::InstrArgs, vector::MaskedSwizzle, VMName,
    },
    amdil_text::vm::{AMDILMaskSwizVector, AMDILRegister, AMDILAction},
    hlsl::{
        syntax::{ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic},
        kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind},
    },
    Action,
};
use lazy_static::lazy_static;

use super::{
    registers::AMDILContext, grammar::{parse_many1_src, parse_dst, DstMod, CtrlSpec}, matchable_ctrl_specs, error::AMDILError,
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
    output_kind: HLSLKind,
}
impl ALUArgsSpec {
    fn sanitize_arguments(&self, output_elem: AMDILMaskSwizVector, input_elems: Vec<AMDILMaskSwizVector>) -> InstrArgs<AMDILMaskSwizVector> {
        let mask_to_apply_to_input = match self.input_mask {
            InputMask::InheritFromFirstOutput => output_elem.swizzle().copy_mask(),
            InputMask::TruncateTo(n) => MaskedSwizzle::identity(n.into()),
        };

        let outputs = vec![(output_elem, self.output_kind)];

        let inputs: Vec<_> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, kind)| {
                let swizzle = data.swizzle().masked_out(mask_to_apply_to_input);
                let kind = kind.intersection(data.toplevel_kind()).unwrap();
                let data = AMDILMaskSwizVector::new(data.register().clone(), swizzle);
                (data, kind)
            })
            .collect();

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
            output_kind: HLSLNumericKind::Float.into(),
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
                output_kind: HLSLKindBitmask::all().into()
            },
            HLSLOperator::Assign,
        )),
        ("dp4_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::TruncateTo(4),
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp3_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::TruncateTo(3),
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("min_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Min),
        )),
        ("max_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLNumericKind::Float.into(),
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
                output_kind: HLSLKindBitmask::INTEGER.into(),
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Lt),
        )),
        ("le", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKindBitmask::INTEGER.into(),
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Le),
        )),
        ("gt", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKindBitmask::INTEGER.into(),
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Gt),
        )),
        ("ge", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKindBitmask::INTEGER.into(),
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
                output_kind: HLSLKindBitmask::all().into(),
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ternary)
        )),
    ]);
}

pub fn parse_alu<'a>(
    data: &'a str, instr: String, ctrl_specifiers: Vec<CtrlSpec>, dst_mods: Vec<DstMod>,
    ctx: &AMDILContext,
) -> Result<(&'a str, ALUInstruction), AMDILError> {
    match (
        instr.as_str(),
        &matchable_ctrl_specs(&ctrl_specifiers)[..],
    ) {
        ("sample", [("resource", tex_id), ("sampler", _sampler_id)]) => {
            let (data, dst) = parse_dst(data, dst_mods)?;
            let dst = ctx.dst_to_maskswizvector(&dst)?;

            let (data, srcs) = parse_many1_src(data)?;
            let srcs: Result<Vec<AMDILMaskSwizVector>, AMDILError> =
                srcs.iter().map(|a| ctx.src_to_maskswizvector(a)).collect();
            let mut srcs = srcs?;
            // Insert the texture argument as the first input
            srcs.insert(
                0,
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
                output_kind: HLSLNumericKind::Float.into(),
            };

            return Ok((data, ALUInstruction {
                args: arg_spec.sanitize_arguments(dst, srcs),
                op: HLSLOperator::SampleI(SampleIntrinsic::Tex2D),
            }));
        }
        _ => {}
    };
    match ALU_INSTR_DEFS.get_key_value(instr.as_str()) {
        Some((_static_name, instr_spec)) => {
            // Map matchable_args into VectorDataRefs
            let (data, dst) = parse_dst(data, dst_mods)?;
            let dst = ctx.dst_to_maskswizvector(&dst)?;

            let (data, srcs) = parse_many1_src(data)?;
            let srcs: Result<Vec<AMDILMaskSwizVector>, AMDILError> =
                srcs.iter().map(|a| ctx.src_to_maskswizvector(a)).collect();

            let args = instr_spec.0.sanitize_arguments(dst, srcs?);

            // ok, produce the instruction
            Ok((data, ALUInstruction {
                args,
                op: instr_spec.1,
            }))
        }
        None => return Err(AMDILError::UnkInstruction(instr, ctrl_specifiers)),
    }
}

impl ALUInstruction {
    pub fn push_actions(&self, v: &mut Vec<AMDILAction>) {
        v.extend(self.args
            .outputs
            .iter()
            .map(|output| Action::Assign {
                op: self.op,
                inputs: self.args.inputs.clone(),
                output: output.clone(),
            }))
    }
}
