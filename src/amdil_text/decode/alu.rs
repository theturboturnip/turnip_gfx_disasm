use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::InstrArgs, vector::MaskedSwizzle, VMName, expr::{Vector, ContigSwizzle},
    },
    amdil_text::vm::{AMDILRegister, AMDILAction, AMDILVector},
    hlsl::{
        syntax::{ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic, BinaryArithmeticOp, UnaryOp},
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
    args: InstrArgs<AMDILRegister>,
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
    fn sanitize_arguments(&self, output_elem: (AMDILRegister, MaskedSwizzle), input_elems: Vec<(AMDILRegister, MaskedSwizzle)>) -> InstrArgs<AMDILRegister> {
        let mask_to_apply_to_input = match self.input_mask {
            InputMask::InheritFromFirstOutput => output_elem.1.copy_mask(),
            InputMask::TruncateTo(n) => MaskedSwizzle::identity(n.into()),
        };

        let dst = (output_elem.0, swizzle_to_contig(output_elem.1), self.output_kind);

        let srcs: Vec<_> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, usage_kind)| {
                let swizzle = data.1.masked_out(mask_to_apply_to_input);
                let usage_kind = usage_kind.intersection(data.0.toplevel_kind()).unwrap();
                (amdil_vec_of(data.0, swizzle, usage_kind), usage_kind)
            })
            .collect();

        assert_eq!(srcs.len(), self.input_kinds.len());

        InstrArgs { dst, srcs }
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

fn logical_cmp(cmp: FauxBooleanOp, arg_kind: HLSLKind) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![arg_kind, arg_kind],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kind: HLSLKindBitmask::INTEGER.into(),
        },
        HLSLOperator::FauxBoolean(cmp)
    )
}

fn logical_op(op: BinaryArithmeticOp) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![HLSLKindBitmask::INTEGER.into(), HLSLKindBitmask::INTEGER.into()],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kind: HLSLKindBitmask::INTEGER.into(),
        },
        HLSLOperator::BinaryArithmetic(op)
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
        // TODO difference between mul and mul_ieee
        // MUL: 0 DX9 DP2-style multiple.
        //      1 IEEE-style multiply
        ("mul", float_arith(ArithmeticOp::Times)),
        ("mul_ieee", float_arith(ArithmeticOp::Times)),
        ("div", float_arith(ArithmeticOp::Div)),

        // TODO mad_ieee has different NaN handling
        ("mad", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Mad)
        )),
        ("mad_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into(), HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Mad)
        )),

        // Computes the reciprocal of the square root of each component of src0.
        // TODO plain "rsq" has weird behaviour - it only computes rsq of the w component.
        ("rsq_vec", (
            ALUArgsSpec {
                input_kinds: vec![HLSLNumericKind::Float.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLNumericKind::Float.into(),
            },
            HLSLOperator::NumericI(NumericIntrinsic::Rsqrt)
        )),

        ("lt", logical_cmp(FauxBooleanOp::Lt, HLSLNumericKind::Float.into())),
        ("le", logical_cmp(FauxBooleanOp::Le, HLSLNumericKind::Float.into())),
        ("gt", logical_cmp(FauxBooleanOp::Gt, HLSLNumericKind::Float.into())),
        ("ge", logical_cmp(FauxBooleanOp::Ge, HLSLNumericKind::Float.into())),

        ("ieq", logical_cmp(FauxBooleanOp::Eq, HLSLKindBitmask::INTEGER.into())),
        ("ine", logical_cmp(FauxBooleanOp::Ne, HLSLKindBitmask::INTEGER.into())),
        ("ilt", logical_cmp(FauxBooleanOp::Lt, HLSLKindBitmask::INTEGER.into())),
        ("ige", logical_cmp(FauxBooleanOp::Ge, HLSLKindBitmask::INTEGER.into())),

        ("iand", logical_op(BinaryArithmeticOp::BitwiseAnd)),
        ("ior", logical_op(BinaryArithmeticOp::BitwiseOr)),
        ("ixor", logical_op(BinaryArithmeticOp::BitwiseXor)),
        ("inot", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKindBitmask::INTEGER.into()],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKindBitmask::INTEGER.into(),
            },
            HLSLOperator::Unary(UnaryOp::BinaryNot)
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
            let dst = ctx.dst_to_vector(&dst)?;

            let (data, srcs) = parse_many1_src(data)?;
            let srcs: Result<Vec<_>, AMDILError> =
                srcs.iter().map(|a| ctx.src_to_vector(a)).collect();
            let mut srcs = srcs?;
            // Insert the texture argument as the first input
            srcs.insert(
                0,
                (
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
            let dst = ctx.dst_to_vector(&dst)?;

            let (data, srcs) = parse_many1_src(data)?;
            let srcs: Result<Vec<_>, AMDILError> =
                srcs.iter().map(|a| ctx.src_to_vector(a)).collect();

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
    pub fn push_actions(self, v: &mut Vec<AMDILAction>) {
        v.push(Action::Assign {
            expr: Vector::of_expr(self.op, self.args.srcs, self.args.dst.2),
            output: (self.args.dst.0, self.args.dst.1),
        })
    }
}

fn swizzle_to_contig(swizzle: MaskedSwizzle) -> ContigSwizzle {
    swizzle.0.iter().filter_map(|comp_opt| *comp_opt).collect()
}

fn amdil_vec_of(reg: AMDILRegister, swizzle: MaskedSwizzle, kind: HLSLKind) -> AMDILVector {
    AMDILVector::PureSwizzle(reg, swizzle_to_contig(swizzle), kind)
}

// AMDILVector::Construction(vec![
//     UntypedScalar::Literal(literal[0]),
//     UntypedScalar::Literal(literal[1]),
//     UntypedScalar::Literal(literal[2]),
//     UntypedScalar::Literal(literal[3]),
// ])