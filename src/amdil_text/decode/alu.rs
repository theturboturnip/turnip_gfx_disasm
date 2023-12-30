use std::collections::HashMap;

use crate::{
    abstract_machine::{
        instructions::InstrArgs, vector::MaskedSwizzle, VMName, expr::{Vector, ContigSwizzle},
    },
    amdil_text::vm::{AMDILRegister, AMDILAction, AMDILVector},
    hlsl::{
        syntax::{ArithmeticOp, FauxBooleanOp, HLSLOperator, NumericIntrinsic, SampleIntrinsic, BinaryArithmeticOp, UnaryOp, NumericCastTo},
        kinds::{HLSLConcreteKind, HLSLKind, HLSLKindBitmask, HLSLNumericKind},
    },
    Action,
};
use lazy_static::lazy_static;

use super::{
    registers::{AMDILContext, InstructionInput}, grammar::{parse_many1_src, parse_dst, DstMod, CtrlSpec, Src, RegId, SrcMod, Dst}, matchable_ctrl_specs, error::AMDILError,
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
    fn sanitize_arguments(&self, ctx: &AMDILContext, output_elem: Dst, input_elems: Vec<InstructionInput>) -> Result<InstrArgs<AMDILRegister>, AMDILError> {
        let mask_to_apply_to_input = match self.input_mask {
            InputMask::InheritFromFirstOutput => output_elem.write_mask.into(),
            InputMask::TruncateTo(n) => MaskedSwizzle::identity(n.into()),
        };

        let (dst_reg, dst_comps) = ctx.dst_to_vector(output_elem)?;
        let dst = (dst_reg, dst_comps, self.output_kind);

        let srcs: Result<Vec<(AMDILVector, HLSLKind)>, AMDILError> = input_elems
            .into_iter()
            .zip(self.input_kinds.iter())
            .map(|(data, usage_kind)| {
                Ok((ctx.input_to_vector(data, mask_to_apply_to_input)?, *usage_kind))
            })
            .collect();

        let srcs = srcs?;

        assert_eq!(srcs.len(), self.input_kinds.len());

        Ok(InstrArgs { dst, srcs })
    }
}
type ALUInstructionSet = HashMap<&'static str, (ALUArgsSpec, HLSLOperator)>;

fn float_arith(op: ArithmeticOp) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kind: HLSLKind::NUMERIC_FLOAT,
        },
        HLSLOperator::Arithmetic(op),
    )
}

fn logical_cmp(cmp: FauxBooleanOp, arg_kind: HLSLKind) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![arg_kind, arg_kind],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kind: HLSLKind::INTEGER,
        },
        HLSLOperator::FauxBoolean(cmp)
    )
}

fn logical_op(op: BinaryArithmeticOp) -> (ALUArgsSpec, HLSLOperator) {
    (
        ALUArgsSpec {
            input_kinds: vec![HLSLKind::INTEGER, HLSLKind::INTEGER],
            input_mask: InputMask::InheritFromFirstOutput,
            output_kind: HLSLKind::INTEGER,
        },
        HLSLOperator::BinaryArithmetic(op)
    )
}

lazy_static! {
    static ref ALU_INSTR_DEFS: ALUInstructionSet = HashMap::from([
        ("mov", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::ALL],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::ALL
            },
            HLSLOperator::Assign,
        )),
        ("dp4_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::TruncateTo(4),
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp3_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::TruncateTo(3),
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("dp2_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::TruncateTo(2),
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Dot),
        )),
        ("min_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Min),
        )),
        ("max_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
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
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Mad)
        )),
        ("mad_ieee", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT, HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Mad)
        )),

        // Computes the reciprocal of the square root of each component of src0.
        // TODO plain "rsq" has weird behaviour - it only computes rsq of the w component.
        ("rsq_vec", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Rsqrt)
        )),
        // TODO as for rsq, exp only uses w component but exp_vec does the whole thing
        // exn raises e to the power of x (w component only), exp and exp_vec raise 2 to the power of x
        ("exp_vec", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Exp2)
        )),
        // TODO as for rsq, sqrt operates on component w but sqrt_vec does the whole thing
        ("sqrt_vec", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Sqrt)
        )),
        // TODO as for rsq, log operates on component w but log_vec does the whole thing
        ("log_vec", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_FLOAT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericI(NumericIntrinsic::Log2)
        )),

        ("lt", logical_cmp(FauxBooleanOp::Lt, HLSLKind::NUMERIC_FLOAT)),
        ("le", logical_cmp(FauxBooleanOp::Le, HLSLKind::NUMERIC_FLOAT)),
        ("gt", logical_cmp(FauxBooleanOp::Gt, HLSLKind::NUMERIC_FLOAT)),
        ("ge", logical_cmp(FauxBooleanOp::Ge, HLSLKind::NUMERIC_FLOAT)),

        ("ieq", logical_cmp(FauxBooleanOp::Eq, HLSLKind::INTEGER)),
        ("ine", logical_cmp(FauxBooleanOp::Ne, HLSLKind::INTEGER)),
        ("ilt", logical_cmp(FauxBooleanOp::Lt, HLSLKind::NUMERIC_SINT)),
        ("ige", logical_cmp(FauxBooleanOp::Ge, HLSLKind::NUMERIC_SINT)),
        ("ult", logical_cmp(FauxBooleanOp::Lt, HLSLKind::NUMERIC_UINT)),
        ("uge", logical_cmp(FauxBooleanOp::Ge, HLSLKind::NUMERIC_UINT)),

        ("iand", logical_op(BinaryArithmeticOp::BitwiseAnd)),
        ("ior", logical_op(BinaryArithmeticOp::BitwiseOr)),
        ("ixor", logical_op(BinaryArithmeticOp::BitwiseXor)),
        ("inot", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::INTEGER],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::INTEGER,
            },
            HLSLOperator::Unary(UnaryOp::BinaryNot)
        )),

        ("cmov_logical", (
            ALUArgsSpec {
                // first input = integer "is zero"?
                // second and third input could be anything, output kind could be anything
                // they should all be the same length! (page 7-154)
                // can do cmov_logical r0.xyz r1.xyz r2.xyz r3.xyz: r3.x = r2.x if r0.x else r1.x and so forth for x,y,z
                input_kinds: vec![HLSLKind::NUMERIC_UINT, HLSLKind::ALL, HLSLKind::ALL],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::ALL,
            },
            HLSLOperator::FauxBoolean(FauxBooleanOp::Ternary)
        )),

        ("utof", (
            ALUArgsSpec {
                input_kinds: vec![HLSLKind::NUMERIC_UINT],
                input_mask: InputMask::InheritFromFirstOutput,
                output_kind: HLSLKind::NUMERIC_FLOAT,
            },
            HLSLOperator::NumericCast(NumericCastTo(HLSLNumericKind::Float))
        ))
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
            let (data, mut srcs) = parse_many1_src(data)?;
            // Insert the texture argument as the first input
            srcs.insert(
                0,
                InstructionInput::Texture(tex_id.parse().unwrap())
            );

            let arg_spec = ALUArgsSpec {
                input_kinds: vec![
                    HLSLConcreteKind::Texture2D.into(),
                    HLSLKind::NUMERIC_FLOAT,
                ],
                input_mask: InputMask::TruncateTo(2),
                output_kind: HLSLKind::NUMERIC_FLOAT,
            };

            return Ok((data, ALUInstruction {
                args: arg_spec.sanitize_arguments(ctx, dst, srcs)?,
                op: HLSLOperator::SampleI(SampleIntrinsic::Tex2D),
            }));
        }
        _ => {}
    };
    match ALU_INSTR_DEFS.get_key_value(instr.as_str()) {
        Some((_static_name, instr_spec)) => {
            // Map matchable_args into VectorDataRefs
            let (data, dst) = parse_dst(data, dst_mods)?;
            let (data, srcs) = parse_many1_src(data)?;
            let args = instr_spec.0.sanitize_arguments(ctx, dst, srcs)?;

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
