use arrayvec::ArrayVec;
use turnip_gfx_disasm::{Action, hlsl::{HLSLRegister, syntax::{HLSLOperator, NumericIntrinsic}, kinds::HLSLKind, compat::program_to_hlsl}, abstract_machine::{vector::VectorComponent, expr::{Vector, ContigSwizzle, Scalar}}, amdil_text::{vm::AMDILAbstractVM, AMDILProgram, AMDILDecoder}, Decoder};

fn parse_amdil(data: &'static str) -> Vec<Action<HLSLRegister>> {
    let decoder = AMDILDecoder::new();
    let program = decoder.decode(data.trim()).expect("Decode failure");
    program_to_hlsl(&program).actions
}
fn compare_actions(actual: Vec<Action<HLSLRegister>>, expected: Vec<Action<HLSLRegister>>) {
    assert_eq!(expected.len(), actual.len());
    for (e, a) in expected.into_iter().zip(actual) {
        assert_eq!(e, a);
    }
}

#[test]
fn test_amdil_complex_dot() {
    let X: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::X,
    ]);
    let Y: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::Y,
    ]);
    let Z: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::Z,
    ]);
    let W: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::W,
    ]);
    let XYZ: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::X,
        VectorComponent::Y,
        VectorComponent::Z,
    ]);
    let XYZW: ContigSwizzle = ArrayVec::from_iter([
        VectorComponent::X,
        VectorComponent::Y,
        VectorComponent::Z,
        VectorComponent::W,
    ]);

    compare_actions(
        parse_amdil("
dp3_ieee r0.x___, r2.xyz, r3.xyz
dp3_ieee r0._y__, r2.xyz, r3.xyz
dp3_ieee r0.__z_, r2.xyz, r3.xyz
dcl_literal l2, 0x3F800000, 0x3F800000, 0x3F800000, 0x3F800000
mov r0.___w, l2
dp4_ieee r1.x___, r2, r0
        "),
        vec![
            Action::Assign {
                output: (HLSLRegister::GenericRegister("r0".into(), 4), X.clone()),
                expr: Vector::Expr {
                    op: HLSLOperator::NumericI(NumericIntrinsic::Dot),
                    inputs: vec![
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r2".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r3".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                    ],
                    output_kind: HLSLKind::NUMERIC_FLOAT,
                    n_comps: 1
                }
            },
            Action::Assign {
                output: (HLSLRegister::GenericRegister("r0".into(), 4), Y.clone()),
                expr: Vector::Expr {
                    op: HLSLOperator::NumericI(NumericIntrinsic::Dot),
                    inputs: vec![
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r2".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r3".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                    ],
                    output_kind: HLSLKind::NUMERIC_FLOAT,
                    n_comps: 1
                }
            },
            Action::Assign {
                output: (HLSLRegister::GenericRegister("r0".into(), 4), Z.clone()),
                expr: Vector::Expr {
                    op: HLSLOperator::NumericI(NumericIntrinsic::Dot),
                    inputs: vec![
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r2".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r3".into(), 4), XYZ.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                    ],
                    output_kind: HLSLKind::NUMERIC_FLOAT,
                    n_comps: 1
                }
            },
            Action::Assign {
                output: (HLSLRegister::GenericRegister("r0".into(), 4), W.clone()),
                expr: Vector::Expr {
                    op: HLSLOperator::Assign,
                    inputs: vec![
                        (
                            Vector::Construction(vec![Scalar::Literal(0x3f80_0000)], HLSLKind::NUMERIC),
                            HLSLKind::NUMERIC
                        ),
                    ],
                    output_kind: HLSLKind::NUMERIC,
                    n_comps: 1
                }
            },
            Action::Assign {
                output: (HLSLRegister::GenericRegister("r1".into(), 4), X.clone()),
                expr: Vector::Expr {
                    op: HLSLOperator::NumericI(NumericIntrinsic::Dot),
                    inputs: vec![
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r2".into(), 4), XYZW.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                        (
                            Vector::PureSwizzle(HLSLRegister::GenericRegister("r0".into(), 4), XYZW.clone(), HLSLKind::NUMERIC_FLOAT),
                            HLSLKind::NUMERIC_FLOAT
                        ),
                    ],
                    output_kind: HLSLKind::NUMERIC_FLOAT,
                    n_comps: 1
                }
            },
        ]
    )
}