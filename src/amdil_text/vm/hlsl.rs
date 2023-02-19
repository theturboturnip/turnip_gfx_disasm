use crate::{
    abstract_machine::{instructions::SimpleDependencyRelation, vector::MaskedSwizzle},
    hlsl::{
        compat::{HLSLCompatibleAction, HLSLCompatibleOutcome},
        syntax::HLSLOperator,
    },
};

use super::{AMDILAbstractVM, AMDILDataRef, AMDILDeclaration, AMDILNameRef};

impl HLSLCompatibleAction<AMDILAbstractVM> for AMDILDeclaration {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::TextureResource(id) => {
                vec![HLSLCompatibleOutcome::Declare(AMDILNameRef::Texture(*id))]
            }
            AMDILDeclaration::NamedLiteral(name, value) => {
                let name = AMDILNameRef::NamedLiteral(name.clone());
                let input_name = AMDILNameRef::Literal(*value);
                vec![
                    HLSLCompatibleOutcome::Declare(name.clone()),
                    HLSLCompatibleOutcome::Assign {
                        output: AMDILDataRef {
                            name,
                            swizzle: MaskedSwizzle::identity(4),
                        }
                        .into(),
                        op: HLSLOperator::Assign,
                        inputs: vec![
                            AMDILDataRef::literal(*value, MaskedSwizzle::identity(4)).into()
                        ],
                        dep_rel: SimpleDependencyRelation::PerComponent,
                    },
                ]
            }
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declare(
                AMDILNameRef::NamedInputRegister(name.clone()),
            )],
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declare(
                AMDILNameRef::NamedOutputRegister(name.clone()),
            )],
            _ => vec![],
            // TODO re-enable this
            // AMDILDeclaration::NamedBuffer { name, len } => {
            //     vec![HLSLCompatibleOutcome::Declaration {
            //         name: HLSLDeclarationSpec {
            //             base_name_ref: AMDILNameRef::NamedOutputRegister(name.clone()),
            //             kind: DataKind::Hole,
            //             n_components: 4,
            //             decl_type: HLSLDeclarationSpecType::Array {
            //                 of: Box::new(HLSLDeclarationSpecType::ShaderInput(name.clone())),
            //                 len: *len,
            //             },
            //             name: name.clone(),
            //         },
            //         literal_value: None,
            //     }]
            // }
        }
    }
}
