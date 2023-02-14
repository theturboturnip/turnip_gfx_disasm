use crate::{
    abstract_machine::{DataWidth, TypedVMRef},
    hlsl::{
        compat::{
            HLSLCompatibleAction, HLSLCompatibleOutcome, HLSLDataRefSpec, HLSLDeclarationSpec,
            HLSLDeclarationSpecType, HLSLNameRefType,
        },
        types::{HLSLConcreteType, HLSLHoleTypeMask, HLSLType},
    },
};

use super::{AMDILAbstractVM, AMDILDataRef, AMDILDeclaration, AMDILNameRef};

impl HLSLCompatibleAction<AMDILAbstractVM> for AMDILDeclaration {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::TextureResource(id) => vec![HLSLCompatibleOutcome::Declaration {
                declspec: HLSLDeclarationSpec {
                    vm_name_ref: AMDILNameRef::Texture(*id),
                    kind: HLSLConcreteType::Texture2D.into(),
                    n_components: 1,
                    decl_type: HLSLDeclarationSpecType::Texture(*id),
                    name: format!("tex{:>03}", id),
                },
                literal_value: None,
            }],
            AMDILDeclaration::NamedLiteral(name, value) => {
                vec![HLSLCompatibleOutcome::Declaration {
                    declspec: HLSLDeclarationSpec {
                        vm_name_ref: AMDILNameRef::NamedLiteral(name.clone()),
                        kind: HLSLHoleTypeMask::NUMERIC.into(),
                        n_components: 4,
                        decl_type: HLSLDeclarationSpecType::GenericRegister,
                        name: name.clone(),
                    },
                    literal_value: Some(TypedVMRef {
                        data: *value,
                        kind: HLSLHoleTypeMask::NUMERIC.into(),
                        width: DataWidth::E32,
                    }),
                }]
            }
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declaration {
                declspec: HLSLDeclarationSpec {
                    vm_name_ref: AMDILNameRef::NamedInputRegister(name.clone()),
                    kind: HLSLHoleTypeMask::NUMERIC_FLOAT.into(),
                    n_components: *len,
                    decl_type: HLSLDeclarationSpecType::ShaderInput(name.clone()),
                    name: name.clone(),
                },
                literal_value: None,
            }],
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declaration {
                declspec: HLSLDeclarationSpec {
                    vm_name_ref: AMDILNameRef::NamedOutputRegister(name.clone()),
                    kind: HLSLHoleTypeMask::NUMERIC_FLOAT.into(),
                    n_components: *len,
                    decl_type: HLSLDeclarationSpecType::ShaderOutput(name.clone()),
                    name: name.clone(),
                },
                literal_value: None,
            }],
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

impl AMDILDataRef {
    pub fn into_hlsl(self, kind: HLSLType) -> HLSLDataRefSpec<AMDILNameRef> {
        let ref_type = match &self.name {
            AMDILNameRef::Texture(id) => HLSLNameRefType::Texture(*id),
            AMDILNameRef::Literal(data) => HLSLNameRefType::Literal(*data),
            AMDILNameRef::NamedBuffer { name, idx } => HLSLNameRefType::ArrayElement {
                of: Box::new(HLSLNameRefType::ShaderInput(name.clone())),
                idx: *idx,
            },
            AMDILNameRef::NamedLiteral(_) | AMDILNameRef::NamedRegister(_) => {
                HLSLNameRefType::GenericRegister
            }
            AMDILNameRef::NamedInputRegister(name) => HLSLNameRefType::ShaderInput(name.clone()),
            AMDILNameRef::NamedOutputRegister(name) => HLSLNameRefType::ShaderOutput(name.clone()),
        };
        HLSLDataRefSpec {
            vm_name_ref: self.name,
            swizzle: self.swizzle,
            name_ref_type: ref_type,
            kind,
        }
    }
}
