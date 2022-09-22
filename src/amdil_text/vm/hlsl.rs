use crate::abstract_machine::{
    hlsl::compat::{
        HLSLCompatibleAction, HLSLCompatibleNameRef, HLSLCompatibleOutcome, HLSLDataRefSpec,
        HLSLDataRefType, HLSLDeclarationSpec, HLSLDeclarationSpecType,
    },
    DataKind, DataWidth, TypedRef,
};

use super::{AMDILAbstractVM, AMDILDataRef, AMDILDeclaration, AMDILNameRef};

impl HLSLCompatibleNameRef for AMDILNameRef {}

impl HLSLCompatibleAction<AMDILAbstractVM> for AMDILDeclaration {
    fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<AMDILAbstractVM>> {
        match self {
            AMDILDeclaration::NamedLiteral(name, value) => {
                vec![HLSLCompatibleOutcome::Declaration {
                    name: HLSLDeclarationSpec {
                        base_name_ref: AMDILNameRef::NamedLiteral(name.clone()),
                        kind: DataKind::Hole,
                        n_components: 4,
                        decl_type: HLSLDeclarationSpecType::GenericRegister,
                        name: name.clone(),
                    },
                    literal_value: Some(TypedRef {
                        data: *value,
                        kind: DataKind::Hole,
                        width: DataWidth::E32,
                    }),
                }]
            }
            AMDILDeclaration::NamedInputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declaration {
                name: HLSLDeclarationSpec {
                    base_name_ref: AMDILNameRef::NamedInputRegister(name.clone()),
                    kind: DataKind::Hole,
                    n_components: *len,
                    decl_type: HLSLDeclarationSpecType::ShaderInput,
                    name: name.clone(),
                },
                literal_value: None,
            }],
            AMDILDeclaration::NamedOutputRegister {
                name,
                len,
                reg_type: _,
            } => vec![HLSLCompatibleOutcome::Declaration {
                name: HLSLDeclarationSpec {
                    base_name_ref: AMDILNameRef::NamedOutputRegister(name.clone()),
                    kind: DataKind::Hole,
                    n_components: *len,
                    decl_type: HLSLDeclarationSpecType::ShaderInput,
                    name: name.clone(),
                },
                literal_value: None,
            }],
            AMDILDeclaration::NamedBuffer { name, len } => {
                vec![HLSLCompatibleOutcome::Declaration {
                    name: HLSLDeclarationSpec {
                        base_name_ref: AMDILNameRef::NamedOutputRegister(name.clone()),
                        kind: DataKind::Hole,
                        n_components: 4,
                        decl_type: HLSLDeclarationSpecType::Array {
                            of: Box::new(HLSLDeclarationSpecType::ShaderInput),
                            len: *len,
                        },
                        name: name.clone(),
                    },
                    literal_value: None,
                }]
            }
        }
    }
}

impl AMDILDataRef {
    pub fn into_hlsl(self, kind: DataKind) -> HLSLDataRefSpec<AMDILNameRef> {
        let ref_type = match self.name {
            AMDILNameRef::Literal(data) => HLSLDataRefType::Literal(data),
            AMDILNameRef::NamedBuffer { idx, .. } => HLSLDataRefType::ArrayElement {
                of: Box::new(HLSLDataRefType::ShaderInput),
                idx,
            },
            AMDILNameRef::NamedLiteral(_) | AMDILNameRef::NamedRegister(_) => {
                HLSLDataRefType::GenericRegister
            }
            AMDILNameRef::NamedInputRegister(_) => HLSLDataRefType::ShaderInput,
            AMDILNameRef::NamedOutputRegister(_) => HLSLDataRefType::ShaderOutput,
        };
        HLSLDataRefSpec {
            base_name_ref: (self.name, self.swizzle),
            ref_type,
            kind,
        }
    }
}
