use crate::{DataWidth, ScalarAbstractVM, ScalarAction, ScalarOutcome, TypedVMRef};

use super::{
    compat::HLSLCompatibleAbstractVM, syntax::HLSLOperator, HLSLScalarDataRef, HLSLVector,
    HLSLVectorDataRef, HLSLVectorName,
};

/// The type of Action held by Programs for the [HLSLAbstractVM]
pub enum HLSLAction {
    /// State that a new variable exists and has a given value taken directly from other variables
    Definition {
        new_var: HLSLVector,
        components: Vec<HLSLScalarDataRef>,
    },
    /// State that the output of an operation has been assigned to some components of a variable
    Operation {
        /// The operation being performed.
        op: HLSLOperator,
        /// List of input values, which each have a (potentially not-concrete) type.
        ///
        /// Checked by the [new] constructor to have the same number of elements as `op.n_inputs()`.
        inputs: Vec<HLSLVectorDataRef>,
        /// The name of the output value
        output: HLSLVectorDataRef,
        // Mapping of each individual scalar output to each individual scalar input
        scalar_deps: Vec<(HLSLScalarDataRef, Vec<HLSLScalarDataRef>)>,
    },
    /// State that the program flow may end early due to some vector components
    EarlyOut { inputs: Vec<HLSLScalarDataRef> },
}

/// Type for the HLSL abstract VM. Implements [ScalarAbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum HLSLAbstractVM {}
impl ScalarAbstractVM for HLSLAbstractVM {
    type Action = HLSLAction;
    type TScalarDataRef = HLSLScalarDataRef;
    type TElementDataRef = HLSLVectorDataRef;
}
impl HLSLCompatibleAbstractVM for HLSLAbstractVM {
    type TElementNameRef = HLSLVectorName;
}

impl ScalarAction<HLSLAbstractVM> for HLSLAction {
    fn outcomes(&self) -> Vec<ScalarOutcome<HLSLAbstractVM>> {
        match self {
            HLSLAction::Definition {
                new_var,
                components,
            } => new_var
                .identity_swizzle()
                .0
                .iter()
                .enumerate()
                .filter_map(|(i, comp)| match comp {
                    Some(comp) => Some(ScalarOutcome::Declaration {
                        name: (new_var.clone(), *comp),
                        value: Some(TypedVMRef {
                            data: components[i].clone(),
                            kind: new_var.kind,
                            width: DataWidth::E32,
                        }),
                    }),
                    None => None,
                })
                .collect(),
            HLSLAction::Operation { scalar_deps, .. } => scalar_deps
                .iter()
                .map(|(a, bs)| ScalarOutcome::Dependency {
                    output: TypedVMRef {
                        data: a.clone(),
                        kind: a.0.kind,
                        width: DataWidth::E32,
                    },
                    inputs: bs
                        .iter()
                        .map(|b| TypedVMRef {
                            data: b.clone(),
                            kind: b.0.kind,
                            width: DataWidth::E32,
                        })
                        .collect(),
                })
                .collect(),
            HLSLAction::EarlyOut { inputs } => vec![ScalarOutcome::EarlyOut {
                inputs: inputs
                    .iter()
                    .map(|(vec, comp)| TypedVMRef {
                        data: (vec.clone(), *comp),
                        kind: vec.kind,
                        width: DataWidth::E32,
                    })
                    .collect(),
            }],
        }
    }
}
