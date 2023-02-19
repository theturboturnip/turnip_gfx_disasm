use crate::{
    abstract_machine::{vector::VectorComponent, VMDataRef, VMScalarDataRef, VMScalarNameRef},
    AbstractVM, Action, LegacyOutcome,
};

use super::{syntax::HLSLOperator, HLSLScalarDataRef, HLSLVector, HLSLVectorDataRef};

/// The type of Action held by Programs for the [HLSLAbstractVM]
#[derive(Debug)]
pub enum HLSLAction {
    /// State that a new variable exists but has no value yet
    Declaration { new_var: HLSLVector },
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
impl AbstractVM for HLSLAbstractVM {
    type Action = HLSLAction;
    type TVectorNameRef = HLSLVector;
    type TVectorDataRef = HLSLVectorDataRef;
    type TScalarDataRef = HLSLScalarDataRef;
}

impl VMDataRef<HLSLVector> for HLSLScalarDataRef {
    fn name(&self) -> &HLSLVector {
        &self.0
    }

    fn type_mask(&self) -> super::types::HLSLType {
        self.0.kind
    }
}
impl VMScalarDataRef<HLSLVector> for HLSLScalarDataRef {
    fn comp(&self) -> VectorComponent {
        self.1
    }

    fn scalar_name(&self) -> VMScalarNameRef<HLSLVector> {
        self.clone()
    }
}

impl Action<HLSLAbstractVM> for HLSLAction {
    fn outcomes(&self) -> Vec<LegacyOutcome<HLSLAbstractVM>> {
        match self {
            HLSLAction::Declaration { new_var } => new_var
                .identity_swizzle()
                .0
                .iter()
                .filter_map(|comp| match comp {
                    Some(comp) => Some(LegacyOutcome::Declaration {
                        name: (new_var.clone(), *comp),
                        value: None,
                    }),
                    None => None,
                })
                .collect(),
            HLSLAction::Definition {
                new_var,
                components,
            } => new_var
                .identity_swizzle()
                .0
                .iter()
                .enumerate()
                .filter_map(|(i, comp)| match comp {
                    Some(comp) => Some(LegacyOutcome::Declaration {
                        name: (new_var.clone(), *comp),
                        value: Some(components[i].clone()),
                    }),
                    None => None,
                })
                .collect(),
            HLSLAction::Operation { scalar_deps, .. } => scalar_deps
                .iter()
                .map(|(a, bs)| LegacyOutcome::Dependency {
                    output: a.clone(),
                    inputs: bs.clone(),
                })
                .collect(),
            HLSLAction::EarlyOut { inputs } => vec![LegacyOutcome::EarlyOut {
                inputs: inputs
                    .iter()
                    .map(|(vec, comp)| (vec.clone(), *comp))
                    .collect(),
            }],
        }
    }
}
