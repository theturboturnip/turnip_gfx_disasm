use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::Deref,
};

pub mod scalar;
pub mod vector;

pub trait DataRef: Clone + PartialEq + Eq + Hash + std::fmt::Debug {
    /// Returns true if the data is a pure input and should not be expanded into dependencies when passed as a parent.
    fn is_pure_input(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataKind {
    Float,
    SignedInt,
    UnsignedInt,
    Untyped,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DataWidth {
    E8,
    E16,
    E32,
    E64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypedRef<TData: DataRef> {
    pub data: TData,
    pub kind: DataKind,
    pub width: DataWidth,
}

pub trait AbstractVM: std::fmt::Debug {
    type TDataRef: DataRef;
}
// Marker trait for VMs that use a TDataRef that refers to a single Scalar.
pub trait ScalarBasedAbstractVM: AbstractVM {}

pub trait Action<TVM: AbstractVM> {
    fn outcomes(&self) -> Vec<Outcome<TVM>>;
}

#[derive(Debug, Clone)]
pub enum Outcome<TVM: AbstractVM> {
    // Declare that some named scalar exists, and optionally has a known value.
    Declaration {
        name: TVM::TDataRef,
        value: Option<TypedRef<TVM::TDataRef>>,
    },
    // Declare that an output scalar has a new value, based on many input scalars.
    Dependency {
        output: TypedRef<TVM::TDataRef>,
        inputs: Vec<TypedRef<TVM::TDataRef>>,
    },
}

pub trait Decoder<TVM: AbstractVM> {
    type Input;
    type BaseAction: Deref<Target = dyn Action<TVM>>;
    type Err;

    fn decode(&self, data: Self::Input) -> Result<Vec<Self::BaseAction>, Self::Err>;
}

/// Dependency solver for scalar-based abstract VMs
pub struct ScalarDependencies<TVM: ScalarBasedAbstractVM> {
    dependents: HashMap<TVM::TDataRef, HashSet<TypedRef<TVM::TDataRef>>>,
}
impl<TVM: ScalarBasedAbstractVM> ScalarDependencies<TVM> {
    pub fn new() -> Self {
        Self {
            dependents: HashMap::new(),
        }
    }

    /// Accumulate the dependencies from the given Action onto the existing world state.
    ///
    /// e.g. if the action introduces a dependency of GeneralPurposeRegister(1) onto Output(o),
    /// set `self.dependents[Output(o)]` to the contents of `self.dependents[GeneralPurposeRegister(1)]`
    pub fn accum_action(&mut self, action: &dyn Action<TVM>) {
        for dep in action.outcomes() {
            match dep {
                Outcome::Declaration { name, value } => todo!(),
                Outcome::Dependency { output, inputs } => {
                    if output.data.is_pure_input() {
                        println!(
                            "Weird! Someone is writing to a pure input. Ignoring dependency {:?} -> {:?}",
                            inputs, output
                        );
                        continue;
                    }

                    // TODO introduce logic for kind/width coercion, store keys as ValueRef

                    let mut resolved_inputs = HashSet::new();
                    for input in inputs {
                        if input.data.is_pure_input() {
                            // Pure inputs are not resolved into their dependencies
                            resolved_inputs.insert(input.clone());
                        } else {
                            if let Some(input_dependents) = self.dependents.get(&input.data) {
                                resolved_inputs.extend(input_dependents.iter().cloned());
                            } else {
                                println!("Weird! Someone is using a non-initialized non-pure input {:?}. Treating as pure", input);
                                resolved_inputs.insert(input.clone());
                            }
                        }
                    }
                    self.dependents.insert(output.data, resolved_inputs);
                }
            }
        }
    }

    pub fn dependents(&self) -> &HashMap<TVM::TDataRef, HashSet<TypedRef<TVM::TDataRef>>> {
        &self.dependents
    }
}
