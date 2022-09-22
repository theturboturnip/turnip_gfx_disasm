use super::{AbstractVM, DataRef, TypedRef};

pub trait HLSLCompatibleDataRef: DataRef {
    // TODO function for converting to HLSL-ready output
}

pub trait HLSLAbstractVM: AbstractVM {
    /// Element = the unit that abstract VM instructions operate on.
    ///
    /// e.g. for DXBC and AMDIL instructions operate on vectors => TElementDataRef = a VectorDataRef.
    /// Must be convertible to an HLSL-esque representation
    type TElementDataRef: HLSLCompatibleDataRef;

    /// Given a [HLSLAbstractVM::TElementDataRef], expand it into constituent [AbstractVM::TScalarDataRef]
    fn expand_element(elem: &Self::TElementDataRef) -> Vec<Self::TScalarDataRef>;
}

pub trait HLSLAction<TVM: HLSLAbstractVM> {
    fn per_element_outcomes(&self) -> Vec<HLSLOutcome<TVM>>;
}

#[derive(Debug, Clone)]
pub enum HLSLOutcome<TVM: HLSLAbstractVM> {
    // Declare that some named element exists, and optionally has a known value.
    Declaration {
        name: TVM::TElementDataRef,
        value: Option<TypedRef<TVM::TElementDataRef>>,
    },
    // Declare that an output element has a new value, based on many input scalars.
    Dependency {
        opname: String,
        output_elem: TypedRef<TVM::TElementDataRef>,
        input_elems: Vec<TypedRef<TVM::TElementDataRef>>,
        component_deps: Vec<(
            TypedRef<TVM::TScalarDataRef>,
            Vec<TypedRef<TVM::TScalarDataRef>>,
        )>,
    },
}
