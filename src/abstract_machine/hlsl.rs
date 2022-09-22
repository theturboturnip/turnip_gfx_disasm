pub mod compat {
    use crate::abstract_machine::{
        vector::{MaskedSwizzle, VectorComponent, VECTOR_COMPONENTS},
        DataKind, DataRef, ScalarAbstractVM, TypedRef,
    };

    pub trait ExpandsIntoHLSLComponents {
        type TName;
        fn expand(&self) -> Vec<(Self::TName, VectorComponent)>;
    }

    #[derive(Debug, Clone)]
    pub struct HLSLDataRefSpec<TName: HLSLCompatibleNameRef> {
        pub base_name_ref: (TName, MaskedSwizzle),
        pub ref_type: HLSLDataRefType,
        pub kind: DataKind,
    }
    #[derive(Debug, Clone)]
    pub enum HLSLDataRefType {
        GenericRegister,
        ShaderInput,
        ShaderOutput,
        Literal([u64; 4]),
        ArrayElement { of: Box<Self>, idx: u64 },
    }
    impl<TName: HLSLCompatibleNameRef> ExpandsIntoHLSLComponents for HLSLDataRefSpec<TName> {
        type TName = TName;
        fn expand(&self) -> Vec<(TName, VectorComponent)> {
            self.base_name_ref
                .1
                 .0
                .iter()
                .filter_map(|comp| comp.map(|comp| (self.base_name_ref.0.clone(), comp)))
                .collect()
        }
    }

    #[derive(Debug, Clone)]
    pub struct HLSLDeclarationSpec<TName: HLSLCompatibleNameRef> {
        pub base_name_ref: TName,
        pub decl_type: HLSLDeclarationSpecType,
        pub n_components: u8,
        pub kind: DataKind,
        pub name: String,
    }
    #[derive(Debug, Clone)]
    pub enum HLSLDeclarationSpecType {
        GenericRegister,
        ShaderInput,
        ShaderOutput,
        Array { of: Box<Self>, len: u64 },
    }
    impl<TName: HLSLCompatibleNameRef> ExpandsIntoHLSLComponents for HLSLDeclarationSpec<TName> {
        type TName = TName;
        fn expand(&self) -> Vec<(TName, VectorComponent)> {
            (0..self.n_components)
                .into_iter()
                .map(|i| (self.base_name_ref.clone(), VECTOR_COMPONENTS[i as usize]))
                .collect()
        }
    }

    pub trait HLSLCompatibleNameRef: DataRef {}

    pub type HLSLCompatibleScalarRef<T> = (T, VectorComponent);
    impl<T: HLSLCompatibleNameRef> DataRef for HLSLCompatibleScalarRef<T> {
        fn is_pure_input(&self) -> bool {
            self.0.is_pure_input()
        }
    }

    /// Trait for abstract VMs that are capable of translation to HLSL.
    ///
    /// Allows VMs to define actions in terms of "elements" i.e. the basic unit that it specifically operates on.
    pub trait HLSLCompatibleAbstractVM: std::fmt::Debug {
        /// Element = the unit that abstract VM instructions operate on.
        ///
        /// e.g. for DXBC and AMDIL instructions operate on vectors => TElementDataRef = a VectorDataRef.
        /// Must be convertible to an HLSL-esque representation
        type TElementNameRef: HLSLCompatibleNameRef;

        // /// A type representing actual data that was used or mutated in an instruction
        // ///
        // /// Usually something like (TElementNameRef, MaskedSwizzle)
        // type TElementDataRef: HLSLCompatibleDataRef;

        // /// Given a [HLSLAbstractVM::TElementDataRef], expand it into constituent [AbstractVM::TScalarDataRef]
        // fn expand_element(elem: &Self::TElementDataRef) -> Vec<Self::TScalarDataRef>;
    }
    impl<T> ScalarAbstractVM for T
    where
        T: HLSLCompatibleAbstractVM,
    {
        type TScalarDataRef = HLSLCompatibleScalarRef<T::TElementNameRef>;
    }

    pub trait HLSLCompatibleAction<TVM: HLSLCompatibleAbstractVM> {
        fn hlsl_outcomes(&self) -> Vec<HLSLCompatibleOutcome<TVM>>;
    }

    #[derive(Debug, Clone)]
    pub enum HLSLCompatibleOutcome<TVM: HLSLCompatibleAbstractVM + ScalarAbstractVM> {
        // Declare that some named element exists, and optionally has a known literal value.
        Declaration {
            name: HLSLDeclarationSpec<TVM::TElementNameRef>,
            literal_value: Option<TypedRef<[u64; 4]>>,
        },

        // Declare that an output element has a new value, based on many input scalars.
        Operation {
            opname: String,
            output_elem: HLSLDataRefSpec<TVM::TElementNameRef>,
            input_elems: Vec<HLSLDataRefSpec<TVM::TElementNameRef>>,
            component_deps: Vec<(
                TypedRef<TVM::TScalarDataRef>,
                Vec<TypedRef<TVM::TScalarDataRef>>,
            )>,
        },
    }
}
