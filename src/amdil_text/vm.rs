//! This abstract machine definition aims to mimic the behaviour of AMDIL and support DXBC.
//!
//! A 2011 AMDIL spec is available [here](http://developer.amd.com/wordpress/media/2012/10/AMD_Intermediate_Language_(IL)_Specification_v2.pdf)
//!
//! All data is represented as 4-component vectors.
//! Inputs to instructions can be swizzled i.e. can have their components reordered or reused (v0.xyxx, v3.wzwx etc. are valid)

use crate::Action;
use crate::abstract_machine::expr::{Vector, Reg};
use crate::abstract_machine::{
    AbstractVM, VMName, VMVector
};
use crate::hlsl::{HLSLRegister, HLSLAction};
use crate::hlsl::compat::HLSLCompatibleAbstractVM;
use crate::hlsl::kinds::{HLSLKind, HLSLKindBitmask};

/// Type for the AMDIL abstract VM. Implements [AbstractVM] and [HLSLCompatibleAbstractVM]
#[derive(Debug, Clone)]
pub enum AMDILAbstractVM {}
impl AbstractVM for AMDILAbstractVM {
    type Register = AMDILRegister;
    // type Scalar = ComponentOf<AMDILRegister>;
    // type Vector = AMDILMaskSwizVector;

    // fn decompose(v: &Self::Vector) -> Vec<Self::Scalar> {
    //     let reg = v.register();
    //     v.swizzle().0.iter().filter_map(|c| match c {
    //         None => None,
    //         Some(comp) => Some(ComponentOf::new(reg.clone(), *comp))
    //     }).collect()
    // }
}
pub type AMDILAction = Action<AMDILRegister>;
impl HLSLCompatibleAbstractVM for AMDILAbstractVM {
    fn convert_action(a: &AMDILAction) -> HLSLAction {
        match a {
            Action::Assign { output, expr } => Action::Assign {
                output: (Self::convert_register(&output.0), output.1.clone()),
                expr: expr.map_reg(&mut |r, _| Self::convert_register(r)),
            },
            Action::EarlyOut => Action::EarlyOut,
            Action::If { expr, if_true, if_fals } => Action::If {
                expr: expr.map_reg(&mut |r, _| Self::convert_register(r), HLSLKind::INTEGER),
                if_true: if_true.into_iter().map(Self::convert_action).collect(),
                if_fals: if_fals.into_iter().map(Self::convert_action).collect()
            },
        }
    }

    fn convert_register(r: &Self::Register) -> HLSLRegister {
        match r {
            AMDILRegister::Literal(_) => panic!("This doesn't have a concept in HLSLRegister :P should delete"),
            AMDILRegister::NamedBuffer { name, idx } => HLSLRegister::ArrayElement { of: Box::new(HLSLRegister::ShaderInput(name.clone(), 4)), idx: *idx },
            AMDILRegister::NamedInputRegister(name) => HLSLRegister::ShaderInput(name.clone(), 4),
            AMDILRegister::NamedOutputRegister(name) => HLSLRegister::ShaderOutput(name.clone(), 4),
            AMDILRegister::Texture2D(idx) => HLSLRegister::Texture2D(*idx),
            AMDILRegister::Texture3D(idx) => HLSLRegister::Texture3D(*idx),
            AMDILRegister::TextureCube(idx) => HLSLRegister::TextureCube(*idx),
            AMDILRegister::NamedRegister(name) => HLSLRegister::GenericRegister(name.clone(), 4)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILRegister {
    NamedRegister(String),
    Literal(Box<[u64; 4]>),
    NamedBuffer { name: String, idx: u64 },
    NamedInputRegister(String),
    NamedOutputRegister(String),
    Texture2D(u64),
    Texture3D(u64),
    TextureCube(u64)
}
impl VMName for AMDILRegister {
    fn is_pure_input(&self) -> bool {
        match self {
            Self::Texture2D(_) | Self::TextureCube(_) => true, // Assuming textures are read only
            Self::Literal(..) => true,
            Self::NamedInputRegister(..) => true,
            // TODO consider concept of i/o buffers
            Self::NamedBuffer { .. } => true,
            _ => false,
        }
    }

    fn is_output(&self) -> bool {
        match self {
            Self::NamedOutputRegister(_) => true,
            _ => false,
        }
    }

    fn toplevel_kind(&self) -> HLSLKind {
        match self {
            Self::Texture2D(_) => HLSLKind::TEXTURE2D,
            Self::TextureCube(_) => HLSLKind::TEXTURECUBE,
            _ => HLSLKind::NUMERIC,
        }
    }
}
impl VMVector for AMDILRegister {
    fn n_components(&self) -> usize {
        match self {
            Self::Texture2D(_) => 1,
            Self::TextureCube(_) => 1,
            _ => 4,
        }
    }
}
impl Reg for AMDILRegister {
    fn indexable_depth(&self) -> usize {
        0
    }
    fn output_kind(&self) -> HLSLKind {
        self.toplevel_kind()
    }
}

pub type AMDILVector = Vector<AMDILRegister>;

// impl AMDILVector {
//     pub fn new(reg: AMDILRegister, swizzle: MaskedSwizzle) -> Self {
//         Self(reg, swizzle)
//     }

//     pub fn register(&self) -> &AMDILRegister {
//         &self.0
//     }
//     pub fn swizzle(&self) -> MaskedSwizzle {
//         self.1
//     }

//     pub fn named_register(name: String, swizzle: MaskedSwizzle) -> Self {
//         Self::new(AMDILRegister::NamedRegister(name), swizzle)
//     }
//     pub fn literal(data: Box<[u64; 4]>, swizzle: MaskedSwizzle) -> Self {
//         Self::new(AMDILRegister::Literal(data), swizzle)
//     }
//     pub fn named_buffer(name: String, idx: u64, swizzle: MaskedSwizzle) -> Self {
//         Self::new(AMDILRegister::NamedBuffer { name, idx }, swizzle)
//     }
//     pub fn named_input_register(name: String, swizzle: MaskedSwizzle) -> Self {
//         Self::new(AMDILRegister::NamedInputRegister(name), swizzle)
//     }
//     pub fn named_output_register(name: String, swizzle: MaskedSwizzle) -> Self {
//         Self::new(AMDILRegister::NamedOutputRegister(name), swizzle)
//     }
// }
// impl VMName for AMDILMaskSwizVector {
//     fn is_pure_input(&self) -> bool {
//         self.0.is_pure_input()
//     }

//     fn is_output(&self) -> bool {
//         self.0.is_output()
//     }

//     fn toplevel_kind(&self) -> HLSLKind {
//         self.0.toplevel_kind()
//     }
// }
// impl VMVector for AMDILMaskSwizVector {
//     fn n_components(&self) -> usize {
//         self.1.num_used_components() // TODO: ???
//     }
// }
// impl From<&AMDILMaskSwizVector> for HLSLVector {
//     fn from(value: &AMDILMaskSwizVector) -> Self {

//         VectorOf::new(&AMDILAbstractVM::decompose(&value).into_iter().map(|comp| {
//             match comp.vec {
//                 AMDILRegister::NamedRegister(name) => HLSLScalar::Component(HLSLRegister::GenericRegister(name, 4), comp.comp),
//                 AMDILRegister::Literal(arr) => HLSLScalar::Literal(arr[comp.comp.into_index()] as u32),
//                 AMDILRegister::NamedBuffer { name, idx } => HLSLScalar::Component(HLSLRegister::ArrayElement { of: Box::new(HLSLRegister::ShaderInput(name, 4)), idx }, comp.comp),
//                 AMDILRegister::NamedInputRegister(name) => HLSLScalar::Component(HLSLRegister::ShaderInput(name, 4), comp.comp),
//                 AMDILRegister::NamedOutputRegister(name) => HLSLScalar::Component(HLSLRegister::ShaderOutput(name, 4), comp.comp),
//                 AMDILRegister::Texture(idx) => HLSLScalar::Component(HLSLRegister::Texture(idx), comp.comp),
//             }
//         }).collect::<Vec<_>>()).unwrap()
//     }
// }
// impl From<&ComponentOf<AMDILRegister>> for HLSLScalar {
//     fn from(comp: &ComponentOf<AMDILRegister>) -> Self {
//         match &comp.vec {
//             AMDILRegister::NamedRegister(name) => HLSLScalar::Component(HLSLRegister::GenericRegister(name.clone(), 4), comp.comp),
//             AMDILRegister::Literal(arr) => HLSLScalar::Literal(arr[comp.comp.into_index()] as u32),
//             AMDILRegister::NamedBuffer { name, idx } => HLSLScalar::Component(HLSLRegister::ArrayElement { of: Box::new(HLSLRegister::ShaderInput(name.clone(), 4)), idx: *idx }, comp.comp),
//             AMDILRegister::NamedInputRegister(name) => HLSLScalar::Component(HLSLRegister::ShaderInput(name.clone(), 4), comp.comp),
//             AMDILRegister::NamedOutputRegister(name) => HLSLScalar::Component(HLSLRegister::ShaderOutput(name.clone(), 4), comp.comp),
//             AMDILRegister::Texture(idx) => HLSLScalar::Component(HLSLRegister::Texture(*idx), comp.comp),
//         }
//     }
// }

// impl VMName for AMDILDataRef {
//     fn is_pure_input(&self) -> bool {
//         self.name.is_pure_input()
//     }
// }
// impl VMDataRef<AMDILNameRef> for AMDILDataRef {
//     fn name(&self) -> &AMDILNameRef {
//         &self.name
//     }

//     fn hlsl_kind(&self) -> HLSLKind {
//         self.name.base_hlsl_kind()
//     }
// }
// impl VMVectorDataRef<AMDILNameRef> for AMDILDataRef {
//     fn swizzle(&self) -> MaskedSwizzle {
//         self.swizzle
//     }
// }
// impl VMDataRef<AMDILNameRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn name(&self) -> &AMDILNameRef {
//         &self.name.name
//     }

//     fn hlsl_kind(&self) -> HLSLKind {
//         self.kind
//     }
// }
// impl VMVectorDataRef<AMDILNameRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn swizzle(&self) -> MaskedSwizzle {
//         self.name.swizzle
//     }
// }
// impl From<AMDILDataRef> for RefinableVMDataRef<AMDILDataRef> {
//     fn from(data: AMDILDataRef) -> Self {
//         Self {
//             kind: data.hlsl_kind(),
//             name: data,
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AMDILDeclaration {
    Texture2D(u64),
    Texture3D(u64),
    TextureCube(u64),
    NamedLiteral(String, [u64; 4]),
    NamedBuffer {
        name: String,
        len: u64,
    },
    NamedInputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
    NamedOutputRegister {
        name: String,
        // TODO maybe this should be a mask instead of a length
        len: u8,
        reg_type: String,
    },
}
impl AMDILDeclaration {
    pub fn get_decl(&self) -> Option<AMDILRegister> {
        match self {
            AMDILDeclaration::Texture2D(id) => Some(AMDILRegister::Texture2D(*id)),
            AMDILDeclaration::Texture3D(id) => Some(AMDILRegister::Texture3D(*id)),
            AMDILDeclaration::TextureCube(id) => Some(AMDILRegister::TextureCube(*id)),
            AMDILDeclaration::NamedInputRegister {
                name,
                len: _,
                reg_type: _,
            } => Some(AMDILRegister::NamedInputRegister(
                name.clone(),
            )),
            AMDILDeclaration::NamedOutputRegister {
                name,
                len: _,
                reg_type: _,
            } => Some(AMDILRegister::NamedOutputRegister(
                name.clone(),
            )),
            // TODO re-enable this
            // AMDILDeclaration::NamedBuffer { name, len } => {
            //     vec![Outcome::Declaration {
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
            _ => None,
        }
    }
}
