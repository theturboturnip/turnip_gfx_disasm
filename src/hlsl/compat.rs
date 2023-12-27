use crate::{abstract_machine::AbstractVM, Action, Program};

use super::{vm::HLSLAbstractVM, HLSLRegister, HLSLAction};

/// Trait for abstract VMs that are capable of translation to HLSL.
///
/// TODO figure out what we need from this trait.
pub trait HLSLCompatibleAbstractVM: AbstractVM
{
    fn convert_action(a: &Action<Self::Register>) -> HLSLAction;
    fn convert_register(r: &Self::Register) -> HLSLRegister;
}

pub struct HLSLCompatProgram {
    pub actions: Vec<HLSLAction>,
    pub io_registers: Vec<HLSLRegister>,
}
impl Program<HLSLAbstractVM> for HLSLCompatProgram {
    fn io_declarations(&self) -> &Vec<<HLSLAbstractVM as AbstractVM>::Register> {
        &self.io_registers
    }

    fn actions(&self) -> &Vec<HLSLAction> {
        &self.actions
    }
}
pub fn program_to_hlsl<T: HLSLCompatibleAbstractVM, P: Program<T>>(p: &P) -> HLSLCompatProgram {
    HLSLCompatProgram {
        actions: p.actions().into_iter().map(T::convert_action).collect(),
        io_registers: p.io_declarations().into_iter().map(T::convert_register).collect()
    }
}