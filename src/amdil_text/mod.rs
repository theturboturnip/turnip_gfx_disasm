use std::marker::PhantomData;

use crate::{Decoder, Program, abstract_machine::VMName, Action};

mod decode;
pub use decode::AMDILErrorContext;

pub mod vm;

use self::{decode::Instruction, vm::{AMDILAbstractVM, AMDILRegister, AMDILAction}};

/// The type returned by [AMDILDecoder] holding the instructions for a given AMDIL program
pub struct AMDILProgram {
    io_registers: Vec<AMDILRegister>,
    actions: Vec<AMDILAction>,
}
impl Program<AMDILAbstractVM> for AMDILProgram {
    fn io_declarations(&self) -> &Vec<<AMDILAbstractVM as crate::AbstractVM>::Register> {
        &self.io_registers
    }
    fn actions(&self) -> &Vec<AMDILAction> {
        &self.actions
    }
}

/// Decoder for AMDIL text disassembly
pub struct AMDILDecoder<'a> {
    _lifetime: PhantomData<&'a ()>, // NOTE: there's no generic type here!
}
impl<'a> AMDILDecoder<'a> {
    pub fn new() -> AMDILDecoder<'a> {
        AMDILDecoder {
            _lifetime: PhantomData::default(),
        }
    }
}
impl<'a> AMDILDecoder<'a> {
    fn instr_to_action(&mut self, instr: Instruction) -> Option<AMDILAction> {
        match instr {
            Instruction::Decl(decl) => None,
            Instruction::Alu(alu) => Some(alu.to_action()),
            Instruction::EarlyOut(scalar) => {
                Some(
                    Action::If {
                        expr: scalar,
                        if_true: vec![Action::EarlyOut],
                        if_fals: vec![]
                    }
                )
            }
            Instruction::If { cond, if_true, if_fals } => {
                let if_true = if_true
                    .into_iter()
                    .filter_map(|instr| self.instr_to_action(instr))
                    .collect();
                let if_fals = if_fals
                    .into_iter()
                    .filter_map(|instr| self.instr_to_action(instr))
                    .collect();
                Some(
                    Action::If {
                        expr: cond,
                        if_true,
                        if_fals,
                    }
                )
            }
        }
    }
}
impl<'a> Decoder<AMDILAbstractVM> for AMDILDecoder<'a> {
    type Input = &'a str;
    type Program = AMDILProgram;
    type Err = AMDILErrorContext;

    fn decode(mut self, data: Self::Input) -> Result<AMDILProgram, AMDILErrorContext> {
        let (io_registers, instructions) = decode::parse_lines(data)?;

        let mut actions = vec![];
        for instr in instructions {
            match self.instr_to_action(instr) {
                Some(action) => actions.push(action),
                None => {},
            }
        }

        // Return
        Ok(AMDILProgram {
            io_registers,
            actions,
        })
    }
}
