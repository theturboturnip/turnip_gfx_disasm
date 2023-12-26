use std::marker::PhantomData;

use crate::{Decoder, Program, abstract_machine::{VMName, vector::ComponentOf}, Action, hlsl::{kinds::HLSLKindBitmask, syntax::HLSLOperator}};

mod decode;
pub use decode::AMDILError;

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
impl<'a> Decoder<AMDILAbstractVM> for AMDILDecoder<'a> {
    type Input = &'a str;
    type Program = AMDILProgram;
    type Err = AMDILError;

    fn decode(&self, data: Self::Input) -> Result<AMDILProgram, AMDILError> {
        let instructions = decode::parse_lines(data)?;

        let mut actions = vec![];
        let mut io_registers = vec![];
        for instr in instructions {
            match &instr {
                Instruction::Decl(decl) => {
                    match decl.get_decl().filter(|r| r.is_pure_input() || r.is_output()) {
                        Some(io_reg) => io_registers.push(io_reg),
                        None => {}
                    }
                },
                Instruction::Alu(alu) => alu.push_actions(&mut actions),
                Instruction::EarlyOut(vec, comp) => {
                    actions.push(
                        Action::If {
                            inputs: vec![(ComponentOf{ vec: vec.clone(), comp: *comp }, HLSLKindBitmask::NUMERIC.into())],
                            cond_operator: HLSLOperator::Assign,
                            if_true: vec![Action::EarlyOut],
                            if_fals: vec![]
                        }
                    )
                }
                _ => {}
            }
        }

        // Return
        Ok(AMDILProgram {
            io_registers,
            actions,
        })
    }
}
