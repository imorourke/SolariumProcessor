use std::fmt::Display;

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, INST_SIZE, Instruction, OpAdd, OpJmpri, OpLd, OpLdi,
    OpLdn, OpPopr, OpPush, OpSav, OpSub, OpTz,
};

use crate::{
    TokenError,
    compiler::{CodeGenerationOptions, Statement},
    expressions::{RegisterDef, TemporaryStackTracker},
    tokenizer::Token,
};

pub fn load_to_register(reg: Register, val: u32) -> Vec<AsmToken> {
    if val <= u16::MAX as u32 {
        vec![AsmToken::OperationLiteral(Box::new(OpLdi::new(
            ArgumentType::new(reg, DataType::U16),
            val as u16,
        )))]
    } else {
        vec![
            AsmToken::OperationLiteral(Box::new(OpLdn::new(ArgumentType::new(reg, DataType::U32)))),
            AsmToken::Literal4(val),
        ]
    }
}

#[derive(Debug, Clone)]
pub struct MemcpyStatement {
    token: Token,
    from_addr: Register,
    to_addr: Register,
    size: usize,
}

impl MemcpyStatement {
    pub fn new(token: Token, from_addr: Register, to_addr: Register, size: usize) -> Self {
        Self {
            token,
            from_addr,
            to_addr,
            size,
        }
    }

    fn get_avail_reg(&self, used_regs: &[Register]) -> Result<Register, TokenError> {
        for i in RegisterDef::FIRST_USEABLE.get_index()..=Register::last_register().get_index() {
            let r = Register::GeneralPurpose(i);

            if !used_regs.contains(&r) {
                return Ok(r);
            }
        }

        Err(self
            .token
            .clone()
            .into_err("unable to find available register for memcpy"))
    }
}

impl Statement for MemcpyStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        let spare_reg = RegisterDef::SPARE;
        let val_reg = self.get_avail_reg(&[self.from_addr, self.to_addr, spare_reg])?;
        let num_reg = self.get_avail_reg(&[self.from_addr, self.to_addr, spare_reg, val_reg])?;

        asm.extend_from_slice(&load_to_register(spare_reg, self.size as u32));

        if options.debug_locations {
            asm.push(AsmToken::Comment(format!("memcpy {}", self.token)));
        }

        asm.push(AsmToken::OperationLiteral(Box::new(OpPush::new(
            self.to_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpPush::new(
            self.from_addr.into(),
        ))));

        if self.size <= 16 * INST_SIZE {
            let mut remaining = self.size;

            let dtypes = [DataType::U32, DataType::U16, DataType::U8];

            for d in dtypes {
                asm.push(AsmToken::OperationLiteral(Box::new(OpLdi::new(
                    ArgumentType::new(num_reg, DataType::U16),
                    d.byte_size() as u16,
                ))));

                let asm_load_save = [
                    AsmToken::OperationLiteral(Box::new(OpLd::new(
                        ArgumentType::new(val_reg, d),
                        self.from_addr.into(),
                    ))),
                    AsmToken::OperationLiteral(Box::new(OpSav::new(
                        ArgumentType::new(self.to_addr, d),
                        val_reg.into(),
                    ))),
                ];

                let asm_incr = [
                    AsmToken::OperationLiteral(Box::new(OpAdd::new(
                        ArgumentType::new(self.to_addr, DataType::U32),
                        self.to_addr.into(),
                        num_reg.into(),
                    ))),
                    AsmToken::OperationLiteral(Box::new(OpAdd::new(
                        ArgumentType::new(self.from_addr, DataType::U32),
                        self.from_addr.into(),
                        num_reg.into(),
                    ))),
                ];

                while remaining >= d.byte_size() {
                    asm.extend(asm_load_save.clone());
                    remaining -= d.byte_size();

                    if remaining > 0 {
                        asm.extend(asm_incr.clone());
                    }
                }
            }
        } else {
            asm.push(AsmToken::OperationLiteral(Box::new(OpLdi::new(
                ArgumentType::new(num_reg, DataType::U16),
                1,
            ))));

            // Define helper functions for the number of instructions in the loop
            enum LoopInstruction {
                InstructionCount(Box<dyn Fn(usize) -> Box<dyn Instruction>>),
                Raw(Box<dyn Instruction>),
            }

            // Define the core loop. Use count - 1 for the sizes to not include the Tz instruction
            let copy_loop_asm: Vec<LoopInstruction> = vec![
                LoopInstruction::Raw(Box::new(OpTz::new(spare_reg.into()))),
                LoopInstruction::InstructionCount(Box::new(|count| {
                    Box::new(OpJmpri::new(((count - 1) * INST_SIZE) as i16))
                })),
                LoopInstruction::Raw(Box::new(OpLd::new(
                    ArgumentType::new(val_reg, DataType::U8),
                    self.from_addr.into(),
                ))),
                LoopInstruction::Raw(Box::new(OpSav::new(
                    ArgumentType::new(self.to_addr, DataType::U8),
                    val_reg.into(),
                ))),
                LoopInstruction::Raw(Box::new(OpAdd::new(
                    ArgumentType::new(self.to_addr, DataType::U32),
                    self.to_addr.into(),
                    num_reg.into(),
                ))),
                LoopInstruction::Raw(Box::new(OpAdd::new(
                    ArgumentType::new(self.from_addr, DataType::U32),
                    self.from_addr.into(),
                    num_reg.into(),
                ))),
                LoopInstruction::Raw(Box::new(OpSub::new(
                    ArgumentType::new(spare_reg, DataType::U32),
                    spare_reg.into(),
                    num_reg.into(),
                ))),
                LoopInstruction::InstructionCount(Box::new(|count| {
                    Box::new(OpJmpri::new(-((count - 1) as i16) * INST_SIZE as i16))
                })),
            ];

            let loop_count = copy_loop_asm.len();
            asm.extend(
                copy_loop_asm
                    .into_iter()
                    .map(|x| match x {
                        LoopInstruction::Raw(x) => x,
                        LoopInstruction::InstructionCount(x) => x(loop_count),
                    })
                    .map(AsmToken::OperationLiteral),
            );
        }

        asm.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
            self.from_addr.into(),
        ))));
        asm.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
            self.to_addr.into(),
        ))));

        Ok(self.token.to_asm_iter(asm).into_iter().collect())
    }
}

impl Display for MemcpyStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "memcpy {} -> {} @ {}",
            self.from_addr, self.to_addr, self.size
        )
    }
}
