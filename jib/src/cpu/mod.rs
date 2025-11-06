mod instruction;
mod operations;
mod register;

use alloc::{fmt, rc::Rc, vec::Vec};
use core::cell::RefCell;
use core::hash::{Hash, Hasher};

pub use crate::cpu::instruction::{DataType, Instruction, UnknownDataTypeArg};
use crate::device::{DeviceAction, ProcessorDevice};
use crate::memory::{MemoryError, MemoryMap, MemorySegment};

pub use self::operations::{
    ArithmeticOperations, BinaryOperations, OperationError, OperatorManager, RelationalOperations,
    convert_types,
};

use self::register::RegisterFlag;
pub use self::register::{Register, RegisterError, RegisterManager};

#[derive(Debug, Clone)]
pub enum ProcessorError {
    Memory(MemoryError),
    UnsupportedInterrupt(Interrupt),
    UnknownInterruptIndex(u32),
    Register(RegisterError),
    UnknownInstruction(Instruction),
    UnsupportedDataType(Instruction, DataType),
    Operation(OperationError),
    StackUnderflow,
    DataType(UnknownDataTypeArg),
    OpcodeAlignment(u32),
}

impl fmt::Display for ProcessorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Memory(m) => write!(f, "Memory Error => {m}"),
            Self::DataType(dt) => write!(f, "Data Type Error => {dt}"),
            Self::UnsupportedInterrupt(i) => write!(f, "Unsupported Interrupt => {i}"),
            Self::UnknownInterruptIndex(i) => write!(f, "Unknown Interrupt Index => {i}"),
            Self::Register(r) => write!(f, "Register Error => {r}"),
            Self::UnknownInstruction(i) => write!(f, "Unknown Instruction => {i}"),
            Self::UnsupportedDataType(i, dt) => {
                write!(f, "Unsupported Data Type {dt} for Instruction {i}")
            }
            Self::Operation(o) => write!(f, "Operation Error => {o}"),
            Self::StackUnderflow => write!(f, "Stack Underflow"),
            Self::OpcodeAlignment(o) => write!(f, "Opcode Alignment Error => 0x{o:08x}"),
        }
    }
}

impl From<MemoryError> for ProcessorError {
    fn from(value: MemoryError) -> Self {
        Self::Memory(value)
    }
}

impl From<RegisterError> for ProcessorError {
    fn from(value: RegisterError) -> Self {
        Self::Register(value)
    }
}

impl From<UnknownDataTypeArg> for ProcessorError {
    fn from(value: UnknownDataTypeArg) -> Self {
        Self::DataType(value)
    }
}

impl From<OperationError> for ProcessorError {
    fn from(value: OperationError) -> Self {
        Self::Operation(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResetType {
    Hard,
    Soft,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Opcode {
    base: u8,
    code: u8,
}

impl Opcode {
    pub fn to_byte(&self) -> u8 {
        ((self.base & 0xF) << 4) | (self.code & 0xF)
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{} ({})", self.base, self.code, self.to_byte())
    }
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        Self {
            base: (value >> 4) & 0xF,
            code: value & 0xF,
        }
    }
}

impl Hash for Opcode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base.hash(state);
        self.code.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Interrupt {
    Software(u32),
    Hardware(u32),
}

impl Interrupt {
    pub const fn to_index(self) -> u32 {
        match self {
            Self::Hardware(x) => x,
            Self::Software(x) => Processor::NUM_INTERRUPT_PER_CLASS + x,
        }
    }
}

impl core::cmp::PartialOrd for Interrupt {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl core::cmp::Ord for Interrupt {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        match self {
            Interrupt::Hardware(is) => match other {
                Interrupt::Hardware(io) => is.cmp(io),
                Interrupt::Software(_) => core::cmp::Ordering::Less,
            },
            Interrupt::Software(is) => match other {
                Interrupt::Hardware(_) => core::cmp::Ordering::Greater,
                Interrupt::Software(io) => is.cmp(io),
            },
        }
    }
}

impl fmt::Display for Interrupt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Software(v) => write!(f, "Software({v}"),
            Self::Hardware(v) => write!(f, "Hardware({v}"),
        }
    }
}

impl TryFrom<u32> for Interrupt {
    type Error = ProcessorError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value < Processor::NUM_INTERRUPT_PER_CLASS {
            Ok(Self::Hardware(value))
        } else if value < InterruptController::NUM_INTERRUPTS {
            Ok(Self::Software(value - Processor::NUM_INTERRUPT_PER_CLASS))
        } else {
            Err(ProcessorError::UnknownInterruptIndex(value))
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct InterruptController {
    interrupts: u64,
}

impl InterruptController {
    /// The total number of supported interrupts allowed by the controller
    const NUM_INTERRUPTS: u32 = u64::BITS;

    /// Determines the highest-priority interrupt to trigger next
    pub fn has_interrupt(&self) -> Option<u32> {
        // We want to find the minum bit set in the integer.
        // To do this, we will reverse the bits and take the ilog2. Without reversing,
        // this will give the maximum bit set. Therefore, by reversing, this will give us
        // the minimum bit set. Because ilog2 rounds down, after reversing, we will have to
        // add one back, so that 0b1 yields a value of 0, 0b0 yields None, and 0b10 yields 1.
        self.interrupts
            .reverse_bits()
            .checked_ilog2()
            .map(|x| u64::BITS - (x + 1))
    }

    /// Queue an interrupt for processing
    pub fn queue_interrupt(&mut self, index: u32) -> Result<(), ProcessorError> {
        if index < Self::NUM_INTERRUPTS {
            self.interrupts |= 1 << index;
            Ok(())
        } else {
            Err(ProcessorError::UnknownInterruptIndex(index))
        }
    }

    /// Clear a given interrupt after processing
    pub fn clear_interrupt(&mut self, index: u32) -> Result<(), ProcessorError> {
        if index < Self::NUM_INTERRUPTS {
            self.interrupts &= !(1 << index);
            Ok(())
        } else {
            Err(ProcessorError::UnknownInterruptIndex(index))
        }
    }

    /// Resets the interrupt controller
    pub fn reset(&mut self) {
        self.interrupts = 0;
    }
}

#[derive(Default)]
pub struct Processor {
    memory: MemoryMap,
    devices: Vec<Rc<RefCell<dyn ProcessorDevice>>>,
    registers: RegisterManager,
    operator_manager: OperatorManager,
    interrupt_controller: InterruptController,
}

impl Processor {
    /// Defines the number of bytes per memory address (size of the default memory word)
    pub const BYTES_PER_WORD: u32 = core::mem::size_of::<u32>() as u32;

    /// Defines the hard reset vector location
    pub const HARD_RESET_VECTOR: u32 = 0;

    /// Defines the number of hard reset location
    pub const SOFT_RESET_VECTOR: u32 = Self::BYTES_PER_WORD;

    /// Defines the number of supported interrupts per interrupt class
    pub const NUM_INTERRUPT_PER_CLASS: u32 = InterruptController::NUM_INTERRUPTS / 2;

    ///Providesthe base address for the hardware interrupts
    pub const BASE_HW_INT_ADDR: u32 = 0x100;

    /// Provides the base address for the software interrupts
    pub const BASE_SW_INT_ADDR: u32 =
        Self::BASE_HW_INT_ADDR + Self::BYTES_PER_WORD * Self::NUM_INTERRUPT_PER_CLASS;

    /// Provides the top address (next free address) after the vector memory segments
    pub const TOP_VEC_SEG_ADDR: u32 =
        Self::BASE_SW_INT_ADDR + Self::BYTES_PER_WORD * Self::NUM_INTERRUPT_PER_CLASS;

    const OP_BASE_CPU: u8 = 0;
    pub const OP_NOOP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 0,
    };
    pub const OP_RESET: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 1,
    };
    pub const OP_INTERRUPT: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 2,
    };
    pub const OP_INTERRUPT_REGISTER: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 3,
    };
    pub const OP_INTERRUPT_RETURN: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 4,
    };
    pub const OP_CALL: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 5,
    };
    pub const OP_RETURN: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 6,
    };
    pub const OP_PUSH: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 7,
    };
    pub const OP_POP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 8,
    };
    pub const OP_POP_REG: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 9,
    };
    pub const OP_JUMP: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 10,
    };
    pub const OP_JUMP_REL: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 11,
    };
    pub const OP_JUMP_REL_IMM: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 12,
    };
    pub const OP_HALT: Opcode = Opcode {
        base: Self::OP_BASE_CPU,
        code: 15,
    };

    const OP_BASE_MEM: u8 = 1;
    pub const OP_LOAD: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 0,
    };
    pub const OP_LOAD_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 1,
    };
    pub const OP_LOAD_IMM: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 2,
    };
    pub const OP_LOAD_IMM_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 3,
    };
    pub const OP_LOAD_NEXT: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 4,
    };
    pub const OP_LOAD_NEXT_OFFSET: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 5,
    };
    pub const OP_SAVE: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 6,
    };
    pub const OP_SAVE_REL: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 7,
    };
    pub const OP_COPY: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 8,
    };
    pub const OP_CONV: Opcode = Opcode {
        base: Self::OP_BASE_MEM,
        code: 9,
    };

    const OP_BASE_TEST: u8 = 2;
    pub const OP_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 0,
    };
    pub const OP_NEQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 1,
    };
    pub const OP_GREATER: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 2,
    };
    pub const OP_GREATER_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 3,
    };
    pub const OP_LESS: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 4,
    };
    pub const OP_LESS_EQ: Opcode = Opcode {
        base: Self::OP_BASE_TEST,
        code: 5,
    };

    const OP_BASE_LOGIC: u8 = 3;
    pub const OP_NOT: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 0,
    };
    pub const OP_BOOL: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 1,
    };
    pub const OP_TEST_ZERO: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 2,
    };
    pub const OP_TEST_NOT_ZERO: Opcode = Opcode {
        base: Self::OP_BASE_LOGIC,
        code: 3,
    };

    const OP_BASE_STATUS_FLAGS: u8 = 4;
    pub const OP_INTERRUPT_ENABLE: Opcode = Opcode {
        base: Self::OP_BASE_STATUS_FLAGS,
        code: 0,
    };
    pub const OP_INTERRUPT_DISABLE: Opcode = Opcode {
        base: Self::OP_BASE_STATUS_FLAGS,
        code: 1,
    };

    const OP_BASE_DEBUG: u8 = 5;
    pub const OP_DEBUG_BREAK: Opcode = Opcode {
        base: Self::OP_BASE_DEBUG,
        code: 0,
    };

    const OP_BASE_MATH: u8 = 10;
    pub const OP_ADD: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 0,
    };
    pub const OP_SUB: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 1,
    };
    pub const OP_MUL: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 2,
    };
    pub const OP_DIV: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 3,
    };
    pub const OP_REM: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 4,
    };
    pub const OP_NEG: Opcode = Opcode {
        base: Self::OP_BASE_MATH,
        code: 5,
    };

    const OP_BASE_BITS: u8 = 11;
    pub const OP_BAND: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 0,
    };
    pub const OP_BOR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 1,
    };
    pub const OP_BXOR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 2,
    };
    pub const OP_BSHL: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 3,
    };
    pub const OP_BSHR: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 4,
    };

    pub const OP_BNOT: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 5,
    };
    pub const OP_BSWAP: Opcode = Opcode {
        base: Self::OP_BASE_BITS,
        code: 6,
    };

    /// Resets the current CPU based on the given soft/hard reset vector
    pub fn reset(&mut self, reset_type: ResetType) -> Result<(), ProcessorError> {
        if ResetType::Hard == reset_type {
            self.memory.reset();
        }

        let reset_vec_addr = match reset_type {
            ResetType::Hard => Self::HARD_RESET_VECTOR,
            ResetType::Soft => Self::SOFT_RESET_VECTOR,
        };

        self.registers.reset();
        self.registers.set(
            Register::ProgramCounter,
            self.memory.get_u32(reset_vec_addr)?,
        )?;
        self.registers
            .set_status_flag(RegisterFlag::InterruptEnable, false)?;

        self.interrupt_controller.reset();

        Ok(())
    }

    /// Provides the current state of the registers
    pub fn get_register_state(&self) -> RegisterManager {
        self.registers
    }

    /// provides the resulting address at the given interrupt vector for the specified interrupt
    pub fn interrupt_address(int: Interrupt) -> Result<u32, ProcessorError> {
        let (base, num) = match int {
            Interrupt::Software(n) => (Self::BASE_SW_INT_ADDR, n),
            Interrupt::Hardware(n) => (Self::BASE_HW_INT_ADDR, n),
        };

        if num < Self::NUM_INTERRUPT_PER_CLASS {
            Ok(base + num * Self::BYTES_PER_WORD)
        } else {
            Err(ProcessorError::UnsupportedInterrupt(int))
        }
    }

    /// Queues an interrupt for processing. If an interrupt is already queued, the lower-numbered interrupt
    /// will have priority. Hardware interrupts have a higher
    /// priority than software interrupts.
    fn queue_interrupt(&mut self, int: Interrupt) -> Result<bool, ProcessorError> {
        self.interrupt_controller.queue_interrupt(int.to_index())?;
        Ok(true)
    }

    pub fn trigger_hardware_interrupt(&mut self, num: u32) -> Result<bool, ProcessorError> {
        self.interrupt_controller
            .queue_interrupt(Interrupt::Hardware(num).to_index())?;
        Ok(true)
    }

    fn call_interrupt(
        &mut self,
        int: Interrupt,
        data: Option<u32>,
    ) -> Result<bool, ProcessorError> {
        // Return false if interrupts are not allowed
        if !self
            .registers
            .get_status_flag(RegisterFlag::InterruptEnable)?
        {
            return Ok(false);
        }

        // Obtain the desired value from the program counter
        let new_pc = self.memory.get_u32(Self::interrupt_address(int)?)?;
        if new_pc == 0 {
            self.interrupt_controller.clear_interrupt(int.to_index())?;
            return Ok(true);
        }

        // Increment the program counter before calling if it is currently on a HALT instruction
        if self.get_current_op()? == Self::OP_HALT {
            self.registers.set(
                Register::ProgramCounter,
                self.get_current_pc()? + Self::BYTES_PER_WORD,
            )?;
        }

        // Update the program counter to the value in the interrupt vector
        self.push_all_registers()?;
        self.set_executing_interrupt(Some(int.to_index()))?;
        self.registers.set(Register::ProgramCounter, new_pc)?;

        // Add the data field if provided
        if let Some(val) = data {
            self.registers.set(Register::ArgumentBase, val)?;
        }

        // Return true if the interrupt was called
        Ok(true)
    }

    fn get_executing_interrupt(&self) -> Result<Option<u32>, ProcessorError> {
        if self
            .registers
            .get_status_flag(RegisterFlag::InterruptExecuting)?
        {
            Ok(Some(
                self.registers
                    .get_status_value(RegisterFlag::InterruptValue)?,
            ))
        } else {
            Ok(None)
        }
    }

    fn set_executing_interrupt(&mut self, int: Option<u32>) -> Result<(), ProcessorError> {
        if let Some(index) = int {
            self.registers
                .set_status_flag(RegisterFlag::InterruptExecuting, true)?;
            self.registers
                .set_status_value(RegisterFlag::InterruptValue, index)?;
        } else {
            self.registers
                .set_status_flag(RegisterFlag::InterruptExecuting, false)?;
            self.registers
                .set_status_value(RegisterFlag::InterruptValue, 0)?;
        }
        Ok(())
    }

    fn push_all_registers(&mut self) -> Result<(), ProcessorError> {
        let reg_vals = self.registers.get_state();

        for r in reg_vals {
            self.stack_push(r)?;
        }

        Ok(())
    }

    fn pop_all_registers(&mut self) -> Result<(), ProcessorError> {
        let mut current_state = self.registers.get_state();

        for v in current_state.iter_mut().rev() {
            *v = self.stack_pop()?;
        }

        self.registers.set_state(current_state);

        Ok(())
    }

    /// Pops all registers, preserving the returning function's return register and
    /// interrupt status flag entries.
    fn pop_all_registers_func_return(&mut self) -> Result<(), ProcessorError> {
        let mut current_state = self.registers.get_state();

        for (i, v) in current_state.iter_mut().enumerate().rev() {
            let val = self.stack_pop()?;

            if i != Register::Return.get_index() {
                *v = val;
            }
        }

        self.registers.set_state(current_state);

        Ok(())
    }

    pub fn memory_set(&mut self, address: u32, val: u8) -> Result<(), ProcessorError> {
        self.memory.set(address, val)?;
        Ok(())
    }

    pub fn memory_inspect(&self, address: u32) -> Result<u8, ProcessorError> {
        Ok(self.memory.inspect(address)?)
    }

    pub fn memory_inspect_u32(&self, address: u32) -> Result<u32, ProcessorError> {
        Ok(self.memory.inspect_u32(address)?)
    }

    pub fn memory_add_segment(
        &mut self,
        address: u32,
        seg: Rc<RefCell<dyn MemorySegment>>,
    ) -> Result<(), ProcessorError> {
        self.memory.add_segment(address, seg)?;
        Ok(())
    }

    pub fn device_add(
        &mut self,
        seg: Rc<RefCell<dyn ProcessorDevice>>,
    ) -> Result<(), ProcessorError> {
        self.devices.push(seg);
        Ok(())
    }

    pub fn get_current_pc(&self) -> Result<u32, ProcessorError> {
        let pc = self.registers.get(Register::ProgramCounter)?;
        if pc % Self::BYTES_PER_WORD != 0 {
            return Err(ProcessorError::OpcodeAlignment(pc));
        }
        Ok(pc)
    }

    pub fn get_current_inst(&self) -> Result<Instruction, ProcessorError> {
        Ok(Instruction::from(
            self.memory_inspect_u32(self.get_current_pc()?)?,
        ))
    }

    pub fn get_current_op(&self) -> Result<Opcode, ProcessorError> {
        Ok(Opcode::from(self.get_current_inst()?.opcode()))
    }

    pub fn step(&mut self) -> Result<(), ProcessorError> {
        // Run the next interrupt if there is an interrupt queued and either:
        // (1) no interrupt is currently running
        // (2) the next interrupt has a lower number
        if let Some(next_interrupt) = self.interrupt_controller.has_interrupt()
            && self
                .get_executing_interrupt()?
                .is_none_or(|x| next_interrupt < x)
        {
            self.call_interrupt(next_interrupt.try_into()?, None)?;
        }

        let mut inst_jump = Some(1);

        let pc = self.get_current_pc()?;
        let inst = Instruction::from(self.memory.get_u32(pc)?);
        let opcode = Opcode::from(inst.opcode());

        match opcode {
            Self::OP_NOOP | Self::OP_DEBUG_BREAK => (),
            Self::OP_RESET => {
                self.reset(ResetType::Soft)?;
                inst_jump = None;
            }
            Self::OP_INTERRUPT_ENABLE => self
                .registers
                .set_status_flag(RegisterFlag::InterruptEnable, true)?,
            Self::OP_INTERRUPT_DISABLE => self
                .registers
                .set_status_flag(RegisterFlag::InterruptEnable, false)?,
            Self::OP_INTERRUPT => {
                self.queue_interrupt(Interrupt::Software(inst.imm_unsigned()))?;
            }
            Self::OP_INTERRUPT_REGISTER => {
                self.queue_interrupt(Interrupt::Software(
                    self.registers.get(inst.arg0_register())?,
                ))?;
            }
            Self::OP_CALL => {
                // Increment the program counter before pushing registers so we return to the next instruction
                self.registers
                    .set(Register::ProgramCounter, pc + Self::BYTES_PER_WORD)?;

                // Push all registers
                self.push_all_registers()?;

                // Set the new program counter and ensure that the next instruction
                // starts incrementing directly from the new value
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
                inst_jump = None;
            }
            Self::OP_RETURN | Self::OP_INTERRUPT_RETURN => {
                // Clear the currently-executing interrupt if we are returning from an interrupt
                if opcode == Self::OP_INTERRUPT_RETURN
                    && let Some(index) = self.get_executing_interrupt()?
                {
                    self.interrupt_controller.clear_interrupt(index)?;
                }
                if opcode == Self::OP_RETURN {
                    self.pop_all_registers_func_return()?;
                } else {
                    self.pop_all_registers()?;
                }
                inst_jump = None;
            }
            Self::OP_PUSH => {
                let val = self.registers.get(inst.arg0_register())?;
                self.stack_push(val)?;
            }
            Self::OP_POP => {
                self.stack_pop()?;
            }
            Self::OP_POP_REG => {
                let val = self.stack_pop()?;
                self.registers.set(inst.arg0_register(), val)?;
            }
            Self::OP_JUMP => {
                self.registers.set(
                    Register::ProgramCounter,
                    self.registers.get(inst.arg0_register())?,
                )?;
                inst_jump = None;
            }
            Self::OP_JUMP_REL => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + self.registers.get(inst.arg0_register())? as i32) as u32,
                )?;
                inst_jump = None;
            }
            Self::OP_JUMP_REL_IMM => {
                self.registers.set(
                    Register::ProgramCounter,
                    (pc as i32 + inst.imm_signed()) as u32,
                )?;
                inst_jump = None;
            }
            Self::OP_HALT => {
                inst_jump = None;
            }
            Self::OP_NOT => {
                let val = self.registers.get(inst.arg1_register())?;
                self.registers
                    .set(inst.arg0_register(), if val != 0 { 0 } else { 1 })?;
            }
            Self::OP_BOOL => {
                let val = self.registers.get(inst.arg1_register())?;
                self.registers
                    .set(inst.arg0_register(), if val != 0 { 1 } else { 0 })?;
            }
            Self::OP_TEST_ZERO | Self::OP_TEST_NOT_ZERO => {
                let val = self.registers.get(inst.arg0_register())?;
                let is_true = match opcode {
                    Self::OP_TEST_ZERO => val == 0,
                    Self::OP_TEST_NOT_ZERO => val != 0,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                inst_jump = Some(if is_true { 1 } else { 2 });
            }
            Self::OP_LOAD_IMM => {
                let dt = inst.arg0_data_type()?;
                match dt {
                    DataType::I16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_signed() as u32)?,
                    DataType::U16 => self
                        .registers
                        .set(inst.arg0_register(), inst.imm_unsigned())?,
                    _ => return Err(ProcessorError::UnsupportedDataType(inst, dt)),
                }
            }
            Self::OP_LOAD
            | Self::OP_LOAD_REL
            | Self::OP_LOAD_IMM_REL
            | Self::OP_LOAD_NEXT
            | Self::OP_LOAD_NEXT_OFFSET => {
                let dt = inst.arg0_data_type()?;
                let addr = match opcode {
                    Self::OP_LOAD => self.registers.get(inst.arg1_register())?,
                    Self::OP_LOAD_REL => {
                        self.registers.get(inst.arg1_register())?
                            + self.registers.get(Register::ProgramCounter)?
                    }
                    Self::OP_LOAD_IMM_REL => {
                        (self.registers.get(Register::ProgramCounter)? as i32 + inst.imm_signed())
                            as u32
                    }
                    Self::OP_LOAD_NEXT => {
                        inst_jump = Some(2);
                        pc + 4
                    }
                    Self::OP_LOAD_NEXT_OFFSET => {
                        inst_jump = Some(2);
                        pc + 4 + self.registers.get(Register::LoadOffset)?
                    }
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };
                let reg_target = inst.arg0_register();

                if dt.integral() {
                    if dt.signed() {
                        match dt.byte_size() {
                            1 => self
                                .registers
                                .set(reg_target, (self.memory.get(addr)? as i32) as u32)?,
                            2 => self
                                .registers
                                .set(reg_target, (self.memory.get_u16(addr)? as i32) as u32)?,
                            4 => self.registers.set(reg_target, self.memory.get_u32(addr)?)?,
                            _ => return Err(ProcessorError::UnknownInstruction(inst)),
                        }
                    } else {
                        match dt.byte_size() {
                            1 => self
                                .registers
                                .set(reg_target, self.memory.get(addr)? as u32)?,
                            2 => self
                                .registers
                                .set(reg_target, self.memory.get_u16(addr)? as u32)?,
                            4 => self.registers.set(reg_target, self.memory.get_u32(addr)?)?,
                            _ => return Err(ProcessorError::UnknownInstruction(inst)),
                        }
                    }
                } else {
                    return Err(ProcessorError::UnsupportedDataType(inst, dt));
                }
            }
            Self::OP_SAVE | Self::OP_SAVE_REL => {
                let dt = inst.arg0_data_type()?;
                let source_reg = self.registers.get(inst.arg1_register())?;

                let addr = match opcode {
                    Self::OP_SAVE => self.registers.get(inst.arg0_register())?,
                    Self::OP_SAVE_REL => {
                        (inst.imm_signed() + self.registers.get(inst.arg0_register())? as i32)
                            as u32
                    }
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                match dt.byte_size() {
                    1 => self.memory.set(addr, (source_reg & 0xFF) as u8)?,
                    2 => self.memory.set_u16(addr, (source_reg & 0xFFFF) as u16)?,
                    4 => self.memory.set_u32(addr, source_reg)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };
            }
            Self::OP_COPY => self.registers.set(
                inst.arg0_register(),
                self.registers.get(inst.arg1_register())?,
            )?,
            Self::OP_CONV => {
                self.registers.set(
                    inst.arg0_register(),
                    convert_types(
                        self.registers.get(inst.arg1_register())?,
                        inst.arg1_data_type()?,
                        inst.arg0_data_type()?,
                    ),
                )?;
            }
            Opcode {
                base: Self::OP_BASE_MATH,
                ..
            } => {
                let arith = self.operator_manager.get_arith(inst.arg0_data_type()?);

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_ADD => arith.add(val_a, val_b)?,
                    Self::OP_SUB => arith.sub(val_a, val_b)?,
                    Self::OP_MUL => arith.mul(val_a, val_b)?,
                    Self::OP_DIV => arith.div(val_a, val_b)?,
                    Self::OP_REM => arith.rem(val_a, val_b)?,
                    Self::OP_NEG => arith.neg(val_a)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set_status_flag(RegisterFlag::Carry, res.carry)?;
            }
            Opcode {
                base: Self::OP_BASE_BITS,
                ..
            } => {
                let bitwise = self.operator_manager.get_bitwise(inst.arg0_data_type()?)?;

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_BAND => bitwise.band(val_a, val_b)?,
                    Self::OP_BOR => bitwise.bor(val_a, val_b)?,
                    Self::OP_BXOR => bitwise.bxor(val_a, val_b)?,
                    Self::OP_BSHL => bitwise.bsftl(val_a, val_b)?,
                    Self::OP_BSHR => bitwise.bsftr(val_a, val_b)?,
                    Self::OP_BNOT => bitwise.bnot(val_a)?,
                    Self::OP_BSWAP => bitwise.bswap(val_a)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers.set(inst.arg0_register(), res.val)?;
                self.registers
                    .set_status_flag(RegisterFlag::Carry, res.carry)?;
            }
            Opcode {
                base: Self::OP_BASE_TEST,
                ..
            } => {
                let relative = self.operator_manager.get_relative(inst.arg0_data_type()?);

                let val_a = self.registers.get(inst.arg1_register())?;
                let val_b = self.registers.get(inst.arg2_register())?;

                let res = match opcode {
                    Self::OP_EQ => relative.eq(val_a, val_b)?,
                    Self::OP_NEQ => relative.neq(val_a, val_b)?,
                    Self::OP_GREATER => relative.gt(val_a, val_b)?,
                    Self::OP_GREATER_EQ => relative.geq(val_a, val_b)?,
                    Self::OP_LESS => relative.lt(val_a, val_b)?,
                    Self::OP_LESS_EQ => relative.leq(val_a, val_b)?,
                    _ => return Err(ProcessorError::UnknownInstruction(inst)),
                };

                self.registers
                    .set(inst.arg0_register(), if res { 1 } else { 0 })?;
            }
            _ => return Err(ProcessorError::UnknownInstruction(inst)),
        };

        // Step the program counter
        if let Some(jmp_val) = inst_jump {
            self.registers
                .set(Register::ProgramCounter, pc + jmp_val * 4)?;
        }

        // Check for any actions
        for dev in self.devices.clone() {
            if let Some(action) = dev.borrow_mut().on_step() {
                match action {
                    DeviceAction::CallInterrupt(num) => {
                        self.queue_interrupt(Interrupt::Hardware(num))?;
                    }
                }
            }
        }

        Ok(())
    }

    fn stack_push(&mut self, val: u32) -> Result<(), ProcessorError> {
        let sp_curr = self.registers.get(Register::StackPointer)?;
        self.memory.set_u32(sp_curr, val)?;
        assert_eq!(self.memory.inspect_u32(sp_curr)?, val);
        self.registers
            .set(Register::StackPointer, sp_curr + Self::BYTES_PER_WORD)?;
        Ok(())
    }

    fn stack_pop(&mut self) -> Result<u32, ProcessorError> {
        let mut sp_curr = self.registers.get(Register::StackPointer)?;

        if sp_curr < Self::BYTES_PER_WORD {
            return Err(ProcessorError::StackUnderflow);
        }

        sp_curr -= Self::BYTES_PER_WORD;

        self.registers.set(Register::StackPointer, sp_curr)?;
        Ok(self.memory.get_u32(sp_curr)?)
    }
}

#[cfg(test)]
mod test {
    use crate::cpu::InterruptController;

    #[test]
    fn interrupt_manager_min_bit() {
        let mut manager = InterruptController { interrupts: 0 };
        assert_eq!(manager.has_interrupt(), None);

        manager.interrupts = 0b111;
        assert_eq!(manager.has_interrupt(), Some(0));

        manager.interrupts = 0b110;
        assert_eq!(manager.has_interrupt(), Some(1));

        manager.interrupts = 0b100;
        assert_eq!(manager.has_interrupt(), Some(2));

        manager.interrupts = 0b101;
        assert_eq!(manager.has_interrupt(), Some(0));
    }
}
