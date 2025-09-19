use core::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Register {
    ProgramCounter,
    Status,
    StackPointer,
    Overflow,
    Return,
    ArgumentBase,
    GeneralPurpose(usize),
}

impl Register {
    pub const NUM_REGISTERS: usize = 32;

    pub const IDX_PROGRAM_COUNTER: usize = 0;
    pub const IDX_STATUS: usize = 1;
    pub const IDX_STACK_POINTER: usize = 2;
    pub const IDX_OVERFLOW: usize = 3;
    pub const IDX_RETURN: usize = 4;
    pub const IDX_ARGUMENT_BASE: usize = 5;
    pub const IDX_FIRST_GP: usize = 6;

    pub const fn first_gp_register() -> Self {
        Self::GeneralPurpose(Self::IDX_FIRST_GP)
    }

    pub const fn last_register() -> Self {
        Self::GeneralPurpose(Self::NUM_REGISTERS - 1)
    }

    pub const fn get_index(&self) -> usize {
        match self {
            Self::ProgramCounter => Self::IDX_PROGRAM_COUNTER,
            Self::Status => Self::IDX_STATUS,
            Self::StackPointer => Self::IDX_STACK_POINTER,
            Self::Overflow => Self::IDX_OVERFLOW,
            Self::Return => Self::IDX_RETURN,
            Self::ArgumentBase => Self::IDX_ARGUMENT_BASE,
            Self::GeneralPurpose(num) => *num,
        }
    }

    pub const fn as_special(&self) -> Option<Self> {
        match self.get_index() {
            Self::IDX_PROGRAM_COUNTER => Some(Self::ProgramCounter),
            Self::IDX_STATUS => Some(Self::Status),
            Self::IDX_STACK_POINTER => Some(Self::StackPointer),
            Self::IDX_OVERFLOW => Some(Self::Overflow),
            Self::IDX_RETURN => Some(Self::Return),
            Self::IDX_ARGUMENT_BASE => Some(Self::ArgumentBase),
            _ => None,
        }
    }

    pub const fn get_special_name(&self) -> &str {
        match self.as_special() {
            Some(Self::ProgramCounter) => "pc",
            Some(Self::Status) => "stat",
            Some(Self::StackPointer) => "sp",
            Some(Self::Overflow) => "ovf",
            Some(Self::Return) => "ret",
            Some(Self::ArgumentBase) => "arg",
            _ => "",
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(spr) = self.as_special() {
            write!(f, "${}", spr.get_special_name())
        } else {
            write!(f, "{}", self.get_index())
        }
    }
}

impl TryFrom<usize> for Register {
    type Error = RegisterError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        Ok(match value {
            Self::IDX_PROGRAM_COUNTER => Self::ProgramCounter,
            Self::IDX_STATUS => Self::Status,
            Self::IDX_STACK_POINTER => Self::StackPointer,
            Self::IDX_OVERFLOW => Self::Overflow,
            Self::IDX_RETURN => Self::Return,
            Self::IDX_ARGUMENT_BASE => Self::ArgumentBase,
            x if (Self::IDX_FIRST_GP..Self::NUM_REGISTERS).contains(&x) => Self::GeneralPurpose(x),
            x => return Err(Self::Error::UnknownRegister(x)),
        })
    }
}

impl Eq for Register {}
impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.get_index() == other.get_index()
    }
}

impl Ord for Register {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.get_index().cmp(&other.get_index())
    }
}

impl PartialOrd for Register {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterError {
    UnknownRegister(usize),
}

impl fmt::Display for RegisterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownRegister(r) => write!(f, "Unknown Register {r}"),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RegisterFlag {
    InterruptEnable,
    InterruptExecuting,
    InterruptValue,
    Carry,
}

impl RegisterFlag {
    const fn get_base_bit(&self) -> u32 {
        match self {
            Self::InterruptEnable => 0,
            Self::InterruptExecuting => 1,
            Self::InterruptValue => 2,
            Self::Carry => 8,
        }
    }

    const fn get_mask(&self) -> u32 {
        const fn gen_int_value_mask() -> u32 {
            let mut i = 0;
            let mut val = 0;
            while i < 6 {
                val |= 1 << i;
                i += 1;
            }
            val
        }

        const INT_VAL_MASK: u32 = gen_int_value_mask();

        match self {
            Self::InterruptValue => INT_VAL_MASK,
            _ => 1,
        }
    }

    const fn get_value(&self, reg: u32) -> u32 {
        (reg >> self.get_base_bit()) & self.get_mask()
    }

    const fn set_value(&self, reg: u32, val: u32) -> u32 {
        (reg & !(self.get_mask() << self.get_base_bit()))
            | ((val & self.get_mask()) << self.get_base_bit())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct RegisterManager {
    pub registers: [u32; Self::REGISTER_COUNT],
}

impl RegisterManager {
    pub const REGISTER_COUNT: usize = Register::NUM_REGISTERS;

    pub const fn get(&self, reg: Register) -> Result<u32, RegisterError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            Ok(self.registers[ind])
        } else {
            Err(RegisterError::UnknownRegister(ind))
        }
    }

    pub const fn set(&mut self, reg: Register, val: u32) -> Result<(), RegisterError> {
        let ind = reg.get_index();
        if ind < self.registers.len() {
            self.registers[ind] = val;
            Ok(())
        } else {
            Err(RegisterError::UnknownRegister(ind))
        }
    }

    pub const fn reset(&mut self) {
        self.registers = [0; Self::REGISTER_COUNT];
    }

    pub const fn get_state(&self) -> [u32; Self::REGISTER_COUNT] {
        self.registers
    }

    pub const fn set_state(&mut self, values: [u32; Self::REGISTER_COUNT]) {
        self.registers = values;
    }

    pub fn get_status_flag(&self, flag: RegisterFlag) -> Result<bool, RegisterError> {
        Ok(self.get_status_value(flag)? != 0)
    }

    pub fn set_status_flag(
        &mut self,
        flag: RegisterFlag,
        value: bool,
    ) -> Result<(), RegisterError> {
        self.set_status_value(flag, if value { 1 } else { 0 })
    }

    pub fn get_status_value(&self, flag: RegisterFlag) -> Result<u32, RegisterError> {
        let val = self.get(Register::Status)?;
        Ok(flag.get_value(val))
    }

    pub fn set_status_value(
        &mut self,
        flag: RegisterFlag,
        value: u32,
    ) -> Result<(), RegisterError> {
        let status = self.get(Register::Status)?;
        let new_status = flag.set_value(status, value);
        self.set(Register::Status, new_status)
    }
}

impl Default for RegisterManager {
    fn default() -> Self {
        Self {
            registers: [0; Self::REGISTER_COUNT],
        }
    }
}
