use core::fmt;
use jib::cpu::{DataType, Register, RegisterManager, UnknownDataTypeArg};

use crate::immediate::ImmediateError;

#[derive(Debug, Clone)]
pub enum ArgumentError {
    Immediate(ImmediateError),
    UnknownRegister(String),
    UnknownRegisterIndex(usize),
    UnknownDataType(String),
    DataType(UnknownDataTypeArg),
    ExpectedTypeInformation(String),
}

impl fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::UnknownRegister(r) => write!(f, "Unknown Register {r}"),
            Self::UnknownRegisterIndex(i) => write!(f, "Unknown Register Index {i}"),
            Self::UnknownDataType(d) => write!(f, "Unknown Data Type {d}"),
            Self::DataType(e) => write!(f, "Data Type => {e}"),
            Self::ExpectedTypeInformation(r) => {
                write!(f, "Expected Argument Type Information in '{r}'")
            }
        }
    }
}

impl From<ImmediateError> for ArgumentError {
    fn from(value: ImmediateError) -> Self {
        ArgumentError::Immediate(value)
    }
}

impl From<UnknownDataTypeArg> for ArgumentError {
    fn from(value: UnknownDataTypeArg) -> Self {
        ArgumentError::DataType(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgumentRegister {
    reg: Register,
}

impl ArgumentRegister {
    const REGISTER_MASK: u8 = 0x1F;

    pub fn new(reg: Register) -> Self {
        Self { reg }
    }

    pub fn to_byte(self) -> u8 {
        (self.reg.get_index() as u8) & Self::REGISTER_MASK
    }
}

impl fmt::Display for ArgumentRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.reg)
    }
}

impl From<Register> for ArgumentRegister {
    fn from(value: Register) -> Self {
        Self { reg: value }
    }
}

impl TryFrom<&str> for ArgumentRegister {
    type Error = ArgumentError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        for i in 0..RegisterManager::REGISTER_COUNT {
            let reg = Register::GeneralPurpose(i);
            if let Some(reg) = reg.as_special() {
                if format!("${}", reg.get_special_name()) == value {
                    return Ok(Self { reg });
                }
            }
        }

        let reg = if let Ok(val) = value.parse() {
            if val == (val & Self::REGISTER_MASK as usize) {
                Register::GeneralPurpose(val)
            } else {
                return Err(ArgumentError::UnknownRegisterIndex(val));
            }
        } else {
            return Err(ArgumentError::UnknownRegister(value.to_string()));
        };

        Ok(Self { reg })
    }
}

impl TryFrom<u8> for ArgumentRegister {
    type Error = ArgumentError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let reg = value & Self::REGISTER_MASK;

        Ok(Self {
            reg: Register::GeneralPurpose(reg as usize),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ArgumentType {
    reg: ArgumentRegister,
    data_type: DataType,
}

impl ArgumentType {
    const DT_OFFSET: i32 = 5;
    const DT_MASK: u8 = 0xE0;

    pub fn new(reg: Register, data_type: DataType) -> Self {
        Self {
            reg: reg.into(),
            data_type,
        }
    }

    pub fn to_byte(self) -> u8 {
        ((self.data_type.get_id() << Self::DT_OFFSET) & Self::DT_MASK) | self.reg.to_byte()
    }
}

impl fmt::Display for ArgumentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.reg, self.data_type)
    }
}

impl TryFrom<&str> for ArgumentType {
    type Error = ArgumentError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some((s_reg, s_dt)) = value.split_once(':') {
            let reg = ArgumentRegister::try_from(s_reg)?;

            let dt = match s_dt {
                "u8" => DataType::U8,
                "u16" => DataType::U16,
                "u32" => DataType::U32,
                "i8" => DataType::I8,
                "i16" => DataType::I16,
                "i32" => DataType::I32,
                "f32" => DataType::F32,
                _ => return Err(ArgumentError::UnknownDataType(s_dt.to_string())),
            };

            Ok(Self { reg, data_type: dt })
        } else {
            Err(ArgumentError::ExpectedTypeInformation(value.to_string()))
        }
    }
}

impl TryFrom<u8> for ArgumentType {
    type Error = ArgumentError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let dt_val = (value & Self::DT_MASK) >> Self::DT_OFFSET;
        let dt = DataType::try_from(dt_val)?;

        Ok(Self {
            reg: ArgumentRegister::try_from(value)?,
            data_type: dt,
        })
    }
}
