use core::fmt;

use super::register::Register;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DataType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    F32,
}

impl DataType {
    pub const ALL: &[Self] = &[
        Self::U8,
        Self::I8,
        Self::U16,
        Self::I16,
        Self::U32,
        Self::I32,
        Self::F32,
    ];

    pub fn coerced(a: Self, b: Self) -> Self {
        if a > b {
            a
        } else {
            b
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 | Self::F32 => 4,
        }
    }

    pub fn signed(&self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::F32)
    }

    pub fn integral(&self) -> bool {
        matches!(
            self,
            Self::U8 | Self::I8 | Self::U16 | Self::I16 | Self::U32 | Self::I32
        )
    }

    pub fn get_id(&self) -> u8 {
        match self {
            Self::U8 => 1,
            Self::I8 => 2,
            Self::U16 => 3,
            Self::I16 => 4,
            Self::U32 => 5,
            Self::I32 => 6,
            Self::F32 => 7,
        }
    }
}

impl fmt::Display for DataType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::U8 => "u8",
            Self::I8 => "i8",
            Self::U16 => "u16",
            Self::I16 => "i16",
            Self::U32 => "u32",
            Self::I32 => "i32",
            Self::F32 => "f32",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct UnknownDataType;

impl TryFrom<&str> for DataType {
    type Error = UnknownDataType;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "u8" => Self::U8,
            "i8" => Self::I8,
            "u16" => Self::U16,
            "i16" => Self::I16,
            "u32" => Self::U32,
            "i32" => Self::I32,
            "f32" => Self::F32,
            _ => return Err(UnknownDataType),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnknownDataTypeArg(u8);

impl fmt::Display for UnknownDataTypeArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Data Type Error {}", self.0)
    }
}

impl TryFrom<u8> for DataType {
    type Error = UnknownDataTypeArg;

    fn try_from(val: u8) -> Result<Self, Self::Error> {
        Ok(match val {
            1 => Self::U8,
            2 => Self::I8,
            3 => Self::U16,
            4 => Self::I16,
            5 => Self::U32,
            6 => Self::I32,
            7 => Self::F32,
            _ => return Err(UnknownDataTypeArg(val)),
        })
    }
}

// Bit Formatting:
// | 31 30 29 28 27 26 25 24 | 23 22 21 | 20 19 18 17 16 | 15 14 13 | 12 11 10  9  8 |  7  6  5 |  4  3  2  1  0 |
// | Opcode                  | Arg 0                     | Arg 1                     | Arg 2                     |
// | Opcode                  | DType 0  | Arg 0 (Reg)    | DType 1  | Arg 1 (Reg)    | DType 2  | Arg 2 (Reg)    |
// | Opcode                  | ImmTodo  | Immediate Value                                                        |
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    data: [u8; Self::NUM_BYTES],
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[{:02x}, {:02x}, {:02x}, {:02x}]",
            self.data[0], self.data[1], self.data[2], self.data[3]
        )
    }
}

impl Instruction {
    pub const NUM_BYTES: usize = 4;

    const NUM_IMM_BITS: u32 = { u16::BITS };
    const IMM_SIGN_BIT: u32 = { 1 << (Self::NUM_IMM_BITS - 1) };

    const IMM_NEG_MASK: u32 = {
        let mut val = 0;
        let mut i = Self::NUM_IMM_BITS;
        while i < u32::BITS {
            val |= 1 << i;
            i += 1;
        }
        val
    };

    pub fn new(data: [u8; Self::NUM_BYTES]) -> Self {
        Self { data }
    }

    pub fn opcode(&self) -> u8 {
        self.data[0]
    }

    fn reg_from_arg(arg: u8) -> Register {
        Register::GeneralPurpose((arg & 0x1F) as usize)
    }

    fn dt_from_arg(arg: u8) -> Result<DataType, UnknownDataTypeArg> {
        let dt = (arg >> 5) & 7;
        DataType::try_from(dt)
    }

    pub fn arg0(&self) -> u8 {
        self.data[1]
    }

    pub fn arg0_data_type(&self) -> Result<DataType, UnknownDataTypeArg> {
        Self::dt_from_arg(self.arg0())
    }

    pub fn arg0_register(&self) -> Register {
        Self::reg_from_arg(self.arg0())
    }

    pub fn arg1(&self) -> u8 {
        self.data[2]
    }

    pub fn arg1_data_type(&self) -> Result<DataType, UnknownDataTypeArg> {
        Self::dt_from_arg(self.arg1())
    }

    pub fn arg1_register(&self) -> Register {
        Self::reg_from_arg(self.arg1())
    }

    pub fn arg2(&self) -> u8 {
        self.data[3]
    }

    pub fn arg2_data_type(&self) -> Result<DataType, UnknownDataTypeArg> {
        Self::dt_from_arg(self.arg2())
    }

    pub fn arg2_register(&self) -> Register {
        Self::reg_from_arg(self.arg2())
    }

    pub fn imm_unsigned(&self) -> u32 {
        ((self.arg1() as u32) << 8) | (self.arg2() as u32)
    }

    pub fn imm_signed(&self) -> i32 {
        let mut val = self.imm_unsigned();
        if (val & Self::IMM_SIGN_BIT) != 0 {
            val |= Self::IMM_NEG_MASK;
        }
        val as i32
    }

    pub fn get_data(&self) -> [u8; Self::NUM_BYTES] {
        self.data
    }
}

impl From<u32> for Instruction {
    fn from(value: u32) -> Self {
        Self::new(value.to_be_bytes())
    }
}

impl From<Instruction> for u32 {
    fn from(value: Instruction) -> Self {
        u32::from_be_bytes(value.data)
    }
}
