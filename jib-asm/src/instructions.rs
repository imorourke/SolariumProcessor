use core::fmt;
use std::fmt::Debug;

use crate::{
    argument::{ArgumentError, ArgumentRegister, ArgumentType},
    immediate::{ImmediateError, parse_imm_i16},
};
use jib::cpu::{Opcode, Processor};

pub const INST_SIZE: usize = 4;

#[derive(Debug, Clone)]
pub enum InstructionError {
    CountMismatch(usize, usize),
    Immediate(ImmediateError),
    Argument(ArgumentError),
    OpcodeMismatch(Opcode, u8),
}

impl fmt::Display for InstructionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CountMismatch(num, expected) => write!(f, "Found {num}, Expected {expected}"),
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::Argument(a) => write!(f, "Argument Error => {a}"),
            Self::OpcodeMismatch(op, b) => {
                write!(f, "Opcode Mismatch - {op} does not match provided {b}")
            }
        }
    }
}

impl From<ImmediateError> for InstructionError {
    fn from(value: ImmediateError) -> Self {
        Self::Immediate(value)
    }
}

impl From<ArgumentError> for InstructionError {
    fn from(value: ArgumentError) -> Self {
        Self::Argument(value)
    }
}

pub trait Instruction: Debug {
    fn get_name(&self) -> String;

    fn get_args(&self) -> Vec<String>;

    fn to_bytes(&self) -> [u8; INST_SIZE];

    fn to_u32(&self) -> u32 {
        u32::from_be_bytes(self.to_bytes())
    }

    fn boxed_clone(&self) -> Box<dyn Instruction>;
}

macro_rules! InstNoArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name;

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 0;

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }

            fn get_args(&self) -> Vec<String> {
                Vec::default()
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), 0, 0, 0]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", Self::name())
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self)
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self)
                }
            }
        }
    };
}

macro_rules! InstSingleArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentRegister,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(arg: ArgumentRegister) -> Self {
                Self { arg }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), self.arg.to_byte(), 0, 0]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {}", Self::name(), self.arg)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self::new(ArgumentRegister::try_from(args[0].as_ref())?))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg: ArgumentRegister::try_from(bytes[1])?,
                    })
                }
            }
        }
    };
}

macro_rules! InstSingleArgDataType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentType,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(arg: ArgumentType) -> Self {
                Self { arg }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [Self::OP.to_byte(), self.arg.to_byte(), 0, 0]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {}", Self::name(), self.arg)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    Ok(Self::new(ArgumentType::try_from(args[0].as_ref())?))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg: ArgumentType::try_from(bytes[1])?,
                    })
                }
            }
        }
    };
}

macro_rules! InstImmediateArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            imm: u16,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(imm: u16) -> Self {
                Self { imm }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.imm)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                let imm = self.imm.to_be_bytes();
                [Self::OP.to_byte(), 0, imm[0], imm[1]]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {}", Self::name(), self.imm as i16)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let imm = parse_imm_i16(&args[0])? as u16;
                    Ok(Self::new(imm))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        imm: u16::from_be_bytes([bytes[2], bytes[3]]),
                    })
                }
            }
        }
    };
}

macro_rules! InstImmediateArgSigned {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            imm: i16,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 1;

            pub fn new(imm: i16) -> Self {
                Self { imm }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.imm)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                let imm = self.imm.to_be_bytes();
                [Self::OP.to_byte(), 0, imm[0], imm[1]]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {}", Self::name(), self.imm as i16)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let imm = parse_imm_i16(&args[0])?;
                    Ok(Self::new(imm))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        imm: i16::from_be_bytes([bytes[2], bytes[3]]),
                    })
                }
            }
        }
    };
}

macro_rules! InstSingleArgImm {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg: ArgumentType,
            imm: u16,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg: ArgumentType, imm: u16) -> Self {
                Self { arg, imm }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg), format!("{}", self.imm)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                let imm = self.imm.to_be_bytes();
                [Self::OP.to_byte(), self.arg.to_byte(), imm[0], imm[1]]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {} 0x{:04x}", Self::name(), self.arg, self.imm)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let imm = parse_imm_i16(&args[1])? as u16;
                    Ok(Self::new(a0, imm))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg: ArgumentType::try_from(bytes[1])?,
                        imm: u16::from_be_bytes([bytes[2], bytes[3]]),
                    })
                }
            }
        }
    };
}

macro_rules! InstDoubleArg {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentRegister,
            arg1: ArgumentRegister,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentRegister, arg1: ArgumentRegister) -> Self {
                Self { arg0, arg1 }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg0), format!("{}", self.arg1)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {} {}", Self::name(), self.arg0, self.arg1)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentRegister::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg0: ArgumentRegister::try_from(bytes[1])?,
                        arg1: ArgumentRegister::try_from(bytes[2])?,
                    })
                }
            }
        }
    };
}

macro_rules! InstDoubleArgType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentRegister,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentType, arg1: ArgumentRegister) -> Self {
                Self { arg0, arg1 }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg0), format!("{}", self.arg1)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {} {}", Self::name(), self.arg0, self.arg1)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg0: ArgumentType::try_from(bytes[1])?,
                        arg1: ArgumentRegister::try_from(bytes[2])?,
                    })
                }
            }
        }
    };
}

macro_rules! InstDoubleArgDoubleType {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentType,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 2;

            pub fn new(arg0: ArgumentType, arg1: ArgumentType) -> Self {
                Self { arg0, arg1 }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![format!("{}", self.arg0), format!("{}", self.arg1)]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    0,
                ]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{} {} {}", Self::name(), self.arg0, self.arg1)
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentType::try_from(args[1].as_ref())?;
                    Ok(Self::new(a0, a1))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg0: ArgumentType::try_from(bytes[1])?,
                        arg1: ArgumentType::try_from(bytes[2])?,
                    })
                }
            }
        }
    };
}

macro_rules! InstArith {
    ($op_name:ident, $opcode:expr) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub struct $op_name {
            arg0: ArgumentType,
            arg1: ArgumentRegister,
            arg2: ArgumentRegister,
        }

        impl $op_name {
            pub const OP: Opcode = $opcode;
            const NUM_ARGS: usize = 3;

            pub fn new(arg0: ArgumentType, arg1: ArgumentRegister, arg2: ArgumentRegister) -> Self {
                Self { arg0, arg1, arg2 }
            }

            pub fn name() -> String {
                stringify!($op_name)
                    .to_lowercase()
                    .strip_prefix("op")
                    .unwrap()
                    .into()
            }
        }

        impl Instruction for $op_name {
            fn get_name(&self) -> String {
                Self::name()
            }

            fn get_args(&self) -> Vec<String> {
                vec![
                    format!("{}", self.arg0),
                    format!("{}", self.arg1),
                    format!("{}", self.arg2),
                ]
            }

            fn to_bytes(&self) -> [u8; INST_SIZE] {
                [
                    Self::OP.to_byte(),
                    self.arg0.to_byte(),
                    self.arg1.to_byte(),
                    self.arg2.to_byte(),
                ]
            }

            fn boxed_clone(&self) -> Box<dyn Instruction> {
                Box::new(self.clone())
            }
        }

        impl fmt::Display for $op_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    "{} {} {} {}",
                    Self::name(),
                    self.arg0,
                    self.arg1,
                    self.arg2
                )
            }
        }

        impl TryFrom<Vec<String>> for $op_name {
            type Error = InstructionError;

            fn try_from(args: Vec<String>) -> Result<Self, Self::Error> {
                if args.len() != Self::NUM_ARGS {
                    Err(InstructionError::CountMismatch(args.len(), Self::NUM_ARGS))
                } else {
                    let a0 = ArgumentType::try_from(args[0].as_ref())?;
                    let a1 = ArgumentRegister::try_from(args[1].as_ref())?;
                    let a2 = ArgumentRegister::try_from(args[2].as_ref())?;
                    Ok(Self::new(a0, a1, a2))
                }
            }
        }

        impl TryFrom<[u8; 4]> for $op_name {
            type Error = InstructionError;

            fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
                if bytes[0] != Self::OP.to_byte() {
                    Err(InstructionError::OpcodeMismatch(Self::OP, bytes[0]))
                } else {
                    Ok(Self {
                        arg0: ArgumentType::try_from(bytes[1])?,
                        arg1: ArgumentRegister::try_from(bytes[2])?,
                        arg2: ArgumentRegister::try_from(bytes[3])?,
                    })
                }
            }
        }
    };
}

InstNoArg!(OpNoop, Processor::OP_NOOP);
InstNoArg!(OpReset, Processor::OP_RESET);
InstNoArg!(OpRetInt, Processor::OP_INTERRUPT_RETURN);
InstNoArg!(OpRet, Processor::OP_RETURN);
InstNoArg!(OpHalt, Processor::OP_HALT);
InstNoArg!(OpInton, Processor::OP_INTERRUPT_ENABLE);
InstNoArg!(OpIntoff, Processor::OP_INTERRUPT_DISABLE);
InstNoArg!(OpBrk, Processor::OP_DEBUG_BREAK);

InstImmediateArg!(OpInt, Processor::OP_INTERRUPT);
InstSingleArg!(OpIntr, Processor::OP_INTERRUPT_REGISTER);
InstSingleArg!(OpCall, Processor::OP_CALL);
InstSingleArg!(OpPush, Processor::OP_PUSH);
InstNoArg!(OpPop, Processor::OP_POP);
InstSingleArg!(OpPopr, Processor::OP_POP_REG);
InstSingleArg!(OpJmp, Processor::OP_JUMP);
InstSingleArg!(OpJmpr, Processor::OP_JUMP_REL);
InstSingleArgDataType!(OpLdn, Processor::OP_LOAD_NEXT);
InstSingleArgDataType!(OpLdno, Processor::OP_LOAD_NEXT);

InstSingleArg!(OpTz, Processor::OP_TEST_ZERO);
InstSingleArg!(OpTnz, Processor::OP_TEST_NOT_ZERO);

InstImmediateArgSigned!(OpJmpri, Processor::OP_JUMP_REL_IMM);
InstSingleArgImm!(OpLdi, Processor::OP_LOAD_IMM);
InstSingleArgImm!(OpLdri, Processor::OP_LOAD_IMM_REL);

InstDoubleArg!(OpNot, Processor::OP_NOT);
InstDoubleArg!(OpBool, Processor::OP_BOOL);
InstDoubleArg!(OpCopy, Processor::OP_COPY);

InstDoubleArgType!(OpSav, Processor::OP_SAVE);
InstDoubleArgType!(OpSavr, Processor::OP_SAVE_REL);
InstDoubleArgType!(OpLd, Processor::OP_LOAD);
InstDoubleArgType!(OpLdr, Processor::OP_LOAD_REL);

InstDoubleArgDoubleType!(OpConv, Processor::OP_CONV);

InstArith!(OpAdd, Processor::OP_ADD);
InstArith!(OpSub, Processor::OP_SUB);
InstArith!(OpMul, Processor::OP_MUL);
InstArith!(OpDiv, Processor::OP_DIV);
InstArith!(OpRem, Processor::OP_REM);
InstDoubleArgType!(OpNeg, Processor::OP_NEG);
InstArith!(OpBand, Processor::OP_BAND);
InstArith!(OpBor, Processor::OP_BOR);
InstArith!(OpBxor, Processor::OP_BXOR);
InstArith!(OpBshl, Processor::OP_BSHL);
InstArith!(OpBshr, Processor::OP_BSHR);
InstDoubleArgType!(OpBnot, Processor::OP_BNOT);

InstArith!(OpTeq, Processor::OP_EQ);
InstArith!(OpTneq, Processor::OP_NEQ);
InstArith!(OpTg, Processor::OP_GREATER);
InstArith!(OpTge, Processor::OP_GREATER_EQ);
InstArith!(OpTl, Processor::OP_LESS);
InstArith!(OpTle, Processor::OP_LESS_EQ);
