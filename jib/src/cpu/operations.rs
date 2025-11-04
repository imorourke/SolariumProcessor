use core::fmt;

use super::{DataType, ProcessorError};

pub struct OperationValue {
    pub val: u32,
    pub carry: bool,
}

impl From<(u32, bool)> for OperationValue {
    fn from(value: (u32, bool)) -> Self {
        Self {
            val: value.0,
            carry: value.1,
        }
    }
}

impl From<f32> for OperationValue {
    fn from(value: f32) -> Self {
        Self {
            val: value.to_bits(),
            carry: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationError {
    DivideByZero,
    UnuspportedOperation,
}

impl fmt::Display for OperationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DivideByZero => write!(f, "Divide By Zero"),
            Self::UnuspportedOperation => write!(f, "Unsupported Operation"),
        }
    }
}

pub trait ArithmeticOperations {
    fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn neg(&self, a: u32) -> Result<OperationValue, OperationError>;
}

pub trait RelationalOperations {
    fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
    fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError>;
}

pub trait BinaryOperations {
    fn band(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bxor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bsftr(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bsftl(&self, a: u32, b: u32) -> Result<OperationValue, OperationError>;
    fn bnot(&self, a: u32) -> Result<OperationValue, OperationError>;
    fn bswap(&self, a: u32) -> Result<OperationValue, OperationError>;
}

#[derive(Default)]
pub struct OperatorManager {
    op_f32: FloatOperations,
    op_u8: IntegerU8Operations,
    op_u16: IntegerU16Operations,
    op_u32: IntegerU32Operations,
    op_i8: IntegerI8Operations,
    op_i16: IntegerI16Operations,
    op_i32: IntegerI32Operations,
}

pub fn convert_types(src_v: u32, src_t: DataType, dst_t: DataType) -> u32 {
    match dst_t {
        DataType::U8 => {
            (match src_t {
                DataType::U8 | DataType::U16 | DataType::U32 => src_v as u8,
                DataType::I8 | DataType::I16 | DataType::I32 => (src_v as i32) as u8,
                DataType::F32 => (f32::from_bits(src_v) as i32) as u8,
            }) as u32
        }
        DataType::U16 => {
            (match src_t {
                DataType::U8 | DataType::U16 | DataType::U32 => src_v as u16,
                DataType::I8 | DataType::I16 | DataType::I32 => (src_v as i32) as u16,
                DataType::F32 => (f32::from_bits(src_v) as i32) as u16,
            }) as u32
        }
        DataType::U32 => match src_t {
            DataType::U8
            | DataType::U16
            | DataType::U32
            | DataType::I8
            | DataType::I16
            | DataType::I32 => src_v,
            DataType::F32 => (f32::from_bits(src_v) as i32) as u32,
        },
        DataType::I8 => {
            ((match src_t {
                DataType::U8 => (src_v as u8) as i8,
                DataType::U16 => (src_v as u16) as i8,
                DataType::U32 => src_v as i8,
                DataType::I8 => src_v as i8,
                DataType::I16 => (src_v as i16) as i8,
                DataType::I32 => (src_v as i32) as i8,
                DataType::F32 => (f32::from_bits(src_v) as i32) as i8,
            }) as i32) as u32
        }
        DataType::I16 => {
            ((match src_t {
                DataType::U8 => (src_v as u8) as i16,
                DataType::U16 => (src_v as u16) as i16,
                DataType::U32 => src_v as i16,
                DataType::I8 => (src_v as i8) as i16,
                DataType::I16 => src_v as i16,
                DataType::I32 => (src_v as i32) as i16,
                DataType::F32 => (f32::from_bits(src_v) as i32) as i16,
            }) as i32) as u32
        }
        DataType::I32 => {
            (match src_t {
                DataType::U8 => (src_v as u8) as i32,
                DataType::U16 => (src_v as u16) as i32,
                DataType::U32 => src_v as i32,
                DataType::I8 => (src_v as i8) as i32,
                DataType::I16 => (src_v as i16) as i32,
                DataType::I32 => src_v as i32,
                DataType::F32 => f32::from_bits(src_v) as i32,
            }) as u32
        }
        DataType::F32 => (match src_t {
            DataType::U8 => (src_v as u8) as f32,
            DataType::U16 => (src_v as u16) as f32,
            DataType::U32 => src_v as f32,
            DataType::I8 => (src_v as i8) as f32,
            DataType::I16 => (src_v as i16) as f32,
            DataType::I32 => (src_v as i32) as f32,
            DataType::F32 => f32::from_bits(src_v),
        })
        .to_bits(),
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OperatorInvalidForType;

impl From<OperatorInvalidForType> for ProcessorError {
    fn from(_value: OperatorInvalidForType) -> Self {
        Self::Operation(OperationError::UnuspportedOperation)
    }
}

impl From<OperatorInvalidForType> for OperationError {
    fn from(_value: OperatorInvalidForType) -> Self {
        Self::UnuspportedOperation
    }
}

impl OperatorManager {
    pub fn get_arith(&self, dt: DataType) -> &dyn ArithmeticOperations {
        match dt {
            DataType::U8 => &self.op_u8,
            DataType::U16 => &self.op_u16,
            DataType::U32 => &self.op_u32,
            DataType::I8 => &self.op_i8,
            DataType::I16 => &self.op_i16,
            DataType::I32 => &self.op_i32,
            DataType::F32 => &self.op_f32,
        }
    }

    pub fn get_bitwise(
        &self,
        dt: DataType,
    ) -> Result<&dyn BinaryOperations, OperatorInvalidForType> {
        Ok(match dt {
            DataType::U8 => &self.op_u8,
            DataType::U16 => &self.op_u16,
            DataType::U32 => &self.op_u32,
            DataType::I8 => &self.op_i8,
            DataType::I16 => &self.op_i16,
            DataType::I32 => &self.op_i32,
            _ => {
                return Err(OperatorInvalidForType);
            }
        })
    }

    pub fn get_relative(&self, dt: DataType) -> &dyn RelationalOperations {
        match dt {
            DataType::U8 => &self.op_u8,
            DataType::U16 => &self.op_u16,
            DataType::U32 => &self.op_u32,
            DataType::I8 => &self.op_i8,
            DataType::I16 => &self.op_i16,
            DataType::I32 => &self.op_i32,
            DataType::F32 => &self.op_f32,
        }
    }
}

macro_rules! define_arith_for_type {
    ($sname:ident, $tname:ident) => {
        impl ArithmeticOperations for $sname {
            fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_add(b as $tname);
                Ok((((res.0 as i32) as u32), res.1).into())
            }

            fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_sub(b as $tname);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_mul(b as $tname);
                Ok((((res.0 as i32) as u32), res.1).into())
            }

            fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                if b == 0 {
                    return Err(OperationError::DivideByZero);
                }
                let res = (a as $tname) / (b as $tname);
                Ok((((res as i32) as u32), false).into())
            }

            fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                if b == 0 {
                    return Err(OperationError::DivideByZero);
                }
                let res = (a as $tname) % (b as $tname);
                Ok((((res as i32) as u32), false).into())
            }

            fn neg(&self, a: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_neg();
                Ok((((res.0 as i32) as u32), res.1).into())
            }
        }
    };
}

macro_rules! define_bitwise_for_type {
    ($sname:ident, $tname:ident) => {
        impl BinaryOperations for $sname {
            fn band(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) & (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) | (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bxor(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname) ^ (b as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bsftr(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_shr(b);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn bsftl(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
                let res = (a as $tname).overflowing_shl(b);
                Ok(((res.0 as i32) as u32, res.1).into())
            }

            fn bnot(&self, a: u32) -> Result<OperationValue, OperationError> {
                let res = !(a as $tname);
                Ok(((res as i32) as u32, false).into())
            }

            fn bswap(&self, a: u32) -> Result<OperationValue, OperationError> {
                Ok((((a as $tname).swap_bytes() as i32) as u32, false).into())
            }
        }
    };
}

macro_rules! define_rel_for_type {
    ($sname:ident, $tname:ident) => {
        impl RelationalOperations for $sname {
            fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) > (b as $tname))
            }

            fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) >= (b as $tname))
            }

            fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) < (b as $tname))
            }

            fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) <= (b as $tname))
            }

            fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) == (b as $tname))
            }

            fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
                Ok((a as $tname) != (b as $tname))
            }
        }
    };
}

#[derive(Default)]
pub struct IntegerU8Operations;
define_arith_for_type!(IntegerU8Operations, u8);
define_bitwise_for_type!(IntegerU8Operations, u8);
define_rel_for_type!(IntegerU8Operations, u8);

#[derive(Default)]
pub struct IntegerU16Operations;
define_arith_for_type!(IntegerU16Operations, u16);
define_bitwise_for_type!(IntegerU16Operations, u16);
define_rel_for_type!(IntegerU16Operations, u16);

#[derive(Default)]
pub struct IntegerU32Operations;
define_arith_for_type!(IntegerU32Operations, u32);
define_bitwise_for_type!(IntegerU32Operations, u32);
define_rel_for_type!(IntegerU32Operations, u32);

#[derive(Default)]
pub struct IntegerI8Operations;
define_arith_for_type!(IntegerI8Operations, i8);
define_bitwise_for_type!(IntegerI8Operations, i8);
define_rel_for_type!(IntegerI8Operations, i8);

#[derive(Default)]
pub struct IntegerI16Operations;
define_arith_for_type!(IntegerI16Operations, i16);
define_bitwise_for_type!(IntegerI16Operations, i16);
define_rel_for_type!(IntegerI16Operations, i16);

#[derive(Default)]
pub struct IntegerI32Operations;
define_arith_for_type!(IntegerI32Operations, i32);
define_bitwise_for_type!(IntegerI32Operations, i32);
define_rel_for_type!(IntegerI32Operations, i32);

#[derive(Default)]
pub struct FloatOperations;
impl ArithmeticOperations for FloatOperations {
    fn add(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) + f32::from_bits(b);
        Ok(r.into())
    }

    fn sub(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) - f32::from_bits(b);
        Ok(r.into())
    }

    fn mul(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let r = f32::from_bits(a) * f32::from_bits(b);
        Ok(r.into())
    }

    fn div(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(b);
        if bf == 0.0f32 {
            return Err(OperationError::DivideByZero);
        }
        let r = f32::from_bits(a) / bf;
        Ok(r.into())
    }

    fn rem(&self, a: u32, b: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(b);
        if bf == 0.0f32 {
            return Err(OperationError::DivideByZero);
        }
        let r = f32::from_bits(a) % bf;
        Ok(r.into())
    }

    fn neg(&self, a: u32) -> Result<OperationValue, OperationError> {
        let bf = f32::from_bits(a);
        Ok((-bf).into())
    }
}

impl RelationalOperations for FloatOperations {
    fn gt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) > f32::from_bits(b))
    }

    fn geq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) >= f32::from_bits(b))
    }

    fn lt(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) < f32::from_bits(b))
    }

    fn leq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) <= f32::from_bits(b))
    }

    fn eq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) == f32::from_bits(b))
    }

    fn neq(&self, a: u32, b: u32) -> Result<bool, OperationError> {
        Ok(f32::from_bits(a) != f32::from_bits(b))
    }
}
