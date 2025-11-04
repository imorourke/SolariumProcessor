use std::{
    fmt::Display,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, OperationError, OperatorManager, convert_types};
use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc};
use regex::{Regex, RegexBuilder};

use crate::{
    TokenError,
    compiler::{CodeGenerationOptions, GlobalStatement},
    expressions::{
        BinaryArithmeticOperation, Expression, ExpressionData, RegisterDef, TemporaryStackTracker,
        UnaryOperation,
    },
    tokenizer::Token,
    typing::Type,
};

static OPERATIONS: LazyLock<OperatorManager> = LazyLock::new(OperatorManager::default);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LiteralValue {
    U8(u8),
    U16(u16),
    U32(u32),
    I8(i8),
    I16(i16),
    I32(i32),
    F32(f32),
}

impl LiteralValue {
    pub fn get_dtype(&self) -> DataType {
        match &self {
            Self::U8(_) => DataType::U8,
            Self::U16(_) => DataType::U16,
            Self::U32(_) => DataType::U32,
            Self::I8(_) => DataType::I8,
            Self::I16(_) => DataType::I16,
            Self::I32(_) => DataType::I32,
            Self::F32(_) => DataType::F32,
        }
    }

    pub fn as_u32(&self) -> u32 {
        match *self {
            Self::U8(x) => x as u32,
            Self::U16(x) => x as u32,
            Self::U32(x) => x,
            Self::I8(x) => (x as i32) as u32,
            Self::I16(x) => (x as i32) as u32,
            Self::I32(x) => x as u32,
            Self::F32(x) => x.to_bits(),
        }
    }

    pub fn as_size(&self) -> Option<i32> {
        match *self {
            Self::U8(x) => Some(x as i32),
            Self::U16(x) => Some(x as i32),
            Self::U32(x) => Some(x as i32),
            Self::I8(x) => Some(x as i32),
            Self::I16(x) => Some(x as i32),
            Self::I32(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_asm_literal(&self) -> AsmToken {
        match *self {
            Self::U8(x) => AsmToken::Literal1(x),
            Self::I8(x) => AsmToken::Literal1(x as u8),
            Self::U16(x) => AsmToken::Literal2(x),
            Self::I16(x) => AsmToken::Literal2(x as u16),
            Self::U32(x) => AsmToken::Literal4(x),
            Self::I32(x) => AsmToken::Literal4(x as u32),
            Self::F32(x) => AsmToken::Literal4(x.to_bits()),
        }
    }

    pub fn from_u32(x: u32, dtype: DataType) -> Self {
        match dtype {
            DataType::U8 => Self::U8(x as u8),
            DataType::U16 => Self::U16(x as u16),
            DataType::U32 => Self::U32(x),
            DataType::I8 => Self::I8((x as i32) as i8),
            DataType::I16 => Self::I16((x as i32) as i16),
            DataType::I32 => Self::I32(x as i32),
            DataType::F32 => Self::F32(f32::from_bits(x)),
        }
    }

    pub fn unary(a: Self, op: UnaryOperation) -> Result<Self, OperationError> {
        let x = a.as_u32();
        let dt = a.get_dtype();

        let res = match op {
            UnaryOperation::BitNot => OPERATIONS.get_bitwise(dt)?.bnot(x)?.val,
            UnaryOperation::Plus => x,
            UnaryOperation::Minus => OPERATIONS.get_arith(dt).neg(x)?.val,
            UnaryOperation::Not => {
                if x == 0 {
                    1
                } else {
                    0
                }
            }
        };

        Ok(Self::from_u32(res, dt))
    }

    pub fn operation(
        lhs: Self,
        rhs: Self,
        op: BinaryArithmeticOperation,
    ) -> Result<Self, OperationError> {
        let dt: DataType = if lhs.get_dtype() > rhs.get_dtype() {
            lhs.get_dtype()
        } else {
            rhs.get_dtype()
        };

        let a = lhs.as_u32();
        let b = rhs.as_u32();

        fn u32_to_bool(x: u32) -> u32 {
            if x != 0 { 1 } else { 0 }
        }

        let res = match op {
            BinaryArithmeticOperation::Plus => OPERATIONS.get_arith(dt).add(a, b)?.val,
            BinaryArithmeticOperation::Minus => OPERATIONS.get_arith(dt).sub(a, b)?.val,
            BinaryArithmeticOperation::Product => OPERATIONS.get_arith(dt).mul(a, b)?.val,
            BinaryArithmeticOperation::Divide => OPERATIONS.get_arith(dt).div(a, b)?.val,
            BinaryArithmeticOperation::Mod => OPERATIONS.get_arith(dt).rem(a, b)?.val,
            BinaryArithmeticOperation::Greater => OPERATIONS.get_relative(dt).gt(a, b)? as u32,
            BinaryArithmeticOperation::GreaterEqual => {
                OPERATIONS.get_relative(dt).geq(a, b)? as u32
            }
            BinaryArithmeticOperation::Less => OPERATIONS.get_relative(dt).lt(a, b)? as u32,
            BinaryArithmeticOperation::LessEqual => OPERATIONS.get_relative(dt).leq(a, b)? as u32,
            BinaryArithmeticOperation::Equals => OPERATIONS.get_relative(dt).eq(a, b)? as u32,
            BinaryArithmeticOperation::NotEquals => OPERATIONS.get_relative(dt).neq(a, b)? as u32,
            BinaryArithmeticOperation::And => {
                u32_to_bool(OPERATIONS.get_bitwise(dt)?.band(a, b)?.val)
            }
            BinaryArithmeticOperation::Or => {
                u32_to_bool(OPERATIONS.get_bitwise(dt)?.bor(a, b)?.val)
            }
            BinaryArithmeticOperation::BitAnd => OPERATIONS.get_bitwise(dt)?.band(a, b)?.val,
            BinaryArithmeticOperation::BitOr => OPERATIONS.get_bitwise(dt)?.bor(a, b)?.val,
            BinaryArithmeticOperation::BitXor => OPERATIONS.get_bitwise(dt)?.bxor(a, b)?.val,
        };

        Ok(Self::from_u32(res, dt))
    }

    pub fn convert(&self, dt: DataType) -> Self {
        let val = convert_types(self.as_u32(), self.get_dtype(), dt);

        match dt {
            DataType::U8 => Self::U8(val as u8),
            DataType::I8 => Self::I8((val as i32) as i8),
            DataType::U16 => Self::U16(val as u16),
            DataType::I16 => Self::I16((val as i32) as i16),
            DataType::U32 => Self::U32(val),
            DataType::I32 => Self::I32(val as i32),
            DataType::F32 => Self::F32(f32::from_bits(val)),
        }
    }

    pub fn get_asm_string(&self) -> String {
        match self {
            Self::F32(x) => x.to_string(),
            Self::U8(x) => x.to_string(),
            Self::U16(x) => x.to_string(),
            Self::U32(x) => x.to_string(),
            Self::I8(x) => x.to_string(),
            Self::I16(x) => x.to_string(),
            Self::I32(x) => x.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Literal {
    token: Token,
    dtype: Type,
    value: LiteralValue,
}

impl Literal {
    pub fn new(token: Token, dtype: Type, value: LiteralValue) -> Self {
        Self {
            token,
            dtype,
            value,
        }
    }

    pub fn new_value(token: Token, value: LiteralValue) -> Self {
        Self {
            token,
            dtype: Type::Primitive(value.get_dtype()),
            value,
        }
    }

    pub fn get_value(&self) -> &LiteralValue {
        &self.value
    }
}

impl Expression for Literal {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        //Ok(Type::Primitive(self.value.get_dtype()))
        Ok(self.dtype.clone())
    }

    fn load_value_to_register(
        &self,
        _options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let op_lit = match self.value {
            LiteralValue::U32(x) => jib_asm::AsmToken::Literal4(x),
            LiteralValue::U16(x) => jib_asm::AsmToken::Literal2(x),
            LiteralValue::U8(x) => jib_asm::AsmToken::Literal1(x),
            LiteralValue::I32(x) => jib_asm::AsmToken::Literal4(x as u32),
            LiteralValue::I16(x) => jib_asm::AsmToken::Literal2(x as u16),
            LiteralValue::I8(x) => jib_asm::AsmToken::Literal1(x as u8),
            LiteralValue::F32(x) => jib_asm::AsmToken::Literal4(x.to_bits()),
        };
        let op_load = AsmToken::OperationLiteral(Box::new(jib_asm::OpLdn::new(ArgumentType::new(
            reg.reg,
            self.value.get_dtype(),
        ))));

        Ok(ExpressionData::new(self.get_token().to_asm_iter([
            op_load,
            op_lit,
            AsmToken::AlignInstruction,
        ])))
    }

    fn simplify(&self) -> Option<Literal> {
        Some(self.clone())
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value {
            LiteralValue::F32(x) => write!(f, "{x}f32"),
            LiteralValue::U8(x) => write!(f, "{x}u8"),
            LiteralValue::U16(x) => write!(f, "{x}u16"),
            LiteralValue::U32(x) => write!(f, "{x}u32"),
            LiteralValue::I8(x) => write!(f, "{x}i8"),
            LiteralValue::I16(x) => write!(f, "{x}i16"),
            LiteralValue::I32(x) => write!(f, "{x}i32"),
        }
    }
}

impl TryFrom<Token> for Literal {
    type Error = TokenError;
    fn try_from(value: Token) -> Result<Self, Self::Error> {
        static LITERAL_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r"^(((?<inum>(0x)?[a-fA-F\d]+)(?<itype>[ui](8|(16)|(32)))?)|((?<fnum>(\d+(\.\d*))|(\.\d+))f32)|(?<f32>\d*\.\d+))$").unwrap()
        });
        static CHAR_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^'(?<num>\\?[\w,\s\!\.\*\(\)\[\]\{\}\?])'$").unwrap());

        let res = if let Some(m) = LITERAL_REGEX.captures(value.get_value()) {
            if let Some(inum) = m.name("inum") {
                let mut num = inum.as_str();

                let radix = if let Some(n) = num.strip_prefix("0x") {
                    num = n;
                    16
                } else {
                    10
                };

                if let Some(itype) = m.name("itype") {
                    let val = match itype.as_str() {
                        "u8" => u8::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U8(x))),
                        "u16" => u16::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U16(x))),
                        "u32" => u32::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::U32(x))),
                        "i8" => i8::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I8(x))),
                        "i16" => i16::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I16(x))),
                        "i32" => i32::from_str_radix(num, radix)
                            .map_or(None, |x| Some(LiteralValue::I32(x))),
                        x => {
                            return Err(value
                                .clone()
                                .into_err(format!("unknown integer type {}", x)));
                        }
                    };

                    match val {
                        Some(v) => Ok(v),
                        None => {
                            return Err(value.clone().into_err(format!(
                                "unable to convert {} as {} literal",
                                num,
                                itype.as_str()
                            )));
                        }
                    }
                } else {
                    match i32::from_str_radix(num, radix) {
                        Ok(x) => Ok(LiteralValue::I32(x)),
                        Err(_) => Err(value
                            .clone()
                            .into_err(format!("unable to get i32 value from {}", inum.as_str()))),
                    }
                }
            } else if let Some(fnum) = m.name("fnum") {
                if let Ok(fv) = fnum.as_str().parse::<f32>() {
                    Ok(LiteralValue::F32(fv))
                } else {
                    Err(value
                        .clone()
                        .into_err(format!("unable to get float value from {}", fnum.as_str())))
                }
            } else if let Some(fnum) = m.name("f32") {
                match fnum.as_str().parse() {
                    Ok(x) => Ok(LiteralValue::F32(x)),
                    Err(_) => Err(value
                        .clone()
                        .into_err(format!("unable to get float value from {}", fnum.as_str()))),
                }
            } else {
                Err(value
                    .clone()
                    .into_err("cannot parse as a literal - unknown type"))
            }
        } else if let Some(m) = CHAR_REGEX.captures(value.get_value())
            && let Some(c) = m.name("num")
        {
            let cs = c.as_str();

            let jib_char = if cs.len() == 1 {
                cs.chars().next().unwrap()
            } else {
                return Err(value
                    .clone()
                    .into_err("somehow got an unexpected character count"));
            };

            if let Ok(val) = jib::text::character_to_byte(jib_char) {
                Ok(LiteralValue::U8(val))
            } else {
                Err(value
                    .clone()
                    .into_err("unable to convert '{jib_char}' into byte"))
            }
        } else {
            Err(value.clone().into_err("cannot parse as a literal"))
        }?;

        Ok(Literal {
            token: value,
            dtype: Type::Primitive(res.get_dtype()),
            value: res,
        })
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    token: Token,
    value: Rc<str>,
    id: u64,
}

impl StringLiteral {
    pub fn get_quoted_text(s: &str) -> Option<&str> {
        static STRING_REGEX: LazyLock<Regex> = LazyLock::new(|| {
            RegexBuilder::new(r#"^"(?<text>.*)"$"#)
                .multi_line(false)
                .dot_matches_new_line(true)
                .build()
                .unwrap()
        });

        if let Some(capture) = STRING_REGEX.captures(s) {
            capture.name("text").map(|x| x.as_str())
        } else {
            None
        }
    }

    pub fn new(token: Token) -> Result<Self, TokenError> {
        if let Some(txt) = Self::get_quoted_text(token.get_value()).map(|x| x.to_string()) {
            let mut h = DefaultHasher::new();
            txt.hash(&mut h);
            Ok(Self {
                token,
                value: txt.into(),
                id: h.finish(),
            })
        } else {
            Err(token.into_err("unable to parse into a valid string literal"))
        }
    }

    fn get_label(&self) -> String {
        format!("___global_string_value_{}_{}", self.id, self.value.len())
    }
}

impl GlobalStatement for StringLiteral {
    fn get_static_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .token
            .to_asm_iter([
                AsmToken::CreateLabel(self.get_label()),
                AsmToken::LiteralText(self.value.to_string()),
                AsmToken::AlignInstruction,
            ])
            .into_iter()
            .collect())
    }

    fn get_init_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_func_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value.replace("\n", "\\n"))
    }
}

impl Expression for StringLiteral {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn load_value_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        Ok(ExpressionData::new(self.token.to_asm_iter(
            options.load_label(reg.reg, self.get_label()),
        )))
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Pointer(Box::new(Type::Primitive(DataType::U8))))
    }

    fn simplify(&self) -> Option<Literal> {
        None
    }
}
