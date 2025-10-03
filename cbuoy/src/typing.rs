use std::collections::hash_map::Entry;
use std::fmt;
use std::rc::Rc;
use std::sync::LazyLock;
use std::{collections::HashMap, fmt::Display};

use jib::cpu::DataType;

use crate::compiler::{CompilingState, UserTypeOptions, UserTypeReference};
use crate::expressions::parse_expression;
use crate::tokenizer::{
    KEYWORD_CONST, KEYWORD_FN, Token, TokenError, TokenIter, get_identifier, is_identifier,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Primitive(DataType),
    Pointer(Box<Self>),
    Array(usize, Box<Self>),
    Struct(Rc<StructDefinition>),
    Function(Rc<Function>),
    Opaque(UserTypeReference),
    Const(Box<Self>),
}

impl Type {
    pub fn read_type(tokens: &mut TokenIter, state: &CompilingState) -> Result<Self, TokenError> {
        let t = tokens.next()?;
        if t.get_value() == "*" {
            Ok(Self::Pointer(Box::new(Self::read_type(tokens, state)?)))
        } else if t.get_value() == "[" {
            let expr = parse_expression(tokens, state)?;
            tokens.expect("]")?;
            if let Some(lit) = expr.simplify() {
                let s = lit.get_value().as_size().unwrap_or_default();
                if s > 0 {
                    Ok(Self::Array(
                        s as usize,
                        Box::new(Self::read_type(tokens, state)?),
                    ))
                } else {
                    Err(t.into_err("computed size is {s} and is not > 0"))
                }
            } else {
                Err(t.into_err("array must have a constant value"))
            }
        } else if t.get_value() == KEYWORD_CONST {
            Ok(Self::Const(Box::new(Type::read_type(tokens, state)?)))
        } else if t.get_value() == KEYWORD_FN {
            Ok(Self::Function(Rc::new(Function::read_tokens(
                tokens, state, false,
            )?)))
        } else if is_identifier(t.get_value()) {
            state.get_user_type(&t)?.get_type(false)
        } else if let Ok(p) = DataType::try_from(t.get_value()) {
            Ok(Self::Primitive(p))
        } else {
            Err(t.into_err("unknown type token found"))
        }
    }

    pub fn byte_size(&self) -> usize {
        match self {
            Self::Primitive(p) => p.byte_size(),
            Self::Pointer(_) => DataType::U32.byte_size(),
            Self::Array(size, t) => size * t.byte_size(),
            Self::Struct(s) => s.fields.values().map(|v| v.dtype.byte_size()).sum(),
            Self::Function(_) => DataType::U32.byte_size(),
            Self::Const(t) => t.byte_size(),
            Self::Opaque(_) => 0,
        }
    }

    pub fn base_type(&self) -> Option<Type> {
        match self {
            Self::Primitive(p) => Some(Self::Primitive(*p)),
            Self::Pointer(_) => Some(Self::Primitive(DataType::U32)),
            Self::Array(_, t) => Some(t.as_ref().clone()),
            Self::Struct(_) => None,
            Self::Function(_) => None,
            Self::Const(t) => t.base_type(),
            Self::Opaque(r) => r.get_type(true).ok(),
        }
    }

    pub fn primitive_type(&self) -> Option<DataType> {
        if let Some(Self::Primitive(p)) = self.base_type() {
            Some(p)
        } else if let Self::Function(_) = &self {
            Some(DataType::U32)
        } else {
            None
        }
    }

    pub fn remove_const(&self) -> Type {
        match self {
            Self::Const(t) => t.as_ref().clone(),
            t => t.clone(),
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_) | Self::Function(_))
    }

    pub fn coerce_type(a: DataType, b: DataType) -> DataType {
        a.max(b)
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const(_))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{p}"),
            Self::Array(size, t) => write!(f, "[{size}]{t}"),
            Self::Const(t) => write!(f, "const {t}"),
            Self::Pointer(t) => write!(f, "*{t}"),
            Self::Struct(s) => write!(f, "{}", s.name),
            Self::Function(d) => write!(
                f,
                "fn({}){}",
                d.parameters
                    .iter()
                    .map(|p| format!("{p}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                d.return_type
                    .as_ref()
                    .map_or("void".to_string(), |r| r.to_string())
            ),
            Self::Opaque(r) => write!(f, "&{}", r.name.get_value()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDefinition {
    name: String,
    fields: HashMap<String, Rc<StructField>>,
    size: usize,
}

impl StructDefinition {
    pub fn read_definition(
        tokens: &mut TokenIter,
        state: &mut CompilingState,
    ) -> Result<(Rc<Self>, Token), TokenError> {
        tokens.expect("struct")?;
        let name = tokens.next()?;
        get_identifier(&name)?;

        state.add_user_type(name.clone(), UserTypeOptions::OpaqueType(name.clone()))?;

        let mut s = Self {
            name: name.get_value().to_string(),
            fields: HashMap::new(),
            size: 0,
        };

        tokens.expect("{")?;

        while !tokens.expect_peek("}") {
            let field_token = tokens.next()?;
            let field_name = get_identifier(&field_token)?.to_string();
            tokens.expect(":")?;

            let dtype = Type::read_type(tokens, state)?;
            let dtype_size = dtype.byte_size();
            tokens.expect(";")?;

            match s.fields.entry(field_name) {
                Entry::Vacant(e) => e.insert(Rc::new(StructField {
                    offset: s.size,
                    dtype,
                })),
                Entry::Occupied(_) => {
                    return Err(field_token.into_err("field with name already exists"));
                }
            };

            s.size += dtype_size;
        }

        tokens.expect("}")?;

        let rs = Rc::new(s);

        state.upgrade_opaque_type(name.clone(), Type::Struct(rs.clone()))?;
        Ok((rs, name))
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_field(&self, field_name: &str) -> Option<Rc<StructField>> {
        self.fields.get(field_name).cloned()
    }

    pub fn get_fields(&self) -> &HashMap<String, Rc<StructField>> {
        &self.fields
    }

    pub fn get_size(&self) -> usize {
        self.size
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub offset: usize,
    pub dtype: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionParameter {
    pub dtype: Type,
    pub name: Option<Token>,
}

impl Eq for FunctionParameter {}
impl PartialEq for FunctionParameter {
    fn eq(&self, other: &Self) -> bool {
        self.dtype == other.dtype
    }
}

impl Display for FunctionParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(n) = &self.name {
            write!(f, "{n}:")?;
        }
        write!(f, "{}", self.dtype)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<Type>,
}

impl Function {
    pub fn read_tokens(
        tokens: &mut TokenIter,
        state: &CompilingState,
        include_names: bool,
    ) -> Result<Self, TokenError> {
        tokens.expect("(")?;

        let mut params = Vec::new();

        while !tokens.expect_peek(")") {
            let param_name = if include_names {
                let p_token = tokens.next()?;
                get_identifier(&p_token)?;
                tokens.expect(":")?;
                Some(p_token)
            } else {
                None
            };

            let param_type = Type::read_type(tokens, state)?;

            params.push(FunctionParameter {
                dtype: param_type,
                name: param_name,
            });

            if !tokens.expect_peek(")") {
                tokens.expect(",")?;
            } else {
                break;
            }
        }

        tokens.expect(")")?;

        let ret = if tokens.expect_peek("void") {
            tokens.expect("void")?;
            None
        } else {
            Some(Type::read_type(tokens, state)?)
        };

        Ok(Self {
            parameters: params,
            return_type: ret,
        })
    }
}

#[derive(Debug, Clone)]
pub struct UnknownPrimitiveError {
    token: Token,
}

impl fmt::Display for UnknownPrimitiveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "unable to find matching primitive type from \"{}\"",
            self.token.get_value()
        )
    }
}

impl From<UnknownPrimitiveError> for TokenError {
    fn from(value: UnknownPrimitiveError) -> Self {
        let msg = value.to_string();
        value.token.into_err(msg)
    }
}

impl TryFrom<TokenIter<'_>> for DataType {
    type Error = TokenError;

    fn try_from(value: TokenIter) -> Result<Self, Self::Error> {
        let mut value = value;

        let first = value.next()?;

        if value.peek().is_some() {
            Err(TokenError {
                token: None,
                msg: "UNKNOWN!".to_owned(),
            })
        } else {
            Ok(DataType::try_from(first)?)
        }
    }
}

impl TryFrom<Token> for DataType {
    type Error = UnknownPrimitiveError;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        static PRIMITIVE_MAP: LazyLock<HashMap<String, DataType>> =
            LazyLock::new(|| DataType::ALL.iter().map(|v| (v.to_string(), *v)).collect());

        if let Some(p) = PRIMITIVE_MAP.get(value.get_value()) {
            Ok(*p)
        } else {
            Err(UnknownPrimitiveError { token: value })
        }
    }
}
