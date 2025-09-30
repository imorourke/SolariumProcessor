use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    sync::LazyLock,
};

use jib::cpu::{DataType, Register, convert_types};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, Instruction, OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBxor,
    OpCall, OpConv, OpCopy, OpDiv, OpLd, OpLdi, OpLdn, OpMul, OpNeg, OpNot, OpPopr, OpPush, OpRem,
    OpSav, OpSub, OpTeq, OpTg, OpTge, OpTl, OpTle, OpTneq,
};

use crate::{
    TokenError,
    compiler::{CompilingState, Statement},
    literals::{Literal, LiteralValue},
    tokenizer::{KEYWORD_SIZEOF, Token, TokenIter, get_identifier, is_identifier},
    typing::{StructDefinition, StructField, Type},
    utilities::{MemcpyStatement, load_to_register},
    variables::LocalVariable,
};

#[derive(Debug, Clone, Copy)]
pub struct RegisterDef {
    pub reg: Register,
    pub spare: Register,
}

#[derive(Debug, Clone, Copy)]
pub enum RegisterDefError {
    RegisterEqualToSpare,
}

impl Display for RegisterDefError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "register and spare location cannot be the same")
    }
}

impl RegisterDef {
    pub const FN_VAR_BASE: Register = Register::first_gp_register();
    pub const FN_TEMPVAR_BASE: Register = Self::increment_const(Self::FN_VAR_BASE);
    pub const SPARE: Register = Self::increment_const(Self::FN_TEMPVAR_BASE);
    pub const FIRST_USEABLE: Register = Self::increment_const(Self::SPARE);

    const fn increment_const(reg: Register) -> Register {
        let next = Register::GeneralPurpose(reg.get_index() + 1);
        assert!(next.get_index() <= Register::last_register().get_index());
        next
    }

    pub fn new(reg: Register) -> Result<Self, RegisterDefError> {
        if reg < Self::FIRST_USEABLE || reg > Register::last_register() {
            Err(RegisterDefError::RegisterEqualToSpare)
        } else {
            Ok(Self {
                reg,
                spare: Self::SPARE,
            })
        }
    }

    fn increment(&self) -> Option<Self> {
        Self::new(Register::GeneralPurpose(self.reg.get_index() + 1)).ok()
    }

    pub fn increment_token(&self, token: &Token) -> Result<Self, TokenError> {
        match self.increment() {
            Some(x) => Ok(x),
            None => Err(token.clone().into_err("cannot find valid register vlaue")),
        }
    }
}

impl Default for RegisterDef {
    fn default() -> Self {
        Self::new(Self::FIRST_USEABLE).unwrap()
    }
}

impl Display for RegisterDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.reg)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ExpressionData {
    pub asm: Vec<AsmTokenLoc>,
}

impl ExpressionData {
    pub fn new<T: IntoIterator<Item = AsmTokenLoc>>(asm: T) -> Self {
        Self {
            asm: asm.into_iter().collect(),
        }
    }

    pub fn merge<T: IntoIterator<Item = Self>>(other: T) -> Self {
        let mut s = Self::default();
        for i in other.into_iter() {
            s.append(i);
        }
        s
    }

    pub fn push_asm(&mut self, v: AsmTokenLoc) {
        self.asm.push(v);
    }

    pub fn append(&mut self, other: Self) {
        self.asm.extend(other.asm);
    }

    pub fn extend_asm<T: IntoIterator<Item = AsmTokenLoc>>(&mut self, asm: T) {
        self.asm.extend(asm);
    }

    pub fn into_asm(self) -> Vec<AsmTokenLoc> {
        self.asm
    }
}

#[derive(Debug, Default, Copy, Clone)]
pub struct TemporaryStackTracker {
    pub offset: usize,
    pub max_size: usize,
}

impl TemporaryStackTracker {
    pub fn merge(&mut self, other: TemporaryStackTracker) {
        self.max_size = self.max_size.max(other.max_size);
    }

    pub fn add_offset_value(&mut self, val: usize) {
        self.offset += val;
        self.max_size = self.max_size.max(self.offset);
    }
}

pub trait Expression: Debug + Display {
    fn get_token(&self) -> &Token;

    fn get_type(&self) -> Result<Type, TokenError>;

    fn get_primitive_type(&self) -> Result<DataType, TokenError> {
        if let Some(t) = self.get_type()?.primitive_type() {
            Ok(t)
        } else {
            Err(self
                .get_token()
                .clone()
                .into_err("unable to obtain primitive type from expression"))
        }
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
        #[allow(unused_variables)] required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        Err(self.get_token().clone().into_err(format!(
            "expression does not have a mappable address in memory - cannot load to {reg}"
        )))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError>;

    fn simplify(&self) -> Option<Literal> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperation {
    BitNot,
    Not,
    Plus,
    Minus,
}

impl UnaryOperation {
    pub const ALL: &[Self] = &[Self::BitNot, Self::Not, Self::Plus, Self::Minus];
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::BitNot => "~",
            Self::Not => "!",
            Self::Plus => "+",
            Self::Minus => "-",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    token: Token,
    op: UnaryOperation,
    base: Rc<dyn Expression>,
}

impl UnaryExpression {
    pub fn new(token: Token, op: UnaryOperation, base: Rc<dyn Expression>) -> Self {
        Self { token, op, base }
    }
}

impl Expression for UnaryExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Some(Type::Primitive(t)) = self.base.get_type()?.base_type() {
            Ok(Type::Primitive(t))
        } else {
            Err(self
                .token
                .clone()
                .into_err("expression is not a primitive data type for unary expression"))
        }
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut base_expr = self.base.load_value_to_register(reg, required_stack)?;

        let asm_inst = match self.op {
            UnaryOperation::Plus => None,
            UnaryOperation::Minus => Some(AsmToken::OperationLiteral(Box::new(OpNeg::new(
                ArgumentType::new(reg.reg, self.base.get_primitive_type()?),
                reg.reg.into(),
            )))),
            UnaryOperation::Not => Some(AsmToken::OperationLiteral(Box::new(OpNot::new(
                reg.reg.into(),
                reg.reg.into(),
            )))),
            UnaryOperation::BitNot => Some(AsmToken::OperationLiteral(Box::new(OpBnot::new(
                ArgumentType::new(reg.reg, self.base.get_primitive_type()?),
                reg.reg.into(),
            )))),
        };

        if let Some(inst) = asm_inst {
            base_expr.asm.push(self.token.to_asm(inst));
        }

        Ok(base_expr)
    }

    fn simplify(&self) -> Option<Literal> {
        LiteralValue::unary(*self.base.simplify()?.get_value(), self.op)
            .map_or(None, |x| Some(Literal::new(self.token.clone(), x)))
    }
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.op, self.base)
    }
}

#[derive(Debug, Clone)]
struct DereferenceExpression {
    token: Token,
    base: Rc<dyn Expression>,
}

impl Expression for DereferenceExpression {
    fn get_type(&self) -> Result<Type, TokenError> {
        if let Type::Pointer(dt) = self.base.get_type()? {
            Ok(dt.as_ref().clone())
        } else {
            Err(self
                .base
                .get_token()
                .clone()
                .into_err("cannot dereference a non-pointer type"))
        }
    }

    fn get_token(&self) -> &Token {
        &self.token
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        self.base.load_value_to_register(reg, required_stack)
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = self.base.load_value_to_register(reg, required_stack)?;
        let dt = self.get_primitive_type()?;

        asm.push_asm(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    ArgumentType::new(reg.reg, dt),
                    reg.reg.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for DereferenceExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*{}", self.base)
    }
}

#[derive(Debug, Clone)]
struct AddressOfExpression {
    token: Token,
    base: Rc<dyn Expression>,
}

impl Expression for AddressOfExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Pointer(Box::new(self.base.get_type()?)))
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        self.base.load_address_to_register(reg, required_stack)
    }
}

impl Display for AddressOfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&{}", self.base)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
    Arithmetic(BinaryArithmeticOperation),
    Assignment,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Arithmetic(op) => write!(f, "{op}"),
            Self::Assignment => write!(f, "="),
        }
    }
}

impl BinaryOperation {
    pub const ALL: &[BinaryOperation] = &Self::get_all();
    const DISTINCT_VALS: &[BinaryOperation] = &[BinaryOperation::Assignment];

    const LEN_VAL: usize =
        BinaryArithmeticOperation::ALL.len() + BinaryOperation::DISTINCT_VALS.len();
    const fn get_all() -> [Self; Self::LEN_VAL] {
        let mut vals = [Self::Assignment; Self::LEN_VAL];

        let mut i = 0;
        while i < BinaryArithmeticOperation::ALL.len() {
            vals[i] = Self::Arithmetic(BinaryArithmeticOperation::ALL[i]);
            i += 1;
        }

        i = 0;
        while i < Self::DISTINCT_VALS.len() {
            vals[BinaryArithmeticOperation::ALL.len() + i] = Self::DISTINCT_VALS[i];
            i += 1;
        }

        vals
    }

    pub const fn get_priority(&self) -> i32 {
        match self {
            Self::Arithmetic(op) => op.get_priority(),
            Self::Assignment => 100,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryArithmeticOperation {
    Plus,
    Minus,
    Product,
    Divide,
    Mod,
    And,
    Or,
    Equals,
    NotEquals,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BitAnd,
    BitOr,
    BitXor,
}

impl BinaryArithmeticOperation {
    pub const ALL: &[Self] = &[
        Self::Plus,
        Self::Minus,
        Self::Product,
        Self::Divide,
        Self::Mod,
        Self::And,
        Self::Or,
        Self::Equals,
        Self::NotEquals,
        Self::Greater,
        Self::GreaterEqual,
        Self::Less,
        Self::LessEqual,
        Self::BitAnd,
        Self::BitOr,
        Self::BitXor,
    ];

    pub const fn get_priority(&self) -> i32 {
        match self {
            Self::Plus => -5,
            Self::Minus => -5,
            Self::Product => -10,
            Self::Divide => -10,
            Self::Mod => -10,
            Self::And => 0,
            Self::Or => 0,
            Self::Equals => 0,
            Self::NotEquals => 0,
            Self::Greater => 0,
            Self::GreaterEqual => 0,
            Self::Less => 0,
            Self::LessEqual => 0,
            Self::BitAnd => 0,
            Self::BitOr => 0,
            Self::BitXor => 0,
        }
    }
}

impl Display for BinaryArithmeticOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Product => "*",
            Self::Divide => "/",
            Self::Mod => "%",
            Self::And => "&&",
            Self::Or => "||",
            Self::Equals => "==",
            Self::NotEquals => "!=",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
        };

        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub struct BinaryArithmeticExpression {
    token: Token,
    operation: BinaryArithmeticOperation,
    lhs: Rc<dyn Expression>,
    rhs: Rc<dyn Expression>,
}

impl BinaryArithmeticExpression {
    pub fn new(
        token: Token,
        op: BinaryArithmeticOperation,
        lhs: Rc<dyn Expression>,
        rhs: Rc<dyn Expression>,
    ) -> Self {
        Self {
            token,
            operation: op,
            lhs,
            rhs,
        }
    }

    fn get_ptr_size(t: &Type) -> Option<usize> {
        match t {
            Type::Pointer(base) => Some(base.byte_size()),
            _ => None,
        }
    }
}

impl Expression for BinaryArithmeticExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        let ta = self.lhs.get_type()?;
        let tb = self.rhs.get_type()?;

        if let Some(sa) = Self::get_ptr_size(&ta)
            && let Some(sb) = Self::get_ptr_size(&tb)
        {
            if sa == sb {
                Ok(Type::Primitive(DataType::I32))
            } else {
                Err(self.token.clone().into_err(format!(
                    "cannot perform arithmetic with {ta} size {sa} and {tb} size {sb}"
                )))
            }
        } else if ta.is_pointer() {
            Ok(ta)
        } else if tb.is_pointer() {
            Ok(tb)
        } else if let Some(Type::Primitive(a)) = ta.base_type() {
            if let Some(Type::Primitive(b)) = tb.base_type() {
                Ok(Type::Primitive(Type::coerce_type(a, b)))
            } else {
                Err(self
                    .rhs
                    .get_token()
                    .clone()
                    .into_err("cannot convert expression to expression primitive type"))
            }
        } else {
            Err(self
                .lhs
                .get_token()
                .clone()
                .into_err("cannot convert expression to expression primitive type"))
        }
    }

    fn simplify(&self) -> Option<Literal> {
        let rhs_val = self.rhs.simplify()?;
        let lhs_val = self.lhs.simplify()?;

        if let Ok(v) =
            LiteralValue::operation(*lhs_val.get_value(), *rhs_val.get_value(), self.operation)
        {
            Some(Literal::new(self.token.clone(), v))
        } else {
            None
        }
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        if let Some(dt) = self.get_type()?.primitive_type() {
            let mut asm = self.lhs.load_value_to_register(reg, required_stack)?;
            let reg_a = reg.reg;

            let reg_b;
            if let Some(reg_next) = reg.increment() {
                asm.append(self.rhs.load_value_to_register(reg_next, required_stack)?);
                reg_b = reg_next.reg;
            } else {
                asm.push_asm(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpPush::new(reg.reg.into()),
                ))));
                asm.append(self.rhs.load_value_to_register(reg, required_stack)?);
                asm.push_asm(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpCopy::new(reg.spare.into(), reg.reg.into()),
                ))));
                asm.push_asm(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                    OpPopr::new(reg.reg.into()),
                ))));
                reg_b = reg.spare;
            }

            if let Some(p) = self.lhs.get_type()?.primitive_type() {
                if p != dt {
                    asm.push_asm(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpConv::new(ArgumentType::new(reg_a, dt), ArgumentType::new(reg_a, p)),
                    ))));
                }
            } else {
                panic!("type error!")
            }

            if let Some(p) = self.rhs.get_type()?.primitive_type() {
                if p != dt {
                    asm.push_asm(self.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpConv::new(ArgumentType::new(reg_b, dt), ArgumentType::new(reg_b, p)),
                    ))));
                }
            } else {
                panic!("type error")
            }

            let type_a = self.lhs.get_type()?;
            let type_b = self.rhs.get_type()?;
            let mut div_val = None;

            let pointer_instructions = if self.operation == BinaryArithmeticOperation::Plus
                || self.operation == BinaryArithmeticOperation::Minus
            {
                let tas = Self::get_ptr_size(&type_a);
                let tbs = Self::get_ptr_size(&type_b);

                if let Some(sa) = tas
                    && let Some(sb) = tbs
                {
                    if sa != sb {
                        Err(self
                        .get_token()
                        .clone()
                        .into_err("cannot add one pointer to another pointer directly with mismatching sizes"))
                    } else {
                        div_val = Some(sa);
                        Ok(Vec::new())
                    }
                } else {
                    fn generate_ptr_mul_asm(
                        size: usize,
                        incr_val: Register,
                        spare: Register,
                    ) -> Result<Vec<AsmToken>, TokenError> {
                        if size != 1 {
                            let mut insts = vec![AsmToken::OperationLiteral(Box::new(
                                OpPush::new(spare.into()),
                            ))];

                            if size <= u16::MAX as usize {
                                let reg_bt = ArgumentType::new(spare, DataType::U16);
                                insts.push(AsmToken::OperationLiteral(Box::new(OpLdi::new(
                                    reg_bt,
                                    size as u16,
                                ))));
                            } else {
                                let reg_bt = ArgumentType::new(spare, DataType::U32);
                                insts
                                    .push(AsmToken::OperationLiteral(Box::new(OpLdn::new(reg_bt))));
                                insts.push(AsmToken::Literal4(size as u32));
                                insts.push(AsmToken::AlignInstruction);
                            }
                            insts.push(AsmToken::OperationLiteral(Box::new(OpMul::new(
                                ArgumentType::new(incr_val, DataType::I32),
                                spare.into(),
                                incr_val.into(),
                            ))));
                            insts.push(AsmToken::OperationLiteral(Box::new(OpPopr::new(
                                spare.into(),
                            ))));

                            Ok(insts)
                        } else {
                            Ok(Vec::new())
                        }
                    }

                    if let Some(sa) = tas {
                        generate_ptr_mul_asm(sa, reg_b, reg_a)
                    } else if let Some(sb) = tbs {
                        generate_ptr_mul_asm(sb, reg_a, reg_b)
                    } else {
                        Ok(Vec::new())
                    }
                }?
            } else {
                Vec::new()
            };

            asm.extend_asm(
                pointer_instructions
                    .into_iter()
                    .map(|x| self.token.to_asm(x)),
            );

            let reg_type = ArgumentType::new(reg.reg, dt);

            let op_instructions: Vec<Box<dyn Instruction>> = match self.operation {
                BinaryArithmeticOperation::NotEquals => {
                    vec![Box::new(OpTneq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Equals => {
                    vec![Box::new(OpTeq::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Greater => {
                    vec![Box::new(OpTg::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::GreaterEqual => {
                    vec![Box::new(OpTge::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Less => {
                    vec![Box::new(OpTl::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::LessEqual => {
                    vec![Box::new(OpTle::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Plus => {
                    vec![Box::new(OpAdd::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Minus => {
                    vec![Box::new(OpSub::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Product => {
                    vec![Box::new(OpMul::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Divide => {
                    vec![Box::new(OpDiv::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::Mod => {
                    vec![Box::new(OpRem::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::And => {
                    vec![
                        Box::new(OpBool::new(reg_a.into(), reg_a.into())),
                        Box::new(OpBool::new(reg_b.into(), reg_b.into())),
                        Box::new(OpBand::new(reg_type, reg_a.into(), reg_b.into())),
                    ]
                }
                BinaryArithmeticOperation::Or => {
                    vec![
                        Box::new(OpBor::new(reg_type, reg_a.into(), reg_b.into())),
                        Box::new(OpBool::new(reg_b.into(), reg_b.into())),
                    ]
                }
                BinaryArithmeticOperation::BitAnd => {
                    vec![Box::new(OpBand::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::BitOr => {
                    vec![Box::new(OpBor::new(reg_type, reg_a.into(), reg_b.into()))]
                }
                BinaryArithmeticOperation::BitXor => {
                    vec![Box::new(OpBxor::new(reg_type, reg_a.into(), reg_b.into()))]
                }
            };

            asm.extend_asm(
                self.get_token()
                    .to_asm_iter(op_instructions.into_iter().map(AsmToken::OperationLiteral)),
            );

            if let Some(d) = div_val {
                asm.extend_asm(self.token.to_asm_iter([
                    AsmToken::OperationLiteral(Box::new(OpLdi::new(
                        ArgumentType::new(reg.spare, DataType::U16),
                        d as u16,
                    ))),
                    AsmToken::OperationLiteral(Box::new(OpDiv::new(
                        reg_type,
                        reg_type.reg,
                        reg.spare.into(),
                    ))),
                ]));
            }

            Ok(asm)
        } else {
            Err(self
                .get_token()
                .clone()
                .into_err("non-primitive types not yet supported"))
        }
    }
}

impl Display for BinaryArithmeticExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.lhs, self.operation, self.rhs)
    }
}

#[derive(Debug, Clone)]
struct AsExpression {
    token: Token,
    data_type: Type,
    expr: Rc<dyn Expression>,
}

impl Expression for AsExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.data_type.clone())
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = self.expr.load_value_to_register(reg, required_stack)?;
        if let Some(dt) = self.data_type.primitive_type() {
            let src_type = self.expr.get_primitive_type()?;

            if dt != src_type {
                asm.push_asm(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                            ArgumentType::new(reg.reg, dt),
                            ArgumentType::new(reg.reg, src_type),
                        )))),
                );
            }
            Ok(asm)
        } else {
            Err(self.token.clone().into_err("unable to perform conversion"))
        }
    }

    fn simplify(&self) -> Option<Literal> {
        if let Some(dt) = self.data_type.primitive_type() {
            let lit = self.expr.simplify()?;
            let val = lit.get_value();
            let res = LiteralValue::from_u32(convert_types(val.as_u32(), val.get_dtype(), dt), dt);

            Some(Literal::new(lit.get_token().clone(), res))
        } else {
            None
        }
    }
}

impl Display for AsExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}):{}", self.expr, self.data_type)
    }
}

#[derive(Debug, Clone)]
struct FieldAccessExpression {
    token: Token,
    field: Token,
    s_expression: Rc<dyn Expression>,
    s_expr_is_pointer: bool,
}

impl FieldAccessExpression {
    fn get_struct(&self) -> Result<Rc<StructDefinition>, TokenError> {
        let mut base_type = self.s_expression.get_type()?;
        while matches!(base_type, Type::Opaque(_) | Type::Const(_)) {
            if let Some(bt) = base_type.base_type() {
                base_type = bt;
            } else {
                return Err(self
                    .token
                    .clone()
                    .into_err(format!("unable to get base type for {}", self.s_expression)));
            }
        }

        fn get_struct_from_base(
            token: &Token,
            base_type: Type,
        ) -> Result<Rc<StructDefinition>, TokenError> {
            match base_type {
                Type::Opaque(o) => {
                    if let Type::Struct(s) = o.get_type(true)? {
                        Ok(s)
                    } else {
                        Err(token.clone().into_err(format!(
                            "left-hand expression to field access is not a structure from user type- {}",
                            o.name
                        )))
                    }
                }
                Type::Struct(s) => Ok(s.clone()),
                x => Err(token.clone().into_err(format!(
                    "left-hand expression to field access is not a structure - {}",
                    x
                ))),
            }
        }

        if !self.s_expr_is_pointer {
            get_struct_from_base(&self.token, base_type)
        } else if let Type::Pointer(p) = &base_type {
            get_struct_from_base(&self.token, p.as_ref().clone())
        } else {
            Err(self.token.clone().into_err(format!(
                "left-hand expression to field access is not a structure - {}",
                base_type
            )))
        }
    }

    fn get_field(&self) -> Result<Rc<StructField>, TokenError> {
        let st = self.get_struct()?;
        if let Some(f) = st.get_field(self.field.get_value()) {
            Ok(f)
        } else {
            Err(self.field.clone().into_err(format!(
                "no field with name found for structure type {}",
                st.get_name()
            )))
        }
    }
}

impl Expression for FieldAccessExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.get_field()?.dtype.clone())
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = if self.s_expr_is_pointer {
            self.s_expression
                .load_value_to_register(reg, required_stack)?
        } else {
            self.s_expression
                .load_address_to_register(reg, required_stack)?
        };

        asm.extend_asm(
            self.token
                .to_asm_iter(load_to_register(reg.spare, self.get_field()?.offset as u32)),
        );
        asm.push_asm(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(reg.reg, DataType::U32),
                    reg.reg.into(),
                    reg.spare.into(),
                )))),
        );
        Ok(asm)
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = self.load_address_to_register(reg, required_stack)?;
        let dt = self.get_primitive_type()?;

        asm.push_asm(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    ArgumentType::new(reg.reg, dt),
                    reg.reg.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for FieldAccessExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.s_expression,
            if self.s_expr_is_pointer { "->" } else { "." },
            self.field.get_value()
        )
    }
}

#[derive(Debug, Clone)]
struct AssignmentExpression {
    token: Token,
    addr_expr: Rc<dyn Expression>,
    val_expr: Rc<dyn Expression>,
}

impl Display for AssignmentExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.addr_expr, self.val_expr)
    }
}

impl Expression for AssignmentExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        self.addr_expr.get_type()
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let tok = self.addr_expr.get_token();

        let mut asm = ExpressionData::default();
        asm.push_asm(self.token.to_asm(AsmToken::Comment(format!(
            "assignment of {} with {}",
            self.addr_expr, self.val_expr
        ))));

        let addr_def = reg;
        let val_def = addr_def.increment_token(tok)?;

        if let Some(dest_dtype) = self.addr_expr.get_type()?.primitive_type() {
            let val_dtype = self.val_expr.get_primitive_type()?;

            asm.append(
                self.addr_expr
                    .load_address_to_register(addr_def, required_stack)?,
            );
            asm.append(
                self.val_expr
                    .load_value_to_register(val_def, required_stack)?,
            );

            if val_dtype != dest_dtype {
                asm.push_asm(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                    ArgumentType::new(val_def.reg, dest_dtype),
                    ArgumentType::new(val_def.reg, val_dtype),
                )))));
            }

            asm.push_asm(tok.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                ArgumentType::new(addr_def.reg, dest_dtype),
                val_def.reg.into(),
            )))));

            Ok(asm)
        } else if let Ok(val_type) = self.val_expr.get_type()
            && let Ok(target_type) = self.addr_expr.get_type()
        {
            if val_type == target_type {
                asm.append(
                    self.addr_expr
                        .load_address_to_register(addr_def, required_stack)?,
                );
                asm.append(
                    self.val_expr
                        .load_address_to_register(val_def, required_stack)?,
                );

                let mem = MemcpyStatement::new(
                    self.token.clone(),
                    val_def.reg,
                    addr_def.reg,
                    val_type.byte_size(),
                );

                asm.extend_asm(mem.get_exec_code(required_stack)?);

                Ok(asm)
            } else {
                Err(tok.clone().into_err(format!(
                    "mismatch of types assigning {val_type} to {target_type}"
                )))
            }
        } else {
            Err(tok
                .clone()
                .into_err("unable to determine valid types for input values"))
        }
    }
}

#[derive(Debug, Clone)]
struct ArrayIndexExpression {
    token: Token,
    address_expr: Rc<dyn Expression>,
    index_expr: Rc<dyn Expression>,
}

impl Display for ArrayIndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(({})[{}])", self.address_expr, self.index_expr)
    }
}

impl Expression for ArrayIndexExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        match self.address_expr.get_type()? {
            Type::Array(_, inner) => Ok(inner.as_ref().clone()),
            Type::Pointer(inner) => Ok(inner.as_ref().clone()),
            v => Err(self.token.clone().into_err(format!(
                "unable to convert from `{}` into a valid array type",
                v
            ))),
        }
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = match self.address_expr.get_type()? {
            Type::Array(_, _) => self
                .address_expr
                .load_address_to_register(reg, required_stack)?,
            Type::Pointer(_) => self
                .address_expr
                .load_value_to_register(reg, required_stack)?,
            v => {
                return Err(self.token.clone().into_err(format!(
                    "unable to convert from `{}` into a valid array type",
                    v
                )));
            }
        };

        let pt = self.index_expr.get_primitive_type()?;
        if !pt.integral() {
            return Err(self
                .index_expr
                .get_token()
                .clone()
                .into_err("unable to convert into an integer value for index operator"));
        }

        let index_reg = reg.increment_token(&self.token)?;
        let type_size = self.get_type()?.byte_size() as u32;

        let index_expr = BinaryArithmeticExpression::new(
            self.index_expr.get_token().clone(),
            BinaryArithmeticOperation::Product,
            self.index_expr.clone(),
            Rc::new(Literal::new(
                self.index_expr.get_token().clone(),
                LiteralValue::U32(type_size),
            )),
        );

        if let Some(lit) = index_expr.simplify() {
            asm.append(lit.load_value_to_register(index_reg, required_stack)?);
        } else {
            asm.append(
                self.index_expr
                    .load_value_to_register(index_reg, required_stack)?,
            );

            if type_size != 1 {
                asm.extend_asm(self.token.to_asm_iter(load_to_register(
                    reg.spare,
                    self.get_type()?.byte_size() as u32,
                )));
                asm.push_asm(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpMul::new(
                            ArgumentType::new(index_reg.reg, DataType::I32),
                            index_reg.reg.into(),
                            reg.spare.into(),
                        )))),
                );
            }
        }

        asm.push_asm(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(reg.reg, DataType::I32),
                    reg.reg.into(),
                    index_reg.reg.into(),
                )))),
        );

        Ok(asm)
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = self.load_address_to_register(reg, required_stack)?;
        asm.push_asm(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                    ArgumentType::new(reg.reg, self.get_primitive_type()?),
                    reg.reg.into(),
                )))),
        );
        Ok(asm)
    }
}

#[derive(Debug, Clone)]
struct FunctionCallExpression {
    token: Token,
    args: Vec<Rc<dyn Expression>>,
    func: Rc<dyn Expression>,
}

impl Display for FunctionCallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.func)?;
        for p in self.args.iter() {
            write!(f, "{p},")?;
        }
        write!(f, ")")
    }
}

impl Expression for FunctionCallExpression {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        if let Type::Function(f) = self.func.get_type()? {
            Ok(f.return_type
                .clone()
                .unwrap_or(Type::Primitive(DataType::U32)))
        } else {
            Err(self
                .token
                .clone()
                .into_err("cannot call a non-function type"))
        }
    }

    fn load_address_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        if let Type::Function(f) = self.func.get_type()?
            && let Some(rt) = &f.return_type
            && rt.primitive_type().is_none()
        {
            self.load_value_to_register(reg, required_stack)
        } else {
            Err(self
                .token
                .clone()
                .into_err("unable to load function with non-struct return type as address"))
        }
    }

    fn load_value_to_register(
        &self,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let func = if let Type::Function(f) = self.func.get_type()? {
            f.clone()
        } else {
            return Err(self
                .token
                .clone()
                .into_err("caller value is not a function type"));
        };

        let mut asm = ExpressionData::new(vec![
            self.token
                .to_asm(AsmToken::Comment(format!("calling {}", self.func))),
        ]);

        // Initially use the provided register to demark the temporary parameters

        let init_offset = required_stack.offset;

        // Load the original stack offset for the variable values

        if init_offset > 0 {
            asm.extend_asm(
                self.token
                    .to_asm_iter(load_to_register(reg.reg, init_offset as u32)),
            );
            asm.push_asm(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                        ArgumentType::new(reg.reg, DataType::U32),
                        reg.reg.into(),
                        RegisterDef::FN_TEMPVAR_BASE.into(),
                    )))),
            );
        } else {
            asm.push_asm(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                        reg.reg.into(),
                        RegisterDef::FN_TEMPVAR_BASE.into(),
                    )))),
            );
        }

        // Add parameter values

        let next_load = reg.increment_token(&self.token)?;

        if self.args.len() != func.parameters.len() {
            return Err(self.token.clone().into_err(format!(
                "expected {} arguments for {}, found {}",
                func.parameters.len(),
                self.func,
                self.args.len()
            )));
        }

        for (p, e) in func.parameters.iter().zip(self.args.iter()) {
            if let Some(p_name) = p.name.clone() {
                let var = Rc::new(LocalVariable::new(
                    p_name,
                    p.dtype.clone(),
                    RegisterDef::FN_TEMPVAR_BASE,
                    required_stack.offset,
                    None,
                )?);
                required_stack.add_offset_value(p.dtype.byte_size());

                // Use an assignment expression for better control over the register usage
                let assign = Rc::new(AssignmentExpression {
                    addr_expr: var,
                    token: self.token.clone(),
                    val_expr: e.clone(),
                });

                let mut tmp_vals = *required_stack;
                asm.push_asm(self.token.to_asm(AsmToken::Comment(format!(
                    "func {} arg {} :: {}",
                    self.func,
                    p.name.as_ref().map(|x| x.get_value().to_string()).unwrap_or("??".to_string()),
                    assign
                ))));
                asm.append(assign.load_value_to_register(next_load, &mut tmp_vals)?);
                required_stack.merge(tmp_vals);
            } else {
                return Err(self
                    .token
                    .clone()
                    .into_err("unable to find name for parameter at"));
            }
        }

        // Load the function location

        asm.append(
            self.func
                .load_value_to_register(next_load, required_stack)?,
        );

        asm.extend_asm(self.token.to_asm_iter([AsmToken::OperationLiteral(Box::new(
            OpPush::new(Register::Return.into()),
        ))]));

        if let Some(rt) = &func.return_type
            && rt.primitive_type().is_none()
        {
            asm.extend_asm(self.token.to_asm_iter(load_to_register(
                Register::Return,
                required_stack.offset as u32,
            )));
            required_stack.add_offset_value(rt.byte_size());

            asm.push_asm(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                        ArgumentType::new(Register::Return, DataType::U32),
                        Register::Return.into(),
                        RegisterDef::FN_TEMPVAR_BASE.into(),
                    )))),
            );
        }

        asm.extend_asm(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpPush::new(Register::ArgumentBase.into()))),
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                Register::ArgumentBase.into(),
                reg.reg.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpCall::new(next_load.reg.into()))),
        ]));

        // Remove the added stack space from the parameters

        asm.extend_asm(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                reg.reg.into(),
                Register::Return.into(),
            ))),
            AsmToken::OperationLiteral(Box::new(OpPopr::new(Register::ArgumentBase.into()))),
            AsmToken::OperationLiteral(Box::new(OpPopr::new(Register::Return.into()))),
        ]));

        // Return the result

        Ok(asm)
    }
}

static UNARY_STR: LazyLock<HashMap<String, UnaryOperation>> = LazyLock::new(|| {
    UnaryOperation::ALL
        .iter()
        .map(|x| (x.to_string(), *x))
        .collect()
});
static BINARY_STR: LazyLock<HashMap<String, BinaryOperation>> = LazyLock::new(|| {
    BinaryOperation::ALL
        .iter()
        .map(|x| (x.to_string(), *x))
        .collect()
});

fn parse_expression_without_binary_expressions(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    let first = tokens.next()?;

    let mut expr: Rc<dyn Expression> = if let Some(op) = UNARY_STR.get(first.get_value()) {
        Rc::new(UnaryExpression::new(
            first,
            *op,
            parse_expression_without_binary_expressions(tokens, state)?,
        ))
    } else if first.get_value() == "*" {
        Rc::new(DereferenceExpression {
            token: first,
            base: parse_expression_without_binary_expressions(tokens, state)?,
        })
    } else if first.get_value() == "&" {
        Rc::new(AddressOfExpression {
            token: first,
            base: parse_expression_without_binary_expressions(tokens, state)?,
        })
    } else if first.get_value() == "(" {
        let inner = parse_expression(tokens, state)?;
        tokens.expect(")")?;
        inner
    } else if first.get_value() == KEYWORD_SIZEOF {
        tokens.expect("(")?;
        let input_t = if let Some(next_token) = tokens.peek()
            && is_identifier(next_token.get_value())
            && let Ok(var) = state.get_variable(&next_token)
        {
            tokens.next()?;
            var.get_type()?
        } else {
            Type::read_type(tokens, state)?
        };
        tokens.expect(")")?;
        Rc::new(Literal::new(
            first.clone(),
            LiteralValue::U32(input_t.byte_size() as u32),
        ))
    } else if get_identifier(&first).is_ok() {
        state.get_variable(&first)?.clone()
    } else if first.get_value().starts_with('"') {
        state.get_string_literal(&first)?
    } else {
        Rc::new(Literal::try_from(first.clone())?)
    };

    const TOKEN_DOT: &str = ".";
    const TOKEN_ARROW: &str = "->";

    while let Some(next) = tokens.peek() {
        if BINARY_STR.contains_key(next.get_value()) {
            break;
        } else if next.get_value() == TOKEN_DOT {
            let dot_token = tokens.expect(TOKEN_DOT)?;
            let ident_token = tokens.next()?;

            expr = Rc::new(FieldAccessExpression {
                field: ident_token,
                token: dot_token,
                s_expression: expr,
                s_expr_is_pointer: false,
            })
        } else if next.get_value() == TOKEN_ARROW {
            let dot_token = tokens.expect(TOKEN_ARROW)?;
            let ident_token = tokens.next()?;

            expr = Rc::new(FieldAccessExpression {
                field: ident_token,
                token: dot_token,
                s_expression: expr,
                s_expr_is_pointer: true,
            })
        } else if next.get_value() == ":" {
            let token = tokens.expect(":")?;
            expr = Rc::new(AsExpression {
                data_type: Type::read_type(tokens, state)?,
                token,
                expr,
            })
        } else if next.get_value() == "(" {
            let call_val = tokens.expect("(")?;

            let mut args = Vec::new();

            while !tokens.expect_peek(")") {
                args.push(parse_expression(tokens, state)?);

                if tokens.expect_peek(",") {
                    tokens.expect(",")?;
                }
            }

            tokens.expect(")")?;

            expr = Rc::new(FunctionCallExpression {
                token: call_val,
                func: expr,
                args,
            });
        } else if next.get_value() == "[" {
            let array_token = tokens.expect("[")?;
            let index_expr = AsExpression {
                data_type: Type::Primitive(DataType::U32),
                token: array_token.clone(),
                expr: parse_expression(tokens, state)?,
            };

            expr = Rc::new(ArrayIndexExpression {
                token: array_token,
                address_expr: expr,
                index_expr: Rc::new(index_expr),
            });

            tokens.expect("]")?;
        } else {
            break;
        }
    }

    Ok(expr)
}

enum BinaryExpressionMatching {
    Expression(Rc<dyn Expression>),
    BinaryOperation(BinaryOperation, Token),
}

fn process_binary_expressions(
    exprs: &[BinaryExpressionMatching],
) -> Result<Rc<dyn Expression>, TokenError> {
    if exprs.len() == 1
        && let Some(BinaryExpressionMatching::Expression(e)) = exprs.first()
    {
        Ok(e.clone())
    } else if !exprs.is_empty() && exprs.len() % 2 == 1 {
        let priorities: HashMap<BinaryOperation, i32> =
            HashMap::from_iter(BinaryOperation::ALL.iter().map(|x| (*x, x.get_priority())));

        let mut highest_priority = None;

        for (i, val) in exprs.iter().enumerate() {
            if let BinaryExpressionMatching::BinaryOperation(op, _) = val {
                let priority = priorities.get(op).copied().unwrap_or(0);

                if let Some((highest, _)) = highest_priority
                    && priority > highest
                {
                    highest_priority = Some((priority, i));
                } else if highest_priority.is_none() {
                    highest_priority = Some((priority, i));
                }
            }
        }

        if let Some((_, index)) = highest_priority
            && let BinaryExpressionMatching::BinaryOperation(op, token) = &exprs[index]
        {
            let a = process_binary_expressions(&exprs[..index])?;
            let b = process_binary_expressions(&exprs[(index + 1)..])?;

            match *op {
                BinaryOperation::Arithmetic(opa) => Ok(Rc::new(BinaryArithmeticExpression::new(
                    token.clone(),
                    opa,
                    a,
                    b,
                ))),
                BinaryOperation::Assignment => {
                    if !a.get_type()?.is_const() {
                        Ok(Rc::new(AssignmentExpression {
                            token: token.clone(),
                            addr_expr: a,
                            val_expr: b,
                        }))
                    } else {
                        Err(token
                            .clone()
                            .into_err("left-hand expression is a constant value"))
                    }
                }
            }
        } else {
            Err(TokenError {
                token: None,
                msg: "unable to determine proper location for expressions".to_string(),
            })
        }
    } else {
        Err(TokenError {
            token: None,
            msg: "no binary expressions provided to process".to_string(),
        })
    }
}

pub fn parse_expression(
    tokens: &mut TokenIter,
    state: &CompilingState,
) -> Result<Rc<dyn Expression>, TokenError> {
    let mut main_expressions: Vec<BinaryExpressionMatching> =
        vec![BinaryExpressionMatching::Expression(
            parse_expression_without_binary_expressions(tokens, state)?,
        )];

    while let Some(next) = tokens.peek()
        && let Some(op) = BINARY_STR.get(next.get_value())
    {
        main_expressions.push(BinaryExpressionMatching::BinaryOperation(
            *op,
            tokens.next()?,
        ));
        main_expressions.push(BinaryExpressionMatching::Expression(
            parse_expression_without_binary_expressions(tokens, state)?,
        ));
    }

    let expr = process_binary_expressions(&main_expressions)?;

    if let Some(lit) = expr.simplify() {
        Ok(Rc::new(lit))
    } else {
        Ok(expr)
    }
}
