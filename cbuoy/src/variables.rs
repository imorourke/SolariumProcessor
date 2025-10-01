use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{ArgumentType, AsmToken, AsmTokenLoc, OpAdd, OpConv, OpCopy, OpLd, OpSav, OpSub};

use crate::{
    TokenError,
    compiler::{CodeGenerationOptions, CompilingState, GlobalStatement, Statement},
    expressions::{
        Expression, ExpressionData, RegisterDef, TemporaryStackTracker, parse_expression,
    },
    literals::Literal,
    tokenizer::{Token, TokenIter, get_identifier},
    typing::Type,
    utilities::{MemcpyStatement, load_to_register},
};

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    name: Token,
    dtype: Type,
    label: Rc<str>,
    init_expr: Option<Rc<dyn Expression>>,
}

impl GlobalVariable {
    pub fn new(
        token: Token,
        id: usize,
        dtype: Type,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<Self, TokenError> {
        let label = format!("___global_variable_{}_{}", id, get_identifier(&token)?).into();
        Ok(Self {
            name: token,
            dtype,
            label,
            init_expr,
        })
    }

    pub fn access_label(&self) -> &str {
        self.label.as_ref()
    }

    pub fn get_name(&self) -> &str {
        self.name.get_value()
    }

    fn to_token_loc<T: IntoIterator<Item = AsmToken>>(
        &self,
        it: T,
    ) -> impl Iterator<Item = AsmTokenLoc> {
        it.into_iter().map(|x| self.name.to_asm(x))
    }
}

impl Display for GlobalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

impl Expression for GlobalVariable {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.dtype.clone())
    }

    fn load_value_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        if let Some(p) = self.dtype.primitive_type() {
            let mut vals = options
                .load_label(reg.reg, self.access_label().into())
                .to_vec();
            vals.push(AsmToken::OperationLiteral(Box::new(jib_asm::OpLd::new(
                ArgumentType::new(reg.reg, p),
                reg.reg.into(),
            ))));

            Ok(ExpressionData::new(self.to_token_loc(vals)))
        } else {
            Err(self.name.clone().into_err(format!(
                "unable to load value for {} to register",
                self.dtype
            )))
        }
    }

    fn load_address_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        Ok(ExpressionData::new(self.to_token_loc(
            options.load_label(reg.reg, self.access_label().into()),
        )))
    }
}

#[derive(Debug, Clone)]
pub struct GlobalVariableStatement {
    global_var: Rc<GlobalVariable>,
}

impl Display for GlobalVariableStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "global {}: {}{};",
            self.global_var.name.get_value(),
            self.global_var.dtype,
            self.global_var
                .init_expr
                .as_ref()
                .map(|x| format!(" = {x}"))
                .unwrap_or_default()
        )
    }
}

impl GlobalVariableStatement {
    pub fn new(var: Rc<GlobalVariable>) -> Self {
        Self {
            global_var: var.clone(),
        }
    }

    fn simplified_literal(&self) -> Option<Literal> {
        self.global_var.init_expr.clone().and_then(|x| x.simplify())
    }
}

impl GlobalStatement for GlobalVariableStatement {
    fn get_static_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let name = self.global_var.get_token();
        let var = &self.global_var;
        let mut asm_static = Vec::new();

        asm_static.push(name.to_asm(AsmToken::LocationComment(format!(
                "+gvar({}) : {}{}",
                name,
                self.global_var.dtype,
                self.global_var
                    .dtype
                    .primitive_type()
                    .map(|x| format!(" ({x})"))
                    .unwrap_or_default()
            ))));

        asm_static.push(name.to_asm(AsmToken::CreateLabel(var.access_label().into())));

        if let Some(a) = self.simplified_literal() {
            asm_static.push(
                name.to_asm(
                    a.get_value()
                        .convert(var.get_primitive_type()?)
                        .as_asm_literal(),
                ),
            );
        } else {
            // Create space for the variable
            let needed_size = var.get_type()?.byte_size();
            if needed_size > 0 {
                asm_static.push(name.to_asm(AsmToken::Reserve(needed_size as u32)));
            } else {
                return Err(name
                    .clone()
                    .into_err("unable to create a global variable with 0 size"));
            }
        }

        asm_static.push(name.to_asm(AsmToken::AlignInstruction));

        Ok(asm_static)
    }

    fn get_init_code(
        &self,
        options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = ExpressionData::default();

        if self.simplified_literal().is_none()
            && let Some(init_expr) = &self.global_var.init_expr
        {
            let name = self.global_var.get_token();

            if let Ok(init_type) = init_expr.get_primitive_type() {
                let var = &self.global_var;

                let reg_state_var = RegisterDef::default();
                let reg_state_init = reg_state_var.increment_token(name)?;

                asm.push_asm(name.to_asm(AsmToken::Comment(format!(
                    "Initializing Global Variable {}",
                    self.global_var.get_name()
                ))));

                let mut stack_tracker = TemporaryStackTracker::default();

                let stack_asm: ExpressionData = ExpressionData::merge([
                    var.load_address_to_register(options, reg_state_var, &mut stack_tracker)?,
                    init_expr.load_value_to_register(
                        options,
                        reg_state_init,
                        &mut stack_tracker,
                    )?,
                ]);

                if stack_tracker.max_size > 0 {
                    asm.extend_asm(var.get_token().to_asm_iter(load_to_register(
                        RegisterDef::SPARE,
                        stack_tracker.max_size as u32,
                    )));
                    asm.push_asm(var.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpAdd::new(
                            ArgumentType::new(Register::StackPointer, DataType::U32),
                            Register::StackPointer.into(),
                            RegisterDef::SPARE.into(),
                        ),
                    ))));
                }

                asm.append(stack_asm);

                let reg_init = reg_state_init.reg;
                let reg_var = reg_state_var.reg;

                let var_type = var.get_primitive_type()?;

                if init_type != var_type {
                    asm.push_asm(name.to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                        ArgumentType::new(reg_init, init_type),
                        ArgumentType::new(reg_init, var_type),
                    )))));
                }

                asm.push_asm(name.to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                    ArgumentType::new(reg_var, var_type),
                    reg_init.into(),
                )))));

                if stack_tracker.max_size > 0 {
                    asm.extend_asm(var.get_token().to_asm_iter(load_to_register(
                        RegisterDef::SPARE,
                        stack_tracker.max_size as u32,
                    )));
                    asm.push_asm(var.get_token().to_asm(AsmToken::OperationLiteral(Box::new(
                        OpSub::new(
                            ArgumentType::new(Register::StackPointer, DataType::U32),
                            Register::StackPointer.into(),
                            RegisterDef::SPARE.into(),
                        ),
                    ))));
                }
            } else {
                return Err(name
                    .clone()
                    .into_err("type does not have a valid primitive type"));
            }
        }

        Ok(asm.into_asm())
    }

    fn get_func_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    token: Token,
    dtype: Type,
    base: Register,
    offset: usize,
    init_expr: Option<Rc<dyn Expression>>,
}

impl LocalVariable {
    pub fn new(
        name: Token,
        dtype: Type,
        base: Register,
        offset: usize,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> Result<Self, TokenError> {
        get_identifier(&name)?;
        Ok(Self::new_unchecked(name, dtype, base, offset, init_expr))
    }

    pub fn get_offset(&self) -> usize {
        self.offset
    }

    pub fn new_unchecked(
        name: Token,
        dtype: Type,
        base: Register,
        offset: usize,
        init_expr: Option<Rc<dyn Expression>>,
    ) -> LocalVariable {
        Self {
            token: name,
            dtype,
            base,
            offset,
            init_expr,
        }
    }
}

impl Display for LocalVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.get_value())
    }
}

impl Expression for LocalVariable {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(self.dtype.clone())
    }

    fn load_address_to_register(
        &self,
        _options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        let mut asm = Vec::new();

        if self.offset > 0 {
            asm.extend_from_slice(&load_to_register(reg.reg, self.offset as u32));
            asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(reg.reg, DataType::U32),
                reg.reg.into(),
                self.base.into(),
            ))));
        } else {
            asm.push(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                reg.reg.into(),
                self.base.into(),
            ))))
        }

        Ok(ExpressionData::new(self.token.to_asm_iter(asm)))
    }

    fn load_value_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        if let Some(dt) = self.dtype.primitive_type() {
            let mut asm = self.load_address_to_register(options, reg, required_stack)?;
            asm.push_asm(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpLd::new(
                        ArgumentType::new(reg.reg, dt),
                        reg.reg.into(),
                    )))),
            );
            Ok(asm)
        } else {
            Err(self
                .token
                .clone()
                .into_err("unable to move non-primitive type into register"))
        }
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariableStatement {
    var: Rc<LocalVariable>,
}

impl LocalVariableStatement {
    pub fn new(var: Rc<LocalVariable>) -> Self {
        Self { var }
    }
}

impl Statement for LocalVariableStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = ExpressionData::default();
        let var = self.var.as_ref();

        asm.push_asm(var.token.to_asm(AsmToken::LocationComment(format!(
            "+lvar({}) ${}+{} : {}{}",
            var.token,
            var.base,
            var.offset,
            var.dtype,
            var.dtype.primitive_type().map(|x| format!(" ({x})")).unwrap_or_default()
        ))));

        if let Some(e) = &var.init_expr {
            asm.push_asm(var.token.to_asm(AsmToken::Comment(format!(
                "initializing local variable \"{}\" with offset {} from {}",
                var.token.get_value(),
                var.offset,
                var.base
            ))));

            if let Some(var_type) = var.dtype.primitive_type() {
                let expr_type = e.get_primitive_type()?;

                let def = RegisterDef::default();
                let load_val = def.increment_token(&var.token)?;

                let addr_reg = if var.offset > 0 {
                    asm.append(var.load_address_to_register(options, def, required_stack)?);
                    def.reg
                } else {
                    var.base
                };
                asm.append(e.load_value_to_register(options, load_val, required_stack)?);

                if var_type != expr_type {
                    asm.push_asm(var.token.to_asm(AsmToken::OperationLiteral(Box::new(
                        OpConv::new(
                            ArgumentType::new(load_val.reg, var_type),
                            ArgumentType::new(load_val.reg, expr_type),
                        ),
                    ))));
                }

                asm.push_asm(
                    var.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpSav::new(
                            ArgumentType::new(addr_reg, var_type),
                            load_val.reg.into(),
                        )))),
                );
            } else if let Ok(t) = e.get_type() {
                if t == var.dtype {
                    let def = RegisterDef::default();
                    let load_val = def.increment_token(&var.token)?;

                    let local_reg = if var.offset > 0 {
                        asm.append(var.load_address_to_register(options, def, required_stack)?);
                        def.reg
                    } else {
                        var.base
                    };

                    asm.append(e.load_address_to_register(options, load_val, required_stack)?);

                    let mem = MemcpyStatement::new(
                        var.token.clone(),
                        load_val.reg,
                        local_reg,
                        var.dtype.byte_size(),
                    );

                    asm.extend_asm(mem.get_exec_code(options, required_stack)?);
                } else {
                    return Err(var.token.clone().into_err(format!(
                        "mismatch in data type - found {} != {}",
                        var.dtype, t
                    )));
                }
            } else {
                return Err(var.token.clone().into_err(
                    "unable to obtain a valid type for the provided variable init expression",
                ));
            }
        }

        Ok(asm.into_asm())
    }
}

impl Display for LocalVariableStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "def {}: {}{};",
            self.var.token.get_value(),
            self.var.dtype,
            self.var
                .init_expr
                .as_ref()
                .map(|x| format!(" = {x}"))
                .unwrap_or_default()
        )
    }
}

pub struct VariableDefinition {
    pub token: Token,
    pub dtype: Type,
    pub init_expr: Option<Rc<dyn Expression>>,
}

impl VariableDefinition {
    pub fn parse(
        def_name: &str,
        tokens: &mut TokenIter,
        state: &mut CompilingState,
    ) -> Result<Self, TokenError> {
        tokens.expect(def_name)?;
        let name_token = tokens.next()?;
        tokens.expect(":")?;

        let dtype = Type::read_type(tokens, state)?;
        let init_expr = if tokens.expect_peek("=") {
            tokens.next()?;
            let mut expr_tokens = Vec::new();
            while let Some(t) = tokens.next_if(|s| s != ";") {
                expr_tokens.push(t);
            }
            Some(parse_expression(&mut TokenIter::from(&expr_tokens), state)?)
        } else {
            None
        };

        tokens.expect(";")?;

        Ok(Self {
            token: name_token,
            dtype,
            init_expr,
        })
    }

    pub fn into_literal(self) -> Result<Literal, TokenError> {
        if let Some(expr) = self.init_expr {
            if let Some(lit) = expr.simplify() {
                let dt = match self.dtype.primitive_type() {
                    Some(x) => x,
                    None => {
                        return Err(self
                            .token
                            .into_err("constant must have a primitive data type"));
                    }
                };

                Ok(Literal::new(self.token, lit.get_value().convert(dt)))
            } else {
                Err(self
                    .token
                    .into_err("constant must have a constant expression value"))
            }
        } else {
            Err(self
                .token
                .into_err("constant must have initialization statement"))
        }
    }
}
