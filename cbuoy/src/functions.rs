use std::{fmt::Display, rc::Rc, sync::LazyLock};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, Instruction, OpAdd, OpBrk, OpConv, OpCopy, OpJmp, OpRet,
    OpRetInt, OpSub, OpTz,
};
use regex::Regex;

use crate::{
    TokenError,
    compiler::{CodeGenerationOptions, CompilingState, GlobalStatement, ScopeManager, Statement},
    expressions::{
        Expression, ExpressionData, RegisterDef, TemporaryStackTracker, parse_expression,
    },
    literals::StringLiteral,
    tokenizer::{
        EndOfTokenStream, KEYWORD_ASMFN, KEYWORD_BREAK, KEYWORD_CONST, KEYWORD_DBG_BREAK,
        KEYWORD_DEF, KEYWORD_ELSE, KEYWORD_FN, KEYWORD_FNINT, KEYWORD_IF, KEYWORD_RETURN,
        KEYWORD_WHILE, Token, TokenIter, get_identifier,
    },
    typing::{Function, Type},
    utilities::load_to_register,
    variables::{LocalVariable, LocalVariableStatement, VariableDefinition},
};

pub trait FunctionDefinition: Display + GlobalStatement {
    fn get_token(&self) -> &Token;
    fn as_expr(&self) -> Rc<dyn Expression>;
    fn get_entry_label(&self) -> &str;
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub enum StandardFunctionType {
    #[default]
    Default,
    Interrupt,
}

impl StandardFunctionType {
    fn keyword(&self) -> &str {
        match self {
            Self::Default => KEYWORD_FN,
            Self::Interrupt => KEYWORD_FNINT,
        }
    }
}

#[derive(Debug)]
pub struct StandardFunctionDefinition {
    declaration: FunctionDeclaration,
    statements: Vec<Rc<dyn Statement>>,
    scope_manager: ScopeManager,
}

impl StandardFunctionDefinition {
    pub fn new(
        declaration: FunctionDeclaration,
        statements: Vec<Rc<dyn Statement>>,
        scope_manager: ScopeManager,
    ) -> Result<Self, TokenError> {
        Ok(Self {
            declaration,
            statements,
            scope_manager,
        })
    }

    pub fn parse(
        tokens: &mut TokenIter,
        state: &mut CompilingState,
        func_type: StandardFunctionType,
    ) -> Result<(), TokenError> {
        tokens.expect(func_type.keyword())?;
        let name_token = tokens.next()?;
        let ident = get_identifier(&name_token)?;

        state.init_scope(name_token.clone())?;

        let dtype = Function::read_tokens(tokens, state, true)?;

        for p in dtype.parameters.iter() {
            state.get_scopes_mut()?.add_parameter(p.clone())?;
        }

        state.get_scopes_mut()?.add_scope();
        tokens.expect("{")?;

        let mut statements = Vec::new();

        let base_label = format!(
            "___{}_{}_{}",
            func_type.keyword(),
            state.get_next_id(),
            ident
        );

        let declaration = FunctionDeclaration::new(name_token, base_label, dtype, func_type);

        state.add_function_declaration(declaration.clone())?;

        while let Some(s) = parse_statement(
            tokens,
            state,
            &declaration.end_label,
            None,
            declaration.dtype.return_type.as_ref(),
        )? {
            statements.push(s);
        }

        tokens.expect("}")?;

        let def = Rc::new(StandardFunctionDefinition::new(
            declaration,
            statements,
            state.extract_scope()?,
        )?);
        state.add_function(def.clone())
    }
}

impl FunctionDefinition for StandardFunctionDefinition {
    fn get_entry_label(&self) -> &str {
        self.declaration.get_entry_label()
    }

    fn get_token(&self) -> &Token {
        self.declaration.get_token()
    }

    fn as_expr(&self) -> Rc<dyn Expression> {
        self.declaration.as_expr()
    }
}

impl GlobalStatement for StandardFunctionDefinition {
    fn get_init_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_static_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }

    fn get_func_code(
        &self,
        options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut init_asm = vec![
            AsmToken::CreateLabel(self.declaration.entry_label.clone()),
            AsmToken::LocationComment(format!(
                "+{}({})",
                self.declaration.func_type.keyword(),
                self.declaration.name
            )),
            AsmToken::OperationLiteral(Box::new(OpCopy::new(
                RegisterDef::FN_VAR_BASE.into(),
                Register::StackPointer.into(),
            ))),
        ];

        let scope_size = self.scope_manager.get_max_size();

        let mut stack_requirements = TemporaryStackTracker::default();
        let mut statement_asm = Vec::new();

        for s in self.statements.iter() {
            let mut local_stack = TemporaryStackTracker::default();
            statement_asm.push(
                self.declaration
                    .name
                    .to_asm(AsmToken::LocationComment(format!("{s}"))),
            );
            statement_asm.extend(s.get_exec_code(options, &mut local_stack)?);
            stack_requirements.merge(local_stack);
        }

        let total_stack_size = scope_size + stack_requirements.max_size;

        if total_stack_size > 0 {
            init_asm.extend(load_to_register(
                RegisterDef::SPARE,
                total_stack_size as u32,
            ));
            init_asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                RegisterDef::SPARE.into(),
            ))));
        }

        if stack_requirements.max_size > 0 {
            init_asm.push(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                RegisterDef::FN_TEMPVAR_BASE.into(),
                RegisterDef::FN_VAR_BASE.into(),
            ))));

            if scope_size > 0 {
                init_asm.extend(load_to_register(RegisterDef::SPARE, scope_size as u32));
                init_asm.push(AsmToken::OperationLiteral(Box::new(OpAdd::new(
                    ArgumentType::new(RegisterDef::FN_TEMPVAR_BASE, DataType::U32),
                    RegisterDef::FN_TEMPVAR_BASE.into(),
                    RegisterDef::SPARE.into(),
                ))));
            }
        }

        let mut asm = self
            .declaration
            .name
            .to_asm_iter(init_asm)
            .into_iter()
            .collect::<Vec<_>>();

        asm.extend(statement_asm);

        let mut asm_end = Vec::new();

        asm_end.push(AsmToken::CreateLabel(self.declaration.end_label.clone()));
        if total_stack_size > 0 {
            asm_end.extend(load_to_register(
                RegisterDef::SPARE,
                total_stack_size as u32,
            ));
            asm_end.push(AsmToken::OperationLiteral(Box::new(OpSub::new(
                ArgumentType::new(Register::StackPointer, DataType::U32),
                Register::StackPointer.into(),
                RegisterDef::SPARE.into(),
            ))));
        }

        let op: Box<dyn Instruction> = match self.declaration.func_type {
            StandardFunctionType::Default => Box::new(OpRet),
            StandardFunctionType::Interrupt => Box::new(OpRetInt),
        };

        asm_end.push(AsmToken::OperationLiteral(op));
        asm_end.push(AsmToken::LocationComment(format!(
            "-{}({})",
            self.declaration.func_type.keyword(),
            self.declaration.name
        )));

        asm.extend(self.declaration.name.to_asm_iter(asm_end));

        Ok(asm)
    }
}

impl Display for StandardFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.declaration)?;

        for s in self.statements.iter() {
            writeln!(f, "    {s}")?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: Token,
    entry_label: String,
    end_label: String,
    dtype: Function,
    func_type: StandardFunctionType,
}

impl FunctionDeclaration {
    fn new(
        name: Token,
        label_base: String,
        dtype: Function,
        func_type: StandardFunctionType,
    ) -> Self {
        Self {
            name,
            entry_label: format!("{label_base}_start"),
            end_label: format!("{label_base}_end"),
            dtype,
            func_type,
        }
    }

    fn get_entry_label(&self) -> &str {
        &self.entry_label
    }

    pub fn get_token(&self) -> &Token {
        &self.name
    }

    pub fn as_expr(&self) -> Rc<dyn Expression> {
        Rc::new(FunctionLabelExpr {
            name: self.name.clone(),
            dtype: self.dtype.clone(),
            entry_label: self.entry_label.clone(),
        })
    }
}

impl Display for FunctionDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} {}({}) {};",
            match self.func_type {
                StandardFunctionType::Default => "fn",
                StandardFunctionType::Interrupt => "fnint",
            },
            self.name.get_value(),
            self.dtype
                .parameters
                .iter()
                .map(|x| format!(
                    "{}: {}",
                    x.name
                        .as_ref()
                        .map(|x| x.get_value().to_string())
                        .unwrap_or("?".to_string()),
                    x.dtype
                ))
                .collect::<Vec<String>>()
                .join(", "),
            self.dtype
                .return_type
                .as_ref()
                .map(|x| format!("{x}"))
                .unwrap_or("void".to_string())
        )
    }
}

#[derive(Debug)]
pub struct AsmFunctionDefinition {
    name: Token,
    entry_label: String,
    dtype: Function,
    asm_text: Vec<Token>,
}

impl AsmFunctionDefinition {
    pub fn parse(tokens: &mut TokenIter, state: &mut CompilingState) -> Result<(), TokenError> {
        tokens.expect(KEYWORD_ASMFN)?;
        let name_token = tokens.next()?;
        let name = get_identifier(&name_token)?;
        state.init_scope(name_token.clone())?;

        let func_type = Function::read_tokens(tokens, state, true)?;
        for p in func_type.parameters.iter() {
            state.get_scopes_mut()?.add_parameter(p.clone())?;
        }

        tokens.expect("{")?;

        let mut statements = Vec::new();
        loop {
            let t = tokens.next()?;
            if t.get_value() == "}" {
                break;
            } else if let Some(token_text) = StringLiteral::get_quoted_text(t.get_value()) {
                struct MatchFunctionValue {
                    name: &'static str,
                    matcher: Regex,
                    match_func: fn(&CompilingState, &str) -> Option<String>,
                }
                impl MatchFunctionValue {
                    fn new(
                        name: &'static str,
                        bounding_char: &str,
                        match_func: fn(&CompilingState, &str) -> Option<String>,
                    ) -> Self {
                        Self {
                            name,
                            match_func,
                            matcher: Regex::new(&format!(
                                "{bounding_char}\\{{(?<name>[\\w\\d_]+)\\}}{bounding_char}"
                            ))
                            .unwrap(),
                        }
                    }
                }
                unsafe impl Send for MatchFunctionValue {}
                unsafe impl Sync for MatchFunctionValue {}

                static MATCHES: LazyLock<[MatchFunctionValue; 5]> = LazyLock::new(|| {
                    [
                        MatchFunctionValue::new("global_loc", "%", |state, name| {
                            state.get_global_location_label(name).map(|x| x.to_string())
                        }),
                        MatchFunctionValue::new("global_lit", "\\^", |state, name| {
                            state
                                .get_global_constant(name)
                                .map(|x| x.get_value().get_asm_string())
                        }),
                        MatchFunctionValue::new("local_var", "@", |state, name| {
                            state.get_local_variable_offset(name).map(|x| x.to_string())
                        }),
                        MatchFunctionValue::new("struct_offset", "&", |_state_, _name| {
                            todo!("dtype.field offsets")
                        }),
                        MatchFunctionValue::new("sizeof", "#", |state, name| {
                            state
                                .get_identifier_type(name)
                                .map(|t| format!("{}", t.byte_size()))
                        }),
                    ]
                });

                let mut replacement_labels = Vec::new();

                for mg in MATCHES.iter() {
                    for c in mg.matcher.captures_iter(token_text) {
                        let (haystack, [name]) = c.extract();
                        if let Some(label) = (mg.match_func)(state, name) {
                            replacement_labels.push((haystack.to_string(), label.to_string()));
                        } else {
                            return Err(t.into_err(format!("unknown {} access", mg.name)));
                        }
                    }
                }

                let mut tok_str = t.get_value().to_string();
                for (src, replacement) in replacement_labels {
                    tok_str = tok_str.replace(&src, &replacement);
                }
                let t = Token::new(&tok_str, *t.get_loc());

                tokens.expect(";")?;
                statements.push(t);
            } else {
                return Err(
                    t.into_err("unable to convert into a string literal for the asmfn context")
                );
            }
        }

        let entry_label = format!("__{KEYWORD_ASMFN}_{}_{name}", state.get_next_id());

        let func = Rc::new(AsmFunctionDefinition {
            name: name_token,
            entry_label,
            dtype: func_type,
            asm_text: statements,
        });

        state.extract_scope()?;
        state.add_function(func)
    }
}

impl FunctionDefinition for AsmFunctionDefinition {
    fn get_entry_label(&self) -> &str {
        &self.entry_label
    }

    fn get_token(&self) -> &Token {
        &self.name
    }

    fn as_expr(&self) -> Rc<dyn Expression> {
        Rc::new(FunctionLabelExpr {
            name: self.name.clone(),
            dtype: self.dtype.clone(),
            entry_label: self.entry_label.clone(),
        })
    }
}

impl GlobalStatement for AsmFunctionDefinition {
    fn get_func_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut vals = vec![
            self.name
                .to_asm(AsmToken::CreateLabel(self.entry_label.to_string())),
        ];
        for t in self.asm_text.iter() {
            let s = match StringLiteral::get_quoted_text(t.get_value()) {
                Some(txt) => txt,
                None => {
                    return Err(t
                        .clone()
                        .into_err("unable to get a string literal from value"));
                }
            };

            match AsmToken::try_from(s) {
                Ok(asm) => vals.push(t.to_asm(asm)),
                Err(e) => {
                    return Err(t.clone().into_err(format!(
                        "unable to convert '{t}' into a valid assembly - {e}"
                    )));
                }
            };
        }
        Ok(vals)
    }

    fn get_init_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::default())
    }

    fn get_static_code(
        &self,
        _options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::default())
    }
}

impl Display for AsmFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "asmfn {}({}) {} {{",
            self.name.get_value(),
            self.dtype
                .parameters
                .iter()
                .map(|x| format!(
                    "{}: {}",
                    x.name
                        .as_ref()
                        .map(|x| x.get_value().to_string())
                        .unwrap_or("?".to_string()),
                    x.dtype
                ))
                .collect::<Vec<String>>()
                .join(", "),
            self.dtype
                .return_type
                .as_ref()
                .map(|x| format!("{x}"))
                .unwrap_or("void".to_string())
        )?;

        for s in self.asm_text.iter() {
            writeln!(f, "  {};", s.get_value())?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone)]
struct FunctionLabelExpr {
    name: Token,
    dtype: Function,
    entry_label: String,
}

impl Expression for FunctionLabelExpr {
    fn get_token(&self) -> &Token {
        &self.name
    }

    fn get_type(&self) -> Result<Type, TokenError> {
        Ok(Type::Function(Rc::new(self.dtype.clone())))
    }

    fn load_address_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        Ok(ExpressionData::new(self.name.to_asm_iter(
            options.load_label(reg.reg, self.entry_label.clone()),
        )))
    }

    fn load_value_to_register(
        &self,
        options: &CodeGenerationOptions,
        reg: RegisterDef,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<ExpressionData, TokenError> {
        self.load_address_to_register(options, reg, required_stack)
    }
}

impl Display for FunctionLabelExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.get_value())
    }
}

#[derive(Debug, Clone)]
struct StatementGroup {
    token: Token,
    statements: Vec<Rc<dyn Statement>>,
}

impl Statement for StatementGroup {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();
        for s in self.statements.iter() {
            let mut local_stack = TemporaryStackTracker::default();
            asm.push(self.token.to_asm(AsmToken::LocationComment(format!("{s}"))));
            asm.extend(s.get_exec_code(options, &mut local_stack)?.into_iter());
            required_stack.merge(local_stack);
        }
        Ok(asm)
    }
}

impl Display for StatementGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for s in self.statements.iter() {
            write!(f, " {s};")?;
        }
        write!(f, " }}")
    }
}

#[derive(Debug, Clone)]
struct IfStatement {
    token: Token,
    id: usize,
    test_expr: Rc<dyn Expression>,
    true_statement: Rc<dyn Statement>,
    false_statement: Option<Rc<dyn Statement>>,
}

impl Statement for IfStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let def = RegisterDef::default();

        let label_base = format!("___{KEYWORD_IF}_statement_{}", self.id);

        let mut asm = vec![self.token.to_asm(AsmToken::Comment(format!(
            "{KEYWORD_IF} statement for {} {}",
            self.id, self.token,
        )))];

        asm.extend(
            self.test_expr
                .load_value_to_register(options, def, required_stack)?
                .into_asm(),
        );

        let false_label = format!("{label_base}_false");

        asm.extend(
            self.token
                .to_asm_iter(options.load_label(def.spare, false_label.clone())),
        );
        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        let mut true_stack = TemporaryStackTracker::default();
        asm.extend(
            self.true_statement
                .get_exec_code(options, &mut true_stack)?,
        );
        required_stack.merge(true_stack);

        let end_label = format!("{label_base}_end");
        if self.false_statement.is_some() {
            asm.extend(
                self.token
                    .to_asm_iter(options.load_label(def.reg, end_label.clone())),
            );
            asm.push(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                        def.reg.into(),
                    )))),
            );
        }

        asm.push(
            self.token
                .to_asm(AsmToken::CreateLabel(format!("{label_base}_false"))),
        );

        if let Some(fe) = &self.false_statement {
            let mut false_stack = TemporaryStackTracker::default();
            asm.extend(fe.get_exec_code(options, &mut false_stack)?);
            asm.push(self.token.to_asm(AsmToken::CreateLabel(end_label)));
            required_stack.merge(false_stack);
        }

        Ok(asm)
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{KEYWORD_IF} ({}) {{ {} }}",
            self.test_expr, self.true_statement
        )?;

        if let Some(st) = &self.false_statement {
            write!(f, " {KEYWORD_ELSE} {{ {st} }}")?;
        }

        write!(f, ";")
    }
}

#[derive(Debug, Clone)]
struct WhileStatement {
    token: Token,
    id: usize,
    test_expr: Rc<dyn Expression>,
    statement: Rc<dyn Statement>,
}

impl WhileStatement {
    fn base_label_fmt(id: usize) -> String {
        format!("___{KEYWORD_WHILE}_statement_{}", id)
    }

    pub fn end_label_fmt(id: usize) -> String {
        format!("{}_end", Self::base_label_fmt(id))
    }
}

impl Statement for WhileStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let def = RegisterDef::default();

        let label_base = Self::base_label_fmt(self.id);
        let test_label = format!("{label_base}_test");
        let end_label = Self::end_label_fmt(self.id);

        let mut asm = vec![
            self.token.to_asm(AsmToken::Comment(format!(
                "while statement for {} {}",
                self.id, self.token,
            ))),
            self.token.to_asm(AsmToken::CreateLabel(test_label.clone())),
        ];

        asm.push(self.token.to_asm(AsmToken::Comment(format!(
            "{KEYWORD_WHILE} test using {}",
            self.test_expr
        ))));
        asm.extend(
            self.test_expr
                .load_value_to_register(options, def, required_stack)?
                .into_asm(),
        );

        asm.extend(
            self.token
                .to_asm_iter(options.load_label(def.spare, end_label.clone())),
        );
        asm.extend(self.token.to_asm_iter([
            AsmToken::OperationLiteral(Box::new(OpTz::new(def.reg.into()))),
            AsmToken::OperationLiteral(Box::new(OpJmp::new(def.spare.into()))),
        ]));

        let mut statement_stack = TemporaryStackTracker::default();
        asm.extend(
            self.statement
                .get_exec_code(options, &mut statement_stack)?,
        );
        required_stack.merge(statement_stack);

        asm.extend(
            self.token
                .to_asm_iter(options.load_label(def.spare, test_label)),
        );
        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                    def.spare.into(),
                )))),
        );

        asm.push(self.token.to_asm(AsmToken::CreateLabel(end_label)));

        Ok(asm)
    }
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{KEYWORD_WHILE} ({}) {{ {} }}",
            self.test_expr, self.statement
        )
    }
}

#[derive(Default, Debug, Clone)]
struct EmptyStatement;

impl Statement for EmptyStatement {
    fn get_exec_code(
        &self,
        _options: &CodeGenerationOptions,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(Vec::new())
    }
}

impl Display for EmptyStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{NOOP}}")
    }
}

#[derive(Debug, Clone)]
struct ExpressionStatement {
    expr: Rc<dyn Expression>,
}

impl Statement for ExpressionStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(self
            .expr
            .load_value_to_register(options, RegisterDef::default(), required_stack)?
            .into_asm())
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}

#[derive(Debug, Clone)]
struct DebugBreakpointStatement {
    token: Token,
}

impl Statement for DebugBreakpointStatement {
    fn get_exec_code(
        &self,
        _options: &CodeGenerationOptions,
        _required_stack: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        Ok(vec![
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpBrk))),
        ])
    }
}

impl Display for DebugBreakpointStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{KEYWORD_DBG_BREAK}")
    }
}

#[derive(Debug, Clone)]
struct ReturnStatementTempVar {
    token: Token,
    temp_var: LocalVariable,
    jump_label: String,
}

impl Statement for ReturnStatementTempVar {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = LocalVariableStatement::new(Rc::new(self.temp_var.clone()))
            .get_exec_code(options, temporary_stack_tracker)?;

        asm.extend(
            self.token
                .to_asm_iter(options.load_label(RegisterDef::SPARE, self.jump_label.clone())),
        );
        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                    RegisterDef::SPARE.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for ReturnStatementTempVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{KEYWORD_RETURN} {};", self.temp_var)
    }
}

#[derive(Debug, Clone)]
struct ReturnStatementRegVar {
    token: Token,
    jump_label: String,
    expr: Option<(DataType, Rc<dyn Expression>)>,
}

impl Statement for ReturnStatementRegVar {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let mut asm = Vec::new();

        asm.push(
            self.token.to_asm(AsmToken::Comment(
                self.expr
                    .as_ref()
                    .map(|(dt, e)| format!("{KEYWORD_RETURN} ({e}) : {dt}"))
                    .unwrap_or(KEYWORD_RETURN.to_string()),
            )),
        );

        if let Some((ret_type, e)) = &self.expr {
            let expr_type = e.get_primitive_type()?;
            let rd = RegisterDef::default();
            asm.extend(
                e.load_value_to_register(options, rd, temporary_stack_tracker)?
                    .into_asm(),
            );

            asm.push(
                self.token
                    .to_asm(AsmToken::OperationLiteral(Box::new(OpCopy::new(
                        Register::Return.into(),
                        rd.reg.into(),
                    )))),
            );

            if expr_type != *ret_type {
                asm.push(
                    self.token
                        .to_asm(AsmToken::OperationLiteral(Box::new(OpConv::new(
                            ArgumentType::new(Register::Return, *ret_type),
                            ArgumentType::new(Register::Return, expr_type),
                        )))),
                );
            }
        }

        asm.extend(
            self.token
                .to_asm_iter(options.load_label(RegisterDef::SPARE, self.jump_label.clone())),
        );
        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                    RegisterDef::SPARE.into(),
                )))),
        );

        Ok(asm)
    }
}

impl Display for ReturnStatementRegVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((_, r)) = &self.expr {
            write!(f, "{KEYWORD_RETURN} {r};")
        } else {
            write!(f, "{KEYWORD_RETURN};")
        }
    }
}

#[derive(Debug, Clone)]
struct BreakStatement {
    token: Token,
    label: String,
}

impl Statement for BreakStatement {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        _temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError> {
        let rd = RegisterDef::default();
        let mut asm = Vec::new();
        asm.push(self.token.to_asm(AsmToken::Comment(format!(
            "{KEYWORD_BREAK} to {}",
            self.label
        ))));
        asm.extend(
            self.token
                .to_asm_iter(options.load_label(rd.reg, self.label.clone())),
        );
        asm.push(
            self.token
                .to_asm(AsmToken::OperationLiteral(Box::new(OpJmp::new(
                    rd.reg.into(),
                )))),
        );
        Ok(asm)
    }
}

impl Display for BreakStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{KEYWORD_BREAK};")
    }
}

fn parse_statement(
    tokens: &mut TokenIter,
    state: &mut CompilingState,
    return_jump_label: &str,
    break_jump_label: Option<&str>,
    return_type: Option<&Type>,
) -> Result<Option<Rc<dyn Statement>>, TokenError> {
    if let Some(next) = tokens.peek() {
        if next.get_value() == KEYWORD_DEF {
            let def = VariableDefinition::parse(KEYWORD_DEF, tokens, state)?;
            Ok(Some(Rc::new(LocalVariableStatement::new(
                state.get_scopes_mut()?.add_var(def)?,
            ))))
        } else if next.get_value() == KEYWORD_CONST {
            let def = VariableDefinition::parse(KEYWORD_CONST, tokens, state)?;
            state.get_scopes_mut()?.add_const(def)?;
            Ok(Some(Rc::new(EmptyStatement)))
        } else if next.get_value() == "{" {
            let init_tok = tokens.expect("{")?;
            state.get_scopes_mut()?.add_scope();
            let mut statements = Vec::new();
            while let Some(s) = parse_statement(
                tokens,
                state,
                return_jump_label,
                break_jump_label,
                return_type,
            )? {
                statements.push(s);
            }
            tokens.expect("}")?;
            state.get_scopes_mut()?.remove_scope()?;
            Ok(Some(Rc::new(StatementGroup {
                token: init_tok,
                statements,
            })))
        } else if next.get_value() == KEYWORD_IF {
            let if_token = tokens.expect(KEYWORD_IF)?;
            let id = state.get_next_id();
            tokens.expect("(")?;
            let test_expr = parse_expression(tokens, state)?;
            tokens.expect(")")?;

            let true_statement = match parse_statement(
                tokens,
                state,
                return_jump_label,
                break_jump_label,
                return_type,
            )? {
                Some(s) => s,
                None => {
                    return Err(
                        if_token.into_err("must have a valid statement after if expression")
                    );
                }
            };

            let false_statement = if tokens.expect_peek(KEYWORD_ELSE) {
                tokens.expect(KEYWORD_ELSE)?;
                parse_statement(
                    tokens,
                    state,
                    return_jump_label,
                    break_jump_label,
                    return_type,
                )?
            } else {
                None
            };

            Ok(Some(Rc::new(IfStatement {
                token: if_token,
                id,
                test_expr,
                true_statement,
                false_statement,
            })))
        } else if next.get_value() == KEYWORD_WHILE {
            let while_token = tokens.expect(KEYWORD_WHILE)?;
            let id = state.get_next_id();
            tokens.expect("(")?;
            let test_expr = parse_expression(tokens, state)?;
            tokens.expect(")")?;

            let while_end = format!("___{KEYWORD_WHILE}_statement_{id}_end");
            let while_statement_end_label = Some(while_end.as_str());

            let statement = match parse_statement(
                tokens,
                state,
                return_jump_label,
                while_statement_end_label,
                return_type,
            )? {
                Some(s) => s,
                None => {
                    return Err(while_token.into_err("while statement must have a valid statement"));
                }
            };

            Ok(Some(Rc::new(WhileStatement {
                token: while_token,
                id,
                test_expr,
                statement,
            })))
        } else if next.get_value() == KEYWORD_BREAK {
            let tok = tokens.expect(KEYWORD_BREAK)?;
            tokens.expect(";")?;

            if let Some(lbl) = break_jump_label {
                Ok(Some(Rc::new(BreakStatement {
                    token: tok,
                    label: lbl.to_string(),
                })))
            } else {
                Err(tok.into_err("cannot break outside of a while loop"))
            }
        } else if next.get_value() == KEYWORD_DBG_BREAK {
            let tok = tokens.expect(KEYWORD_DBG_BREAK)?;
            tokens.expect(";")?;
            Ok(Some(Rc::new(DebugBreakpointStatement { token: tok })))
        } else if next.get_value() == KEYWORD_RETURN {
            let tok = tokens.expect(KEYWORD_RETURN)?;

            let ret_statement: Rc<dyn Statement> = if let Some(rt) = return_type {
                if let Some(pt) = rt.primitive_type() {
                    Rc::new(ReturnStatementRegVar {
                        token: tok,
                        jump_label: return_jump_label.to_string(),
                        expr: Some((pt, parse_expression(tokens, state)?)),
                    })
                } else {
                    let temp_var = LocalVariable::new_unchecked(
                        tok.clone(),
                        rt.clone(),
                        Register::Return,
                        0,
                        Some(parse_expression(tokens, state)?),
                    );
                    Rc::new(ReturnStatementTempVar {
                        jump_label: return_jump_label.to_string(),
                        token: tok,
                        temp_var,
                    })
                }
            } else {
                Rc::new(ReturnStatementRegVar {
                    token: tok,
                    jump_label: return_jump_label.to_string(),
                    expr: None,
                })
            };

            tokens.expect(";")?;
            Ok(Some(ret_statement))
        } else if next.get_value() == "}" {
            Ok(None)
        } else if next.get_value() == ";" {
            tokens.expect(";")?;
            Ok(Some(Rc::new(EmptyStatement)))
        } else {
            let expr = parse_expression(tokens, state)?;
            tokens.expect(";")?;
            Ok(Some(Rc::new(ExpressionStatement { expr })))
        }
    } else {
        Err(EndOfTokenStream.into())
    }
}
