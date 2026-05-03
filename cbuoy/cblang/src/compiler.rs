use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque, hash_map::Entry},
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
};

use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, AssemblerErrorLoc, AssemblerOutput, LocationInfo, OpBrk,
    OpCall, OpCopy, OpHalt, OpLd, OpLdn, OpLdno, OpRet, assemble_tokens,
};
use jib_cpu::cpu::{DataType, Register};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef, TemporaryStackTracker},
    functions::{FunctionDeclaration, FunctionDefinition},
    literals::{Literal, StringLiteral},
    tokenizer::{Token, TokenIter, get_identifier, is_identifier, tokenize_str},
    typing::{Function, FunctionParameter, StructDefinition, Type},
    utilities::load_to_register,
    variables::{GlobalVariable, GlobalVariableStatement, LocalVariable, VariableDefinition},
};

pub trait Statement: Debug + Display {
    fn get_exec_code(
        &self,
        options: &CodeGenerationOptions,
        temporary_stack_tracker: &mut TemporaryStackTracker,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

pub trait GlobalStatement: Debug {
    fn get_init_code(
        &self,
        options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;
    fn get_static_code(
        &self,
        options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;
    fn get_func_code(
        &self,
        options: &CodeGenerationOptions,
    ) -> Result<Vec<AsmTokenLoc>, TokenError>;
}

#[derive(Debug, Clone)]
enum GlobalType {
    Variable(Rc<GlobalVariable>),
    FunctionDeclaration(FunctionDeclaration),
    Function(Rc<dyn FunctionDefinition>),
    Constant(Rc<Literal>),
    UserType(Token, UserTypeOptions),
}

impl GlobalType {
    fn get_token(&self) -> &Token {
        match self {
            Self::Variable(v) => v.get_token(),
            Self::Function(v) => v.get_token(),
            Self::FunctionDeclaration(v) => v.get_token(),
            Self::Constant(v) => v.get_token(),
            Self::UserType(t, _) => t,
        }
    }

    fn get_statement(&self) -> Option<Rc<dyn GlobalStatement>> {
        match self {
            Self::Variable(var) => Some(Rc::new(GlobalVariableStatement::new(var.clone()))),
            Self::Function(func) => Some(func.clone()),
            _ => None,
        }
    }
}

impl Display for GlobalType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(v) => write!(f, "var({v})"),
            Self::Function(v) => write!(f, "func({v})"),
            Self::FunctionDeclaration(v) => write!(f, "funcdec({v})"),
            Self::Constant(v) => write!(f, "const({v})"),
            Self::UserType(t, _) => write!(f, "type({t})"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeManager {
    token: Token,
    scopes: Vec<ScopeBlock>,
    max_size: usize,
    parameters: ScopeBlock,
}

impl ScopeManager {
    pub fn new(t: Token) -> Self {
        Self {
            token: t,
            scopes: Vec::new(),
            max_size: 0,
            parameters: ScopeBlock::default(),
        }
    }

    pub fn add_scope(&mut self) {
        self.scopes.push(ScopeBlock::default());
    }

    pub fn remove_scope(&mut self) -> Result<(), TokenError> {
        if self.scopes.pop().is_some() {
            Ok(())
        } else {
            Err(TokenError {
                token: None,
                msg: "already removed all scopes".to_string(),
            })
        }
    }

    pub fn get_max_size(&self) -> usize {
        self.max_size
    }

    fn scope_full_size(&self) -> Result<usize, TokenError> {
        self.scopes.iter().map(|x| x.size()).sum()
    }

    pub fn add_var(&mut self, def: VariableDefinition) -> Result<Rc<LocalVariable>, TokenError> {
        let ident = get_identifier(&def.token)?.to_string();
        let offset = self.scope_full_size()?;

        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => {
                    let var_size = def.dtype.byte_size();
                    let var = Rc::new(LocalVariable::new(
                        def.token,
                        def.dtype,
                        RegisterDef::FN_VAR_BASE,
                        offset,
                        def.init_expr,
                    )?);

                    // Update the scope values and maximum size of the stack
                    e.insert(ScopeVariables::Local(var.clone()));
                    self.max_size = self.max_size.max(offset + var_size);
                    Ok(var)
                }
                Entry::Occupied(_) => Err(def
                    .token
                    .into_err("dupliate variable name exists within the same scope")),
            }
        } else {
            Err(def
                .token
                .into_err("unable to add variable without a scope block"))
        }
    }

    pub fn add_const(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let ident = get_identifier(&def.token)?.to_string();
        if let Some(s) = self.scopes.last_mut() {
            match s.variables.entry(ident) {
                Entry::Vacant(e) => {
                    let literal_expr = Rc::new(def.into_literal()?);
                    e.insert(ScopeVariables::Const(literal_expr.clone()));
                    Ok(())
                }
                Entry::Occupied(_) => Err(def
                    .token
                    .into_err("dupliate variable name exists within the same scope")),
            }
        } else {
            Err(def
                .token
                .into_err("unable to add variable without a scope block"))
        }
    }

    pub fn add_parameter(&mut self, def: FunctionParameter) -> Result<(), TokenError> {
        let token = match &def.name {
            Some(name) => name,
            None => {
                return Err(self
                    .token
                    .clone()
                    .into_err("no token associated with function parameter"));
            }
        };

        if !self.scopes.is_empty() {
            return Err(self
                .token
                .clone()
                .into_err("cannot add a parameter when a scope has already been added!"));
        }

        let ident = get_identifier(token)?.to_string();
        let offset = self.parameters.size()?;

        match self.parameters.variables.entry(ident) {
            Entry::Vacant(e) => {
                let var = Rc::new(LocalVariable::new(
                    token.clone(),
                    def.dtype,
                    Register::ArgumentBase,
                    offset,
                    None,
                )?);

                // Update the scope values and maximum size of the stack
                e.insert(ScopeVariables::Local(var.clone()));
                Ok(())
            }
            Entry::Occupied(_) => Err(token
                .clone()
                .into_err("dupliate variable name exists within the same scope")),
        }
    }

    pub fn get_variable(&self, name: &Token) -> Result<&ScopeVariables, TokenError> {
        let ident = get_identifier(name)?;
        match self.get_variable_name(ident) {
            Some(v) => Ok(v),
            None => Err(name
                .clone()
                .into_err("no variable with provided name found")),
        }
    }

    fn get_variable_name(&self, ident: &str) -> Option<&ScopeVariables> {
        for s in self.scopes.iter().rev() {
            if let Some(var) = s.variables.get(ident) {
                return Some(var);
            }
        }

        if let Some(p) = self.parameters.variables.get(ident) {
            Some(p)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScopeVariables {
    Local(Rc<LocalVariable>),
    Const(Rc<Literal>),
}

impl ScopeVariables {
    fn stack_size(&self) -> Result<usize, TokenError> {
        match self {
            Self::Local(var) => var.get_type().map(|x| x.byte_size()),
            Self::Const(_) => Ok(0),
        }
    }

    fn get_expr(&self) -> Rc<dyn Expression> {
        match self {
            Self::Local(var) => var.clone(),
            Self::Const(lit) => lit.clone(),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct ScopeBlock {
    variables: HashMap<String, ScopeVariables>,
}

impl ScopeBlock {
    pub fn size(&self) -> Result<usize, TokenError> {
        self.variables.values().map(|v| v.stack_size()).sum()
    }
}

#[derive(Debug, Clone)]
pub enum UserTypeOptions {
    OpaqueType(Token),
    ConcreteType(Type),
}

#[derive(Debug, Default, Clone)]
struct UserTypes {
    types: HashMap<String, UserTypeOptions>,
}

#[derive(Clone)]
pub struct UserTypeReference {
    pub name: Token,
    db: Rc<RefCell<UserTypes>>,
}

impl Debug for UserTypeReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UserTypeReference {{ name: {:?} }}", self.name)
    }
}

impl UserTypeReference {
    pub fn get_type(&self, follow_links: bool) -> Result<Type, TokenError> {
        if let Some(ty) = self.db.borrow().types.get(self.name.get_value()) {
            match ty {
                UserTypeOptions::ConcreteType(inner) => Ok(inner.clone()),
                UserTypeOptions::OpaqueType(next) => {
                    if next.get_value() != self.name.get_value() {
                        let user_ref = Self {
                            name: next.clone(),
                            db: self.db.clone(),
                        };

                        user_ref.get_type(follow_links)
                    } else if follow_links {
                        Err(self
                            .name
                            .clone()
                            .into_err("recursive type definition found"))
                    } else {
                        Ok(Type::Opaque(self.clone()))
                    }
                }
            }
        } else {
            Err(self
                .name
                .clone()
                .into_err("unable to find a valid user type for the provided token value"))
        }
    }
}

impl Eq for UserTypeReference {}

impl PartialEq for UserTypeReference {
    fn eq(&self, other: &Self) -> bool {
        self.name.get_value() == other.name.get_value()
    }
}

#[derive(Debug, Clone)]
pub enum ProgramType {
    Application,
    Kernel { stack_loc: u32, start_offset: u32 },
}

impl ProgramType {
    pub const DEFAULT_STACK_LOC: u32 = 0x1000;
    pub const DEFAULT_START_OFFSET: u32 = 0x2000;
}

impl Default for ProgramType {
    fn default() -> Self {
        Self::Kernel {
            stack_loc: Self::DEFAULT_STACK_LOC,
            start_offset: Self::DEFAULT_START_OFFSET,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct CodeGenerationOptions {
    pub prog_type: ProgramType,
    pub debug_locations: bool,
    pub trim_code: bool,
}

impl CodeGenerationOptions {
    pub fn load_label(&self, r: Register, s: String) -> [AsmToken; 2] {
        [self.load_next_label_inst(r), AsmToken::LoadLoc(s)]
    }

    pub fn load_next_label_inst(&self, r: Register) -> AsmToken {
        let arg = ArgumentType::new(r, DataType::U32);
        AsmToken::OperationLiteral(if matches!(self.prog_type, ProgramType::Application) {
            Box::new(OpLdno::new(arg))
        } else {
            Box::new(OpLdn::new(arg))
        })
    }

    pub fn load_label_inst_name(&self) -> String {
        if matches!(self.prog_type, ProgramType::Application) {
            OpLdno::name()
        } else {
            OpLdn::name()
        }
    }
}

#[derive(Debug)]
struct AccessState {
    own: Rc<str>,
    accesses: HashSet<Rc<str>>,
}

impl AccessState {
    pub fn new<T: AsRef<str>>(name: T) -> Self {
        Self {
            own: name.as_ref().into(),
            accesses: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompilerError {
    AssemblerError(AssemblerErrorLoc),
    TokenError(TokenError),
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AssemblerError(e) => write!(f, "Assembler: {e}"),
            Self::TokenError(e) => write!(f, "Token: {e}"),
        }
    }
}

impl From<AssemblerErrorLoc> for CompilerError {
    fn from(value: AssemblerErrorLoc) -> Self {
        Self::AssemblerError(value)
    }
}

impl From<TokenError> for CompilerError {
    fn from(value: TokenError) -> Self {
        Self::TokenError(value)
    }
}

#[derive(Debug, Default)]
pub struct CompilingState {
    statements: Vec<(Rc<str>, Rc<dyn GlobalStatement>)>,
    struct_defs: Vec<(Token, Rc<StructDefinition>)>,
    global_scope: HashMap<String, (Rc<RefCell<AccessState>>, GlobalType)>,
    user_types: Rc<RefCell<UserTypes>>,
    string_literals: RefCell<HashMap<String, Rc<StringLiteral>>>,
    current_id: usize,
    scope_manager: Option<ScopeManager>,
    options: CodeGenerationOptions,
}

impl CompilingState {
    pub const MAIN_FUNC_NAME: &'static str = "main";

    pub fn new(options: CodeGenerationOptions) -> Self {
        Self {
            options,
            ..Default::default()
        }
    }

    fn blank_token_loc(tok: AsmToken) -> AsmTokenLoc {
        AsmTokenLoc {
            tok,
            loc: LocationInfo::default(),
        }
    }

    pub fn get_next_id(&mut self) -> usize {
        let current = self.current_id;
        self.current_id += 1;
        current
    }

    pub fn get_options(&self) -> &CodeGenerationOptions {
        &self.options
    }

    pub fn is_used(&self, name: &str) -> bool {
        if self.options.trim_code {
            let mut dfs: VecDeque<Rc<str>> = vec![Self::MAIN_FUNC_NAME.into()].into();
            let mut visited: HashSet<Rc<str>> = HashSet::new();

            while let Some(r) = dfs.pop_front() {
                if r.as_ref() == name {
                    return true;
                }

                if visited.contains(&r) {
                    continue;
                }

                visited.insert(r.clone());

                if let Some((x, _)) = self.global_scope.get(r.as_ref()) {
                    for x in x.borrow().accesses.iter() {
                        dfs.push_back(x.clone());
                    }
                }
            }

            false
        } else {
            true
        }
    }

    pub fn get_assembler(&self) -> Result<AssemblerOutput, CompilerError> {
        Ok(assemble_tokens(self.get_assembler_tokens()?)?)
    }

    pub fn get_assembler_tokens(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
        fn add_name(asm: &mut Vec<AsmTokenLoc>, name: &str) {
            asm.extend_from_slice(&[
                CompilingState::blank_token_loc(AsmToken::Comment(name.into())),
                CompilingState::blank_token_loc(AsmToken::AlignInstruction),
            ]);
        }

        let init_label = "program_init".to_string();

        let mut asm = if let ProgramType::Kernel {
            stack_loc: _,
            start_offset,
        } = &self.options.prog_type
        {
            vec![Self::blank_token_loc(AsmToken::ChangeAddress(
                *start_offset,
            ))]
        } else {
            Vec::new()
        };

        if self.options.debug_locations {
            for (tok, s) in self.struct_defs.iter() {
                asm.push(tok.to_asm(AsmToken::LocationComment(format!(
                    "+struct({}) {}",
                    tok,
                    s.get_size(),
                ))));

                let mut struct_vals = s
                    .get_fields()
                    .iter()
                    .map(|(k, v)| (v.offset, k, v.dtype.clone()))
                    .collect::<Vec<_>>();
                struct_vals.sort_by_key(|x| x.0);

                for (offset, name, dtype) in struct_vals {
                    asm.push(tok.to_asm(AsmToken::LocationComment(format!(
                        "+field({}) +{} : {}{}",
                        name,
                        offset,
                        dtype,
                        dtype.primitive_type().map(|x| format!(" ({x})")).unwrap_or(String::new()),
                    ))));
                }
            }
        }

        add_name(&mut asm, "Initialization");
        asm.push(Self::blank_token_loc(AsmToken::CreateLabel(
            init_label.clone(),
        )));

        if let ProgramType::Kernel { stack_loc, .. } = &self.options.prog_type {
            asm.extend(
                load_to_register(Register::StackPointer, *stack_loc)
                    .into_iter()
                    .map(Self::blank_token_loc),
            );
        } else {
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpCopy::new(Register::LoadOffset.into(), Register::ProgramCounter.into()),
            ))));
        }

        for (u, s) in self.statements.iter() {
            if self.is_used(u) {
                asm.extend_from_slice(&s.get_init_code(&self.options)?);
            }
        }

        if let Some(GlobalType::Function(f)) = self.get_global(Self::MAIN_FUNC_NAME)? {
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpLd::new(
                    ArgumentType::new(Register::ArgumentBase, DataType::U32),
                    Register::StackPointer.into(),
                ),
            ))));
            asm.extend(
                self.options
                    .load_label(RegisterDef::SPARE, f.get_entry_label().to_owned())
                    .into_iter()
                    .map(Self::blank_token_loc),
            );
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpCall::new(RegisterDef::SPARE.into()),
            ))));
        }

        if matches!(self.options.prog_type, ProgramType::Application) {
            asm.extend(
                [
                    AsmToken::Comment("Program End".into()),
                    AsmToken::OperationLiteral(Box::new(OpRet)),
                ]
                .map(Self::blank_token_loc),
            );
        } else {
            asm.extend(
                [
                    AsmToken::Comment("Program Halt".into()),
                    AsmToken::OperationLiteral(Box::new(OpHalt)),
                ]
                .map(Self::blank_token_loc),
            );
        }

        add_name(&mut asm, "Static");
        for (u, s) in self.statements.iter() {
            if self.is_used(u) {
                asm.extend_from_slice(&s.get_static_code(&self.options)?);
            }
        }

        let string_vals = {
            let mut literals = self
                .string_literals
                .borrow()
                .values()
                .cloned()
                .collect::<Vec<_>>();
            literals.sort_by(|a, b| a.get_token().get_value().cmp(b.get_token().get_value()));
            literals
        };

        for s in string_vals {
            asm.extend_from_slice(&s.get_static_code(&self.options)?);
        }

        add_name(&mut asm, "Functions");
        for (u, s) in self.statements.iter() {
            if self.is_used(u) {
                asm.extend_from_slice(&s.get_func_code(&self.options)?);
            }
        }

        Ok(asm)
    }

    pub fn get_scopes(&self) -> Result<&ScopeManager, TokenError> {
        if let Some(s) = &self.scope_manager {
            Ok(s)
        } else {
            Err(TokenError {
                msg: "no scope initialized".to_string(),
                token: None,
            })
        }
    }

    pub fn get_scopes_mut(&mut self) -> Result<&mut ScopeManager, TokenError> {
        if let Some(s) = &mut self.scope_manager {
            Ok(s)
        } else {
            Err(TokenError {
                msg: "no scope initialized".to_string(),
                token: None,
            })
        }
    }

    pub fn init_scope(&mut self, token: Token) -> Result<(), TokenError> {
        if let Some(s) = &self.scope_manager {
            Err(s.token.clone().into_err("scope has not been cleared"))
        } else {
            self.scope_manager = Some(ScopeManager::new(token));
            Ok(())
        }
    }

    pub fn extract_scope(&mut self) -> Result<ScopeManager, TokenError> {
        if let Some(manager) = self.scope_manager.take() {
            Ok(manager)
        } else {
            Err(TokenError {
                msg: "unable to extract scope from uninitialized vlaues".to_string(),
                token: None,
            })
        }
    }

    pub fn add_user_type(&mut self, name: Token, ty: UserTypeOptions) -> Result<(), TokenError> {
        let ret = match self.global_scope.entry(name.get_value().to_string()) {
            Entry::Occupied(mut e) => {
                let opaque_token = match e.get() {
                    (_, GlobalType::UserType(token, UserTypeOptions::OpaqueType(_))) => {
                        token.clone()
                    }
                    _ => {
                        return Err(name.into_err("unable to find opaque type with provided name"));
                    }
                };

                if matches!(ty, UserTypeOptions::ConcreteType(_)) {
                    if let UserTypeOptions::ConcreteType(Type::Struct(s)) = &ty {
                        self.struct_defs.push((opaque_token.clone(), s.clone()));
                    }
                    *e.get_mut() = (
                        Rc::new(RefCell::new(AccessState::new(get_identifier(
                            &opaque_token,
                        )?))),
                        GlobalType::UserType(opaque_token, ty),
                    );
                    Ok(())
                } else {
                    Err(name.into_err(
                        "provided type is not a concrete type to upgrade opaque type with",
                    ))
                }
            }
            Entry::Vacant(e) => {
                if let UserTypeOptions::ConcreteType(Type::Struct(s)) = &ty {
                    self.struct_defs.push((name.clone(), s.clone()));
                }

                e.insert((
                    Rc::new(RefCell::new(AccessState::new(get_identifier(&name)?))),
                    GlobalType::UserType(name, ty),
                ));
                Ok(())
            }
        };

        self.update_user_types();

        ret
    }

    /// Provides a user type
    pub fn get_user_type(&self, name: &Token) -> Result<UserTypeReference, TokenError> {
        Ok(UserTypeReference {
            name: name.clone(),
            db: self.user_types.clone(),
        })
    }

    /// Adds a new global variable
    pub fn add_global_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let var = Rc::new(GlobalVariable::new(
            def.token,
            self.get_next_id(),
            def.dtype,
            def.init_expr,
        )?);
        self.add_to_global_scope(GlobalType::Variable(var), None)
    }

    /// Adds a type alias for a new type
    pub fn add_type_alias(&mut self, token: Token, alias_type: Type) -> Result<(), TokenError> {
        self.add_to_global_scope(
            GlobalType::UserType(token, UserTypeOptions::ConcreteType(alias_type)),
            None,
        )?;
        self.update_user_types();
        Ok(())
    }

    /// Gets a string literal from the given token. If the literal already exists, re-use the exisitng
    /// entry. Otherwise, create a new literal. This allows entries to share and reduce the overall
    /// usage burden
    pub fn get_string_literal(&self, token: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        match self
            .string_literals
            .borrow_mut()
            .entry(token.get_value().to_string())
        {
            Entry::Occupied(e) => Ok(e.get().clone()),
            Entry::Vacant(e) => {
                let sv = Rc::new(StringLiteral::new(token.clone())?);
                e.insert(sv.clone());
                Ok(sv)
            }
        }
    }

    /// Adds a constant variable to the global scope
    pub fn add_const_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Constant(Rc::new(def.into_literal()?)), None)
    }

    /// Adds a function declaration to the global scope
    pub fn add_function_declaration(
        &mut self,
        func: FunctionDeclaration,
    ) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::FunctionDeclaration(func), None)
    }

    /// Adds a function to the global scope
    pub fn add_function(&mut self, func: Rc<dyn FunctionDefinition>) -> Result<(), TokenError> {
        let access_vals = if let Entry::Occupied(e) = self
            .global_scope
            .entry(func.get_token().get_value().to_string())
        {
            if let (_, GlobalType::FunctionDeclaration(_)) = e.get() {
                Some(e.remove_entry().1.0)
            } else {
                return Err(func.get_token().clone().into_err(format!(
                    "token already exists as {} - cannot add as a function",
                    e.get().1
                )));
            }
        } else {
            None
        };

        self.add_to_global_scope(GlobalType::Function(func), access_vals)
    }

    /// Updates the internal user type database for internal consistency
    fn update_user_types(&mut self) {
        // Take the current user types data and clear
        let mut tv = self.user_types.borrow_mut();
        tv.types.clear();

        // Iterate through the global scope and add new user types as necessary
        for (n, v) in self.global_scope.iter() {
            if let (_, GlobalType::UserType(_, t)) = v {
                tv.types.insert(n.clone(), t.clone());
            }
        }
    }

    /// Adds an entry to the global scope if requested
    fn add_to_global_scope(
        &mut self,
        t: GlobalType,
        previous_val: Option<Rc<RefCell<AccessState>>>,
    ) -> Result<(), TokenError> {
        // Obtain the token and ensure that the name is not used
        let name = get_identifier(t.get_token())?;

        // Check if the name already exists based on the given access state
        let used_val =
            previous_val.unwrap_or_else(|| Rc::new(RefCell::new(AccessState::new(name))));

        assert_eq!(name, used_val.borrow().own.as_ref());

        // Get a new Rc for the name
        let name_rc = used_val.borrow().own.clone();

        // Get the resulting statement from the type, add the type to the
        // global scope, and add the resulting statement to the statement list
        let statement = t.get_statement();
        match self.global_scope.entry(name.to_string()) {
            Entry::Vacant(e) => e.insert((used_val, t)),
            Entry::Occupied(_) => {
                return Err(t
                    .get_token()
                    .clone()
                    .into_err(format!("global scope already contains name \"{name}\"")));
            }
        };

        if let Some(s) = statement {
            self.statements.push((name_rc, s));
        }

        // Update user types for any potential new structures
        self.update_user_types();

        Ok(())
    }

    /// Gets a function declaration with the given name
    pub fn get_function_declaration(
        &self,
        name: &str,
    ) -> Result<Option<&FunctionDeclaration>, TokenError> {
        if let Some(GlobalType::FunctionDeclaration(d)) = self.get_global(name)? {
            Ok(Some(d))
        } else {
            Ok(None)
        }
    }

    /// Gets a generic global for the given name
    fn get_global(&self, name: &str) -> Result<Option<&GlobalType>, TokenError> {
        if let Some((_, value)) = self.global_scope.get(name) {
            if let Some(current) = self.scope_manager.as_ref() {
                let caller = get_identifier(&current.token)?;
                if let Some((access_val, _)) = self.global_scope.get(caller) {
                    access_val.borrow_mut().accesses.insert(name.into());
                } else {
                    panic!(
                        "unable to access a global with the provided entry value - {name} @ {caller}"
                    );
                }
            }
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    /// Provides the variable expression associtated with the given token
    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?.to_string();

        if let Ok(v) = self.get_scopes().and_then(|x| x.get_variable(name)) {
            return Ok(v.get_expr());
        }

        if let Some(value) = self.get_global(&ident)? {
            let res = match value.clone() {
                GlobalType::Variable(v) => Ok(v as Rc<dyn Expression>),
                GlobalType::Constant(v) => Ok(v as Rc<dyn Expression>),
                GlobalType::Function(f) => Ok(f.as_expr()),
                GlobalType::FunctionDeclaration(f) => Ok(f.as_expr()),
                x => Err(x
                    .get_token()
                    .clone()
                    .into_err("global is not a variable type")),
            }?;

            Ok(res)
        } else {
            Err(name
                .clone()
                .into_err("no variable with matching name found"))
        }
    }

    /// Provides the global location label for the given name
    pub fn get_global_location_label(&self, name: &str) -> Option<&str> {
        match self.get_global(name).ok().flatten() {
            Some(GlobalType::Variable(v)) => Some(v.access_label()),
            Some(GlobalType::Function(v)) => Some(v.get_entry_label()),
            _ => None,
        }
    }

    /// Provides the global constant value for the given name
    pub fn get_global_constant(&self, name: &str) -> Option<Rc<Literal>> {
        match self.get_global(name).ok().flatten() {
            Some(GlobalType::Constant(v)) => Some(v.clone()),
            _ => None,
        }
    }

    /// Provides the current local variable offset for the given name
    pub fn get_local_variable_offset(&self, name: &str) -> Option<usize> {
        self.scope_manager.as_ref().and_then(|sm| {
            sm.get_variable_name(name).and_then(|x| match x {
                ScopeVariables::Local(v) => Some(v.get_offset()),
                _ => None,
            })
        })
    }

    /// Provides a structure definition with the given name
    pub fn get_struct_definition(&self, name: &str) -> Option<Rc<StructDefinition>> {
        for (t, s) in &self.struct_defs {
            if t.get_value() == name {
                return Some(s.clone());
            }
        }

        None
    }

    /// Provides the type of the given identifier, if it has a valid type
    pub fn get_identifier_type(&self, name: &str) -> Option<Type> {
        // Check that the name is an identifier
        if is_identifier(name) {
            // Return the type if it is a given value in the scope
            // Otherwise, check for the identifier in the global scope
            if let Some(lv_type) = self.scope_manager.as_ref().and_then(|sm| {
                sm.get_variable_name(name).and_then(|x| match x {
                    ScopeVariables::Local(x) => x.get_type().ok(),
                    _ => None,
                })
            }) {
                return Some(lv_type);
            } else if let Some(g_type) = self.get_global(name).ok().flatten() {
                match g_type {
                    GlobalType::Variable(v) => return v.get_type().ok(),
                    GlobalType::Constant(v) => return v.get_type().ok(),
                    GlobalType::Function(f) => return f.as_expr().get_type().ok(),
                    GlobalType::FunctionDeclaration(f) => return f.as_expr().get_type().ok(),
                    GlobalType::UserType(_, t) => {
                        if let UserTypeOptions::ConcreteType(x) = t {
                            return Some(x.clone());
                        }
                    }
                };
            }
        }

        // Read the type if needed from the name, in case the value is a type definition itself
        if let Ok(iter) = tokenize_str(name) {
            let mut tokens = TokenIter::from(&iter);
            Type::read_type(&mut tokens, self).ok()
        } else {
            None
        }
    }

    /// Provides statements used in the overall program definition, attempting to reconstruct
    /// the AST representation of the program
    pub fn get_statements(&self) -> Vec<String> {
        let mut statements = Vec::default();

        // Add any constant statements
        for (n, i) in self.global_scope.iter() {
            if let (used, GlobalType::Constant(c)) = i
                && self.is_used(&used.borrow().own)
            {
                statements.push(format!("const {n} = {c};"));
            }
        }

        // Add user type statements
        for (_, i) in self.global_scope.iter() {
            if let (used, GlobalType::UserType(_, ut)) = i
                && self.is_used(&used.borrow().own)
            {
                match ut {
                    UserTypeOptions::OpaqueType(s) => {
                        statements.push(format!("struct {};", s.get_value()))
                    }
                    UserTypeOptions::ConcreteType(s) => statements.push(format!("{s};")),
                }
            }
        }

        // Add global variable statements
        for (_, i) in self.global_scope.iter() {
            if let (used, GlobalType::Variable(v)) = i
                && self.is_used(&used.borrow().own)
            {
                statements.push(format!("{};", GlobalVariableStatement::new(v.clone())));
            }
        }

        // Add function statements
        for (_, i) in self.global_scope.iter() {
            if let (used, GlobalType::Function(f)) = i
                && self.is_used(&used.borrow().own)
            {
                statements.push(format!("{f}"))
            }
        }

        statements
    }

    /// Provides the resulting interface for locations within the compiled program
    pub fn get_exported_interface(&self) -> Result<InterfaceDefinition, CompilerError> {
        // Construct the interface and assemble the program
        let mut interface = InterfaceDefinition::default();
        let asm = self.get_assembler()?;

        // Iterate over each item in the global scope
        for (name, (_, global)) in self.global_scope.iter() {
            match global {
                // Extract function labels from the assembled program
                GlobalType::Function(func) => {
                    if let Some(loc) = asm.labels.get(func.get_entry_label()).cloned() {
                        let def = func.get_func_def();
                        interface.functions.push(InterfaceFunction {
                            loc,
                            name: name.clone(),
                            def: def.clone(),
                        });
                    }
                }
                // Add constants to the assembled program
                GlobalType::Constant(lit) => {
                    interface.consts.push(InterfaceConstant {
                        name: name.clone(),
                        value: lit.as_ref().clone(),
                    });
                }
                // Add variables from the assembled program
                GlobalType::Variable(var) => {
                    if let Some(loc) = asm.labels.get(var.access_label()).cloned() {
                        interface.variables.push(InterfaceVariable {
                            name: name.clone(),
                            def: var.get_type()?.clone(),
                            loc,
                        })
                    }
                }
                _ => continue,
            };
        }

        // Sort the resulting constants for consistent ordering (TODO - Add like the structures for definition ordering?)
        interface.consts.sort_by(|a, b| a.name.cmp(&b.name));
        interface.functions.sort_by(|a, b| a.name.cmp(&b.name));
        interface.variables.sort_by(|a, b| a.name.cmp(&b.name));

        // Add structures to the interface in the order that they are defined
        for (token, def) in self.struct_defs.iter() {
            interface.structs.push(InterfaceStruct {
                name: token.get_value().to_string(),
                def: def.as_ref().clone(),
            });
        }

        // Sort the results

        Ok(interface)
    }
}

/// Interface function definition
#[derive(Debug, Clone)]
pub struct InterfaceFunction {
    /// The name of the function
    pub name: String,
    /// The location for the function
    pub loc: u32,
    /// The definition for the funtion
    pub def: Function,
}

/// Interface constant definition
#[derive(Debug, Clone)]
pub struct InterfaceConstant {
    /// The name of the constant
    pub name: String,
    /// The type and value for the constant
    pub value: Literal,
}

/// Interface structure definition
#[derive(Debug, Clone)]
pub struct InterfaceStruct {
    /// The name of the structure type
    pub name: String,
    /// The actual definition for the structure interface
    pub def: StructDefinition,
}

/// Interface variable definition
#[derive(Debug, Clone)]
pub struct InterfaceVariable {
    /// The name of the variable
    pub name: String,
    /// The variable type (actual, no pointers added)
    pub def: Type,
    /// The location of the variable
    pub loc: u32,
}

/// Structure to define overall interface information
#[derive(Default, Debug, Clone)]
pub struct InterfaceDefinition {
    /// List of functions to provide as an interface
    pub functions: Vec<InterfaceFunction>,
    /// List of constants and values to provide as an interface
    pub consts: Vec<InterfaceConstant>,
    /// List of structures to provide as an interface
    pub structs: Vec<InterfaceStruct>,
    /// List of global variables to provide as an interface
    pub variables: Vec<InterfaceVariable>,
}

impl InterfaceDefinition {
    /// Filters only the matching functions into a new instance
    pub fn filter<T: Fn(&str) -> bool>(self, filter_func: T) -> Self {
        Self {
            functions: self
                .functions
                .into_iter()
                .filter(|x| filter_func(&x.name))
                .collect(),
            consts: self
                .consts
                .into_iter()
                .filter(|x| filter_func(&x.name))
                .collect(),
            structs: self
                .structs
                .into_iter()
                .filter(|x| filter_func(&x.name))
                .collect(),
            variables: self
                .variables
                .into_iter()
                .filter(|x| filter_func(&x.name))
                .collect(),
        }
    }

    /// Provides the resulting interface text for
    pub fn write_interface<T: Write>(&self, f: &mut T) -> Result<(), std::io::Error> {
        // Add matching structures
        if !self.structs.is_empty() {
            writeln!(f, "// Structures")?;
            for i in &self.structs {
                writeln!(f, "struct {} {{", i.name)?;

                let mut fields = i.def.get_fields().iter().collect::<Vec<_>>();
                fields.sort_by_key(|(_, x)| x.offset);

                for (name, field) in fields {
                    writeln!(f, "    {}: {};", name, field.dtype)?;
                }

                writeln!(f, "}}")?;
            }
        }

        // Add any matching constants
        if !self.consts.is_empty() {
            writeln!(f, "// Constants")?;
            for c in &self.consts {
                writeln!(
                    f,
                    "global {}: {} = {};",
                    c.name,
                    c.value.get_value().get_dtype(),
                    c.value
                )?;
            }
        }

        // Add any matching global variables
        if !self.variables.is_empty() {
            writeln!(f, "// Variables")?;
            for i in &self.variables {
                writeln!(
                    f,
                    "global {}: {} = {}u32;",
                    i.name,
                    Type::Pointer(Box::new(i.def.clone())),
                    i.loc
                )?;
            }
        }

        // Add the output functions
        if !self.functions.is_empty() {
            writeln!(f, "// Functions").unwrap();
            for i in &self.functions {
                writeln!(
                    f,
                    "global {}: fn({}) {} = {}u32;",
                    i.name,
                    i.def
                        .parameters
                        .iter()
                        .map(|x| x.dtype.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    i.def
                        .return_type
                        .as_ref()
                        .map(|x| x.to_string())
                        .unwrap_or("void".into()),
                    i.loc,
                )?;
            }
        }

        Ok(())
    }
}
