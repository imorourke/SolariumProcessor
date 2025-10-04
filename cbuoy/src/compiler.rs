use std::{
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    fmt::{Debug, Display},
    rc::Rc,
};

use jib::cpu::{DataType, Register};
use jib_asm::{
    ArgumentType, AsmToken, AsmTokenLoc, LocationInfo, OpCall, OpCopy, OpHalt, OpLdn, OpLdno, OpRet,
};

use crate::{
    TokenError,
    expressions::{Expression, RegisterDef, TemporaryStackTracker},
    functions::{FunctionDeclaration, FunctionDefinition},
    literals::{Literal, StringLiteral},
    tokenizer::{Token, TokenIter, get_identifier, is_identifier, tokenize_str},
    typing::{FunctionParameter, StructDefinition, Type},
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

#[derive(Debug, Clone)]
pub struct UserTypeReference {
    pub name: Token,
    db: Rc<RefCell<UserTypes>>,
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

impl Default for ProgramType {
    fn default() -> Self {
        Self::Kernel {
            stack_loc: 0x1000,
            start_offset: 0x2000,
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct CodeGenerationOptions {
    pub prog_type: ProgramType,
    pub debug_locations: bool,
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

#[derive(Debug, Default, Clone)]
pub struct CompilingState {
    statements: Vec<Rc<dyn GlobalStatement>>,
    struct_defs: Vec<(Token, Rc<StructDefinition>)>,
    global_scope: HashMap<String, GlobalType>,
    user_types: Rc<RefCell<UserTypes>>,
    string_literals: RefCell<HashMap<String, Rc<StringLiteral>>>,
    current_id: usize,
    scope_manager: Option<ScopeManager>,
    options: CodeGenerationOptions,
}

impl CompilingState {
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

    pub fn get_assembler(&self) -> Result<Vec<AsmTokenLoc>, TokenError> {
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
            vec![
                Self::blank_token_loc(AsmToken::LoadLoc(init_label.clone())),
                Self::blank_token_loc(AsmToken::LoadLoc(init_label.clone())),
                Self::blank_token_loc(AsmToken::ChangeAddress(*start_offset)),
            ]
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
                struct_vals.sort_by(|a, b| a.0.cmp(&b.0));

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
        }

        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_init_code(&self.options)?);
        }

        if let Some(GlobalType::Function(f)) = self.global_scope.get("main") {
            asm.push(Self::blank_token_loc(AsmToken::OperationLiteral(Box::new(
                OpCopy::new(Register::ArgumentBase.into(), Register::StackPointer.into()),
            ))));
            asm.extend(
                self.options
                    .load_label(RegisterDef::SPARE, f.get_entry_label().to_string())
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
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_static_code(&self.options)?);
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
        for s in self.statements.iter() {
            asm.extend_from_slice(&s.get_func_code(&self.options)?);
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
                    GlobalType::UserType(token, UserTypeOptions::OpaqueType(_)) => token.clone(),
                    _ => {
                        return Err(name.into_err("unable to find opaque type with provided name"));
                    }
                };

                if matches!(ty, UserTypeOptions::ConcreteType(_)) {
                    if let UserTypeOptions::ConcreteType(Type::Struct(s)) = &ty {
                        self.struct_defs.push((opaque_token.clone(), s.clone()));
                    }
                    *e.get_mut() = GlobalType::UserType(opaque_token, ty);
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
                e.insert(GlobalType::UserType(name, ty));
                Ok(())
            }
        };

        self.update_user_types();

        ret
    }

    pub fn get_user_type(&self, name: &Token) -> Result<UserTypeReference, TokenError> {
        Ok(UserTypeReference {
            name: name.clone(),
            db: self.user_types.clone(),
        })
    }

    pub fn add_global_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        let var = Rc::new(GlobalVariable::new(
            def.token,
            self.get_next_id(),
            def.dtype,
            def.init_expr,
        )?);
        self.add_to_global_scope(GlobalType::Variable(var))
    }

    pub fn add_type_alias(&mut self, token: Token, alias_type: Type) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::UserType(
            token,
            UserTypeOptions::ConcreteType(alias_type),
        ))?;
        self.update_user_types();
        Ok(())
    }

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

    pub fn add_const_var(&mut self, def: VariableDefinition) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::Constant(Rc::new(def.into_literal()?)))
    }

    pub fn add_function_declaration(
        &mut self,
        func: FunctionDeclaration,
    ) -> Result<(), TokenError> {
        self.add_to_global_scope(GlobalType::FunctionDeclaration(func))
    }

    pub fn add_function(&mut self, func: Rc<dyn FunctionDefinition>) -> Result<(), TokenError> {
        if let Entry::Occupied(e) = self
            .global_scope
            .entry(func.get_token().get_value().to_string())
        {
            if let GlobalType::FunctionDeclaration(_) = e.get() {
                e.remove_entry();
            } else {
                return Err(func.get_token().clone().into_err(format!(
                    "token already exists as {} - cannot add as a function",
                    e.get()
                )));
            };
        }

        self.add_to_global_scope(GlobalType::Function(func))
    }

    fn update_user_types(&mut self) {
        let mut tv = self.user_types.borrow_mut();
        tv.types.clear();

        for (n, v) in self.global_scope.iter() {
            if let GlobalType::UserType(_, t) = v {
                tv.types.insert(n.clone(), t.clone());
            }
        }
    }

    fn add_to_global_scope(&mut self, t: GlobalType) -> Result<(), TokenError> {
        let name = get_identifier(t.get_token())?;

        let statement = t.get_statement();
        match self.global_scope.entry(name.to_string()) {
            Entry::Vacant(e) => e.insert(t),
            Entry::Occupied(_) => {
                return Err(t
                    .get_token()
                    .clone()
                    .into_err(format!("global scope already contains name \"{name}\"")));
            }
        };

        if let Some(s) = statement {
            self.statements.push(s);
        }

        self.update_user_types();

        Ok(())
    }

    pub fn get_variable(&self, name: &Token) -> Result<Rc<dyn Expression>, TokenError> {
        let ident = get_identifier(name)?.to_string();

        if let Ok(v) = self.get_scopes().and_then(|x| x.get_variable(name)) {
            return Ok(v.get_expr());
        }

        match self.global_scope.get(&ident).map_or(
            Err(name
                .clone()
                .into_err("no variable with matching name found")),
            |x| Ok(x.clone()),
        )? {
            GlobalType::Variable(v) => Ok(v),
            GlobalType::Constant(v) => Ok(v),
            GlobalType::Function(f) => Ok(f.as_expr()),
            GlobalType::FunctionDeclaration(f) => Ok(f.as_expr()),
            x => Err(x
                .get_token()
                .clone()
                .into_err("global is not a variable type")),
        }
    }

    pub fn get_global_location_label(&self, name: &str) -> Option<&str> {
        match self.global_scope.get(name) {
            Some(GlobalType::Variable(v)) => Some(v.access_label()),
            Some(GlobalType::Function(v)) => Some(v.get_entry_label()),
            _ => None,
        }
    }

    pub fn get_global_constant(&self, name: &str) -> Option<Rc<Literal>> {
        match self.global_scope.get(name) {
            Some(GlobalType::Constant(v)) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn get_local_variable_offset(&self, name: &str) -> Option<usize> {
        self.scope_manager.as_ref().and_then(|sm| {
            sm.get_variable_name(name).and_then(|x| match x {
                ScopeVariables::Local(v) => Some(v.get_offset()),
                _ => None,
            })
        })
    }

    pub fn get_identifier_type(&self, name: &str) -> Option<Type> {
        if is_identifier(name) {
            if let Some(lv_type) = self.scope_manager.as_ref().and_then(|sm| {
                sm.get_variable_name(name).and_then(|x| match x {
                    ScopeVariables::Local(x) => x.get_type().ok(),
                    _ => None,
                })
            }) {
                return Some(lv_type);
            } else if let Some(g_type) = self.global_scope.get(name) {
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

        if let Ok(iter) = tokenize_str(name) {
            let mut tokens = TokenIter::from(&iter);
            Type::read_type(&mut tokens, self).ok()
        } else {
            None
        }
    }

    pub fn get_statements(&self) -> Vec<String> {
        let mut statements = Vec::default();
        for (n, i) in self.global_scope.iter() {
            if let GlobalType::Constant(c) = i {
                statements.push(format!("const {n} = {c};"));
            }
        }

        for (_, i) in self.global_scope.iter() {
            if let GlobalType::UserType(_, ut) = i {
                match ut {
                    UserTypeOptions::OpaqueType(s) => {
                        statements.push(format!("struct {};", s.get_value()))
                    }
                    UserTypeOptions::ConcreteType(s) => statements.push(format!("{s};")),
                }
            }
        }

        for (_, i) in self.global_scope.iter() {
            if let GlobalType::Variable(v) = i {
                statements.push(format!("{};", GlobalVariableStatement::new(v.clone())));
            }
        }

        for (_, i) in self.global_scope.iter() {
            if let GlobalType::Function(f) = i {
                statements.push(format!("{f}"))
            }
        }

        statements
    }
}
