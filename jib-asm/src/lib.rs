mod argument;
mod immediate;
mod instructions;

use core::fmt;
use std::{collections::HashMap, fmt::Display, rc::Rc, sync::LazyLock};

pub use instructions::{
    Instruction, InstructionError, OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBrk, OpBshl, OpBshr,
    OpBxor, OpCall, OpConv, OpCopy, OpDiv, OpHalt, OpInt, OpIntoff, OpInton, OpIntr, OpJmp, OpJmpr,
    OpJmpri, OpLd, OpLdi, OpLdn, OpLdr, OpLdri, OpMul, OpNeg, OpNoop, OpNot, OpPop, OpPopr, OpPush,
    OpRem, OpReset, OpRet, OpRetInt, OpSav, OpSavr, OpSub, OpTeq, OpTg, OpTge, OpTl, OpTle, OpTneq,
    OpTnz, OpTz, INST_SIZE,
};

pub use argument::{ArgumentError, ArgumentRegister, ArgumentType};

use jib::cpu::{Opcode, Processor, ProcessorError};

use immediate::{
    parse_imm_i16, parse_imm_i32, parse_imm_i8, parse_imm_u16, parse_imm_u32, parse_imm_u8,
    ImmediateError,
};
use regex::Regex;

static LABEL_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new("^[a-z_](a-z0-9_)*").unwrap());

pub fn is_valid_label(s: &str) -> bool {
    LABEL_REGEX.is_match(s)
}

#[derive(Debug, Clone)]
pub enum AssemblerError {
    UnknownLabel(String),
    UnknownInstruction(String, Option<usize>),
    Instruction(InstructionError),
    ArgumentCountMismatch(usize, usize),
    CannotBackupAddress(u32),
    InvalidLabel(String),
    Immediate(ImmediateError),
    BadLabel(String),
    DuplicateLabel(String),
    Character(jib::text::CharacterError),
    AddressTaken(u32),
    Parser(ParseError),
    Processor(ProcessorError),
}

impl fmt::Display for AssemblerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownLabel(l) => write!(f, "Unknown Label {l}"),
            Self::UnknownInstruction(i, None) => write!(f, "Unknown Instruction {i}"),
            Self::UnknownInstruction(i, Some(len)) => {
                write!(f, "Unknown Instruction {i} with length {len}")
            }
            Self::ArgumentCountMismatch(num, expected) => {
                write!(f, "Argument Count Expected {expected}, found {num}")
            }
            Self::InvalidLabel(l) => write!(f, "Invalid Label {l}"),
            Self::Instruction(i) => write!(f, "Instruction Error => {i}"),
            Self::Immediate(i) => write!(f, "Immediate Error => {i}"),
            Self::BadLabel(l) => write!(f, "Bad Label '{l}'"),
            Self::DuplicateLabel(l) => write!(f, "Duplicate Label '{l}'"),
            Self::Character(c) => write!(f, "Character Error => {c}"),
            Self::AddressTaken(addr) => write!(f, "Address 0x{addr:08x} Taken"),
            Self::Parser(e) => write!(f, "Parser Error - {e}"),
            Self::Processor(e) => write!(f, "Processor Error - {e}"),
            Self::CannotBackupAddress(addr) => {
                write!(f, "Cannot Backup Address - Already Passed {addr}")
            }
        }
    }
}

impl From<ImmediateError> for AssemblerError {
    fn from(value: ImmediateError) -> Self {
        Self::Immediate(value)
    }
}

impl From<InstructionError> for AssemblerError {
    fn from(value: InstructionError) -> Self {
        Self::Instruction(value)
    }
}

impl From<jib::text::CharacterError> for AssemblerError {
    fn from(value: jib::text::CharacterError) -> Self {
        Self::Character(value)
    }
}

impl From<ProcessorError> for AssemblerError {
    fn from(value: ProcessorError) -> Self {
        Self::Processor(value)
    }
}

impl From<ParseError> for AssemblerError {
    fn from(value: ParseError) -> Self {
        Self::Parser(value)
    }
}

#[derive(Debug, Default, Clone)]
pub struct LocationInfo {
    pub line: usize,
    pub column: usize,
    pub text: Option<Rc<str>>,
}

impl fmt::Display for LocationInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", self.line)?;
        if let Some(txt) = &self.text {
            write!(f, " {}", txt)
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssemblerErrorLoc {
    pub err: AssemblerError,
    pub loc: LocationInfo,
}

impl fmt::Display for AssemblerErrorLoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Line {} - {}", self.loc.line, self.err)?;
        if let Some(s) = self.loc.text.as_ref() {
            write!(f, " - \"{}\"", s)?;
        }

        Ok(())
    }
}

type FnInst = fn(Vec<String>) -> Result<Rc<dyn Instruction>, InstructionError>;
type FnDisp = fn([u8; 4]) -> Option<String>;

pub trait FromLiteral<T> {
    fn from_literal(v: T) -> Self;
}

#[derive(Debug, Clone)]
pub enum AsmToken {
    ChangeAddress(u32),
    Operation(FnInst, String, Vec<String>),
    OperationLiteral(Box<dyn Instruction>),
    CreateLabel(String),
    LoadLoc(String),
    Literal1(u8),
    Literal2(u16),
    Literal4(u32),
    Reserve(u32),
    LiteralText(String),
    AlignInstruction,
    Comment(String),
    LocationComment(String),
    Empty,
}

impl AsmToken {
    fn trim_line(line: &str) -> &str {
        let s = line.trim();
        let mut ind = None;

        let mut is_escape = false;
        let mut within_quote = false;

        for (i, c) in line.char_indices() {
            if is_escape {
                continue;
            } else if c == '\\' {
                if !is_escape {
                    is_escape = true;
                }
            } else if c == '"' {
                within_quote = !within_quote;
            } else if !within_quote && c == ';' {
                ind = Some(i);
                break;
            }
        }

        if let Some(ind) = ind {
            &s[..ind]
        } else {
            s
        }
    }

    fn split_asm_delim(s: &str) -> Result<Vec<String>, ParseError> {
        let mut within_quote = false;
        let mut is_escape = false;
        let mut last_was_quote = false;

        let mut so_far = Vec::<char>::new();
        let mut words = Vec::new();

        for c in s.chars() {
            if last_was_quote {
                if c.is_whitespace() {
                    last_was_quote = false;
                } else {
                    return Err(ParseError::ExpectedSpaceBetweenQuote);
                }
            } else if within_quote {
                if is_escape {
                    if c == '\\' {
                        so_far.push('\\');
                    } else if c == 'n' {
                        so_far.push('\n');
                    } else if c == '"' {
                        so_far.push('"')
                    } else {
                        return Err(ParseError::UnknownEscapeCode(c));
                    }

                    is_escape = false;
                } else if c == '\\' {
                    is_escape = true;
                } else if c == '"' {
                    within_quote = false;
                    last_was_quote = true;
                    if !so_far.is_empty() {
                        words.push(so_far.iter().collect());
                        so_far.clear();
                    }
                } else {
                    so_far.push(c);
                }
            } else if c == '"' {
                if !so_far.is_empty() {
                    return Err(ParseError::ExpectedSpaceBetweenQuote);
                }
                within_quote = true;
            } else if c.is_whitespace() {
                if !so_far.is_empty() {
                    words.push(so_far.iter().collect());
                    so_far.clear();
                }
            } else {
                so_far.push(c);
            }
        }

        if within_quote {
            return Err(ParseError::WithinQuote);
        } else if is_escape {
            return Err(ParseError::WithinEscape);
        }

        if !so_far.is_empty() {
            words.push(so_far.into_iter().collect());
        }

        Ok(words)
    }
}

impl TryFrom<&str> for AsmToken {
    type Error = AssemblerError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        // Create the instruction list
        static INSTRUCTION_LIST: LazyLock<InstructionList> =
            LazyLock::new(InstructionList::default);

        // Check for a debug comment
        if let Some(cmt) = value.trim().strip_prefix("!") {
            return Ok(Self::LocationComment(cmt.trim().to_string()));
        }

        // Trim Comments
        let s = Self::trim_line(value.trim());

        let words = Self::split_asm_delim(s)?;

        let first: &str = if let Some(w) = words.first() {
            w
        } else {
            return Ok(Self::Empty);
        };

        let tok = if let Some(op) = first.strip_prefix('.') {
            let args = &words[1..];

            if args.is_empty() {
                match op {
                    "align" => Self::AlignInstruction,
                    _ => {
                        return Err(Self::Error::UnknownInstruction(
                            op.to_string(),
                            Some(args.len()),
                        ));
                    }
                }
            } else if args.len() == 1 {
                let arg = &args[0];

                match op {
                    "oper" => {
                        let addr = if let Some(r) = arg.strip_prefix('#') {
                            Processor::interrupt_address(jib::cpu::Interrupt::Hardware(
                                parse_imm_u32(r)?,
                            ))?
                        } else if let Some(r) = arg.strip_prefix('@') {
                            Processor::interrupt_address(jib::cpu::Interrupt::Software(
                                parse_imm_u32(r)?,
                            ))?
                        } else {
                            parse_imm_u32(arg)?
                        };

                        Self::ChangeAddress(addr)
                    }
                    "loadloc" => Self::LoadLoc(arg.into()),
                    "text" => Self::LiteralText(arg.as_str().into()),
                    "u8" => Self::Literal1(parse_imm_u8(arg)?),
                    "u16" => Self::Literal2(parse_imm_u16(arg)?),
                    "u32" => Self::Literal4(parse_imm_u32(arg)?),
                    "reserve" => Self::Reserve(parse_imm_u32(arg)?),
                    "i8" => Self::Literal1(parse_imm_i8(arg)? as u8),
                    "i16" => Self::Literal2(parse_imm_i16(arg)? as u16),
                    "i32" => Self::Literal4(parse_imm_i32(arg)? as u32),
                    "f32" => Self::Literal4(
                        match arg.parse::<f32>() {
                            Ok(v) => v,
                            Err(_) => return Err(ImmediateError(arg.to_string()).into()),
                        }
                        .to_bits(),
                    ),
                    _ => {
                        return Err(Self::Error::UnknownInstruction(
                            op.to_string(),
                            Some(args.len()),
                        ));
                    }
                }
            } else {
                return Err(Self::Error::ArgumentCountMismatch(args.len(), 1));
            }
        } else if let Some(lbl) = first.strip_prefix(':') {
            if !is_valid_label(lbl) {
                return Err(Self::Error::BadLabel(lbl.to_string()));
            }

            if words.len() != 1 {
                return Err(Self::Error::ArgumentCountMismatch(words.len(), 1));
            }

            Self::CreateLabel(lbl.to_string())
        } else if let Some(inst_fn) = INSTRUCTION_LIST.get_instruction(&words[0]) {
            let name = &words[0];
            let args = &words[1..];
            Self::Operation(
                *inst_fn,
                name.into(),
                args.iter().map(|s| s.as_str().into()).collect(),
            )
        } else {
            return Err(Self::Error::UnknownInstruction(value.into(), None));
        };

        Ok(tok)
    }
}

fn update_comment_values(s: &str) -> String {
    s.replace('\n', "\\n")
}

impl Display for AsmToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Comment(s) => write!(f, "; {}", update_comment_values(s)),
            Self::LocationComment(s) => write!(f, "! {}", update_comment_values(s)),
            Self::LoadLoc(l) => write!(f, ".loadloc {l}"),
            Self::AlignInstruction => write!(f, ".align"),
            Self::ChangeAddress(addr) => write!(f, ".oper {addr:#x}"),
            Self::Literal1(x) => write!(f, ".u8 {x:#x}"),
            Self::Literal2(x) => write!(f, ".u16 {x:#x}"),
            Self::Literal4(x) => write!(f, ".u32 {x:#x}"),
            Self::Reserve(x) => write!(f, ".reserve {x:#x}"),
            Self::LiteralText(t) => write!(f, ".text \"{t}\""),
            Self::CreateLabel(lbl) => write!(f, ":{lbl}"),
            Self::Operation(_, name, args) => {
                write!(f, "{name}")?;
                for a in args {
                    write!(f, " {a}")?;
                }
                Ok(())
            }
            Self::OperationLiteral(op) => {
                write!(f, "{}", op.get_name())?;
                for a in op.get_args() {
                    write!(f, " {a}")?;
                }
                Ok(())
            }
            Self::Empty => Ok(()),
        }
    }
}

impl Clone for Box<dyn Instruction> {
    fn clone(&self) -> Self {
        self.boxed_clone()
    }
}

impl FromLiteral<u8> for AsmToken {
    fn from_literal(v: u8) -> Self {
        Self::Literal1(v)
    }
}

impl FromLiteral<i8> for AsmToken {
    fn from_literal(v: i8) -> Self {
        Self::Literal1(v as u8)
    }
}

impl FromLiteral<u16> for AsmToken {
    fn from_literal(v: u16) -> Self {
        Self::Literal2(v)
    }
}

impl FromLiteral<i16> for AsmToken {
    fn from_literal(v: i16) -> Self {
        Self::Literal2(v as u16)
    }
}

impl FromLiteral<u32> for AsmToken {
    fn from_literal(v: u32) -> Self {
        Self::Literal4(v)
    }
}

impl FromLiteral<i32> for AsmToken {
    fn from_literal(v: i32) -> Self {
        Self::Literal4(v as u32)
    }
}

impl FromLiteral<f32> for AsmToken {
    fn from_literal(v: f32) -> Self {
        Self::Literal4(v.to_bits())
    }
}

#[derive(Debug, Clone)]
pub struct AsmTokenLoc {
    pub tok: AsmToken,
    pub loc: LocationInfo,
}

pub struct InstructionList {
    inst_map: HashMap<String, FnInst>,
    name_map: HashMap<Opcode, String>,
    disp_map: HashMap<Opcode, FnDisp>,
}

macro_rules! create_instruction_map {
    ($($op:ident),*) => {
        Vec::<(Opcode, String, FnInst, FnDisp)>::from([
            $( { (
                $op::OP,
                $op::name().into(),
                (|a| Ok(Rc::new($op::try_from(a)?) as Rc<dyn Instruction>)) as FnInst,
                (|b| { let res = $op::try_from(b); if let Ok(s) = res { Some(s.to_string()) } else { None } }) as FnDisp )
            } ),*
        ])
    };
}

impl InstructionList {
    pub fn get_name_for_opcode(&self, op: &Opcode) -> Option<&String> {
        self.name_map.get(op)
    }

    pub fn get_instruction(&self, s: &str) -> Option<&FnInst> {
        self.inst_map.get(s)
    }

    pub fn get_display(&self, inst: [u8; jib::cpu::Instruction::NUM_BYTES]) -> Option<String> {
        let op = Opcode::from(inst[0]);
        if let Some(f) = self.disp_map.get(&op) {
            f(inst)
        } else {
            None
        }
    }

    pub fn get_display_inst(&self, inst: jib::cpu::Instruction) -> Option<String> {
        self.get_display(inst.get_data())
    }
}

impl Default for InstructionList {
    fn default() -> Self {
        let inst = create_instruction_map!(
            OpAdd, OpBand, OpBnot, OpBool, OpBor, OpBshl, OpBshr, OpBxor, OpCall, OpConv, OpCopy,
            OpDiv, OpHalt, OpInt, OpIntoff, OpBrk, OpInton, OpIntr, OpJmp, OpJmpr, OpJmpri, OpLd,
            OpLdi, OpLdn, OpLdr, OpLdri, OpMul, OpNeg, OpNoop, OpNot, OpPop, OpPopr, OpPush, OpRem,
            OpReset, OpRet, OpRetInt, OpSav, OpSavr, OpSub, OpTeq, OpTg, OpTge, OpTl, OpTle,
            OpTneq, OpTnz, OpTz
        );

        let inst_map = inst.iter().map(|(_, n, f, _)| (n.to_owned(), *f)).collect();
        let name_map = inst.iter().map(|(o, n, _, _)| (*o, n.to_owned())).collect();
        let disp_map = inst.iter().map(|(o, _, _, d)| (*o, *d)).collect();

        Self {
            inst_map,
            name_map,
            disp_map,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ParseError {
    UnknownEscapeCode(char),
    WithinQuote,
    WithinEscape,
    ExpectedSpaceBetweenQuote,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownEscapeCode(c) => write!(f, "Unknown escape code '\\{c}'"),
            Self::WithinQuote => write!(f, "Parser ended within a quote"),
            Self::WithinEscape => write!(f, "Parser ending with an unfinished escape code"),
            Self::ExpectedSpaceBetweenQuote => write!(f, "Expected space between quote"),
        }
    }
}

#[derive(Default)]
pub struct TokenList {
    tokens: Vec<AsmTokenLoc>,
}

impl TokenList {
    pub fn parse_line(&mut self, line: &str, loc: LocationInfo) -> Result<(), AssemblerError> {
        self.tokens.push(AsmTokenLoc {
            tok: AsmToken::try_from(line)?,
            loc,
        });
        Ok(())
    }

    pub fn add_token(&mut self, tok: AsmTokenLoc) {
        self.tokens.push(tok)
    }

    pub fn to_bytes(&self) -> Result<AssemblerOutput, AssemblerErrorLoc> {
        let mut state = ParserState::new();

        for t in self.tokens.iter() {
            let loc = t.loc.clone();

            match &t.tok {
                AsmToken::Comment(..) => (),
                AsmToken::LocationComment(s) => state.debug_comment.push((state.addr, s.clone())),
                AsmToken::Empty => (),
                AsmToken::AlignInstruction => state.align_boundary(Processor::BYTES_PER_WORD),
                AsmToken::OperationLiteral(op) => {
                    state.add_bytes(&op.to_u32().to_be_bytes(), loc)?;
                }
                AsmToken::ChangeAddress(new_addr) => {
                    if *new_addr < state.addr {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::CannotBackupAddress(*new_addr),
                            loc,
                        });
                    } else {
                        state.addr = *new_addr;
                    }
                }
                AsmToken::LiteralText(s) => {
                    for c in s.chars() {
                        let bv = match jib::text::character_to_byte(c) {
                            Ok(v) => v,
                            Err(e) => {
                                return Err(AssemblerErrorLoc {
                                    err: AssemblerError::from(e),
                                    loc,
                                });
                            }
                        };
                        state.add_bytes(&[bv], loc.clone())?;
                    }
                    state.add_bytes(&[0], loc.clone())?;
                }
                AsmToken::Literal1(i) => {
                    state.add_bytes(&[*i], loc)?;
                }
                AsmToken::Literal2(i) => {
                    state.add_bytes(&i.to_be_bytes(), loc)?;
                }
                AsmToken::Literal4(i) => {
                    state.add_bytes(&i.to_be_bytes(), loc)?;
                }
                AsmToken::Reserve(x) => {
                    state.add_bytes(&vec![0; *x as usize], loc)?;
                }
                AsmToken::CreateLabel(lbl) => {
                    if state.labels.contains_key(lbl) {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::DuplicateLabel(lbl.to_string()),
                            loc,
                        });
                    }
                    state.labels.insert(lbl.into(), state.addr);
                }
                AsmToken::LoadLoc(lbl) => {
                    state.add_delay(
                        DelayToken::LoadLoc {
                            label: lbl.as_str().into(),
                        },
                        t.loc.clone(),
                    )?;
                }
                AsmToken::Operation(func, _, args) => {
                    state.add_delay(
                        DelayToken::Operation {
                            inst: *func,
                            args: args.clone(),
                        },
                        t.loc.clone(),
                    )?;
                }
            }
        }

        state.process_delays()?;

        if let Some(min_addr) = state.values.keys().min().copied() {
            if let Some(max_addr) = state.values.keys().max().copied() {
                let mut bytes = vec![0; (max_addr - min_addr) as usize + 1];

                for (a, v) in state.values {
                    bytes[(a - min_addr) as usize] = v;
                }

                return Ok(AssemblerOutput {
                    start_address: min_addr,
                    bytes,
                    labels: state.labels,
                    debug: state.debug_comment,
                });
            }
        }

        Ok(AssemblerOutput::default())
    }
}

#[derive(Default, Clone)]
pub struct AssemblerOutput {
    pub start_address: u32,
    pub bytes: Vec<u8>,
    pub labels: HashMap<String, u32>,
    pub debug: Vec<(u32, String)>,
}

#[derive(Debug, Clone)]
enum DelayToken {
    LoadLoc { label: Rc<str> },
    Operation { inst: FnInst, args: Vec<String> },
}

#[derive(Default)]
struct ParserState {
    addr: u32,
    labels: HashMap<String, u32>,
    debug_comment: Vec<(u32, String)>,
    values: HashMap<u32, u8>,
    delay_vals: HashMap<u32, (DelayToken, LocationInfo)>,
}

impl ParserState {
    pub fn new() -> Self {
        Self::default()
    }

    fn add_delay(&mut self, delay: DelayToken, loc: LocationInfo) -> Result<(), AssemblerErrorLoc> {
        let base = self.add_bytes(&0u32.to_be_bytes(), loc.clone())?;

        if self.delay_vals.insert(base, (delay, loc.clone())).is_some() {
            return Err(AssemblerErrorLoc {
                err: AssemblerError::AddressTaken(base),
                loc,
            });
        }

        Ok(())
    }

    fn add_bytes(&mut self, vals: &[u8], loc: LocationInfo) -> Result<u32, AssemblerErrorLoc> {
        let base = self.addr;
        for v in vals {
            if self.values.insert(self.addr, *v).is_some() {
                return Err(AssemblerErrorLoc {
                    err: AssemblerError::AddressTaken(self.addr),
                    loc,
                });
            }
            self.addr += 1;
        }
        Ok(base)
    }

    fn align_boundary(&mut self, val: u32) {
        if val > 0 && !self.addr.is_multiple_of(val) {
            self.addr += val - (self.addr % val);
        }
    }

    fn process_delays(&mut self) -> Result<(), AssemblerErrorLoc> {
        for (addr, (tok, loc)) in self.delay_vals.iter() {
            let insert_value = match tok {
                DelayToken::LoadLoc { label } => {
                    if let Some(loc) = self.labels.get(label.as_ref()) {
                        *loc
                    } else {
                        return Err(AssemblerErrorLoc {
                            err: AssemblerError::UnknownLabel(label.to_string()),
                            loc: loc.clone(),
                        });
                    }
                }
                DelayToken::Operation { inst, args } => {
                    // Create new arguments to get relative values for any label parameters
                    let mut new_args = Vec::new();

                    for a in args.iter() {
                        let na = if let Some(v) = self.labels.get(a) {
                            format!("{}", (*v as i32) - (*addr as i32))
                        } else {
                            a.to_string()
                        };

                        new_args.push(na);
                    }

                    // Call the instruction function and obtain the resulting parameters
                    let val = match inst(new_args) {
                        Ok(v) => v,
                        Err(err) => {
                            return Err(AssemblerErrorLoc {
                                err: err.into(),
                                loc: loc.clone(),
                            });
                        }
                    };

                    // Replace the
                    val.to_u32()
                }
            };

            for (i, b) in insert_value.to_be_bytes().iter().enumerate() {
                self.values.insert(addr + i as u32, *b);
            }
        }

        self.delay_vals.clear();

        Ok(())
    }
}

pub fn assemble_text(txt: &str) -> Result<AssemblerOutput, AssemblerErrorLoc> {
    assemble_lines(&txt.lines().collect::<Vec<_>>())
}

pub fn assemble_lines(txt: &[&str]) -> Result<AssemblerOutput, AssemblerErrorLoc> {
    let mut state = TokenList::default();

    for (i, l) in txt.iter().enumerate() {
        let loc = LocationInfo {
            line: i + 1,
            column: 0,
            text: Some(l.to_string().into()),
        };
        if let Err(e) = state.parse_line(l, loc.clone()) {
            return Err(AssemblerErrorLoc { err: e, loc });
        }
    }

    state.to_bytes()
}

pub fn assemble_tokens<T>(tokens: T) -> Result<AssemblerOutput, AssemblerErrorLoc>
where
    T: IntoIterator<Item = AsmTokenLoc>,
{
    let mut state = TokenList::default();
    for t in tokens.into_iter() {
        state.add_token(t);
    }
    state.to_bytes()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_counter() {
        let txt = include_str!("../../jib-asm/examples/counter.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().bytes.is_empty());
    }

    #[test]
    fn test_hello_world() {
        let txt = include_str!("../../jib-asm/examples/hello_world.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().bytes.is_empty());
    }

    #[test]
    fn test_infinite_counter() {
        let txt = include_str!("../../jib-asm/examples/infinite_counter.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().bytes.is_empty());
    }

    #[test]
    fn test_serial_echo() {
        let txt = include_str!("../../jib-asm/examples/serial_echo.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().bytes.is_empty());
    }

    #[test]
    fn test_thread_test() {
        let txt = include_str!("../../jib-asm/examples/thread_test.jsm");
        let res = assemble_text(txt);
        assert!(res.is_ok());
        assert!(!res.unwrap().bytes.is_empty());
    }
}
