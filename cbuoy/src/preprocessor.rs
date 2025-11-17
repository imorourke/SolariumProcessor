use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    rc::Rc,
};

use crate::{TokenError, tokenize, tokenizer::Token};

#[derive(Debug, Clone)]
pub struct PreprocessorOutput {
    lines: Vec<PreprocessorLine>,
}

impl PreprocessorOutput {
    pub fn full_string(&self) -> String {
        self.lines
            .iter()
            .map(|x| &x.text)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn tokenize(&self) -> Result<Vec<Token>, TokenError> {
        tokenize(
            self.lines
                .iter()
                .map(|x| (x.text.clone(), Some(x.loc.clone()))),
        )
    }

    pub fn get_lines(&self) -> &[PreprocessorLine] {
        &self.lines
    }
}

#[derive(Debug)]
struct PreprocessorState {
    filesystem: Box<dyn PreprocessorFilesystem>,
    definitions: HashSet<String>,
    if_statements: Vec<IfState>,
}

impl PreprocessorState {
    pub fn new(fs: Box<dyn PreprocessorFilesystem>) -> PreprocessorState {
        Self {
            filesystem: fs,
            definitions: HashSet::default(),
            if_statements: Vec::default(),
        }
    }

    fn read_file(&mut self, file: &Path) -> Result<PreprocessorOutput, PreprocessorError> {
        self.read_file_inner(file, 0)
    }

    fn read_file_inner(
        &mut self,
        file: &Path,
        level: i32,
    ) -> Result<PreprocessorOutput, PreprocessorError> {
        let fname: Rc<str> = file.to_str().unwrap().into();

        let file_text = self.filesystem.read_file(file)?;

        let mut lines = Vec::new();

        for (i, l) in file_text.lines().enumerate() {
            if let Some(after) = l.trim_start().strip_prefix("#") {
                let verb;
                let arg;

                if let Some((first, second)) = after.split_once(' ') {
                    verb = first;
                    let tmp = second.trim();

                    if let Some(i) = tmp.find("//") {
                        arg = tmp[..i].trim()
                    } else {
                        arg = tmp
                    }
                } else {
                    verb = after;
                    arg = "";
                }

                let gen_error = |s: &str| PreprocessorError {
                    loc: Some(PreprocessorLocation {
                        file: fname.clone(),
                        line: i + 1,
                    }),
                    text: l.to_string(),
                    error: s.to_string(),
                };

                if verb == "include" {
                    let mut current = Path::parent(file).map(|x| x.to_path_buf());
                    for c in Path::new(&arg).components() {
                        if c.as_os_str() == "." {
                            // Do Nothing
                        } else if c.as_os_str() == ".." {
                            current = current.and_then(|x| x.parent().map(|x| x.to_path_buf()));
                        } else {
                            current = current.map(|x| x.join(c));
                        }
                    }

                    let file_to_load = current.unwrap_or(Path::new(&arg).to_path_buf());

                    lines.extend(self.read_file_inner(&file_to_load, level + 1)?.lines);
                } else if verb == "ifdef" {
                    self.if_statements
                        .push(IfState::new(self.definitions.contains(arg)));
                } else if verb == "ifndef" {
                    self.if_statements
                        .push(IfState::new(!self.definitions.contains(arg)));
                } else if verb == "define" {
                    self.definitions.insert(arg.into());
                } else if verb == "else" {
                    if !arg.is_empty() {
                        return Err(gen_error("no argument expected for line"));
                    }

                    if let Some(state) = self.if_statements.last_mut() {
                        if state.is_else {
                            return Err(gen_error(
                                "else statement already used for this statement",
                            ));
                        } else {
                            state.is_else = true;
                        }
                    }
                } else if verb == "endif" {
                    if !arg.is_empty() {
                        return Err(gen_error("no argument expected for line"));
                    } else if self.if_statements.pop().is_none() {
                        return Err(gen_error(
                            "cannot end an if statement after all statements have been applied already",
                        ));
                    }
                } else {
                    return Err(gen_error(&format!(
                        "unknown preprocessor action '{verb}' with arg '{arg}'"
                    )));
                }
            } else if self.if_statements.iter().all(|x| x.get_current()) {
                lines.push(PreprocessorLine {
                    text: l.into(),
                    loc: PreprocessorLocation {
                        file: fname.clone(),
                        line: i + 1,
                    },
                });
            }
        }

        if level == 0 && !self.if_statements.is_empty() {
            Err(PreprocessorError {
                loc: None,
                text: String::default(),
                error: "unclosed if statements remaining after processing completion".into(),
            })
        } else {
            Ok(PreprocessorOutput { lines })
        }
    }
}

trait PreprocessorFilesystem: Debug {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, PreprocessorError>;
}

#[derive(Debug, Default)]
struct RealPreprocessorFilesystem {
    files: HashMap<PathBuf, Rc<str>>,
}

impl PreprocessorFilesystem for RealPreprocessorFilesystem {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, PreprocessorError> {
        let txt: Rc<str> = match std::fs::read_to_string(file) {
            Ok(s) => s.into(),
            Err(e) => {
                return Err(PreprocessorError {
                    loc: None,
                    text: file.to_str().map(|x| x.to_string()).unwrap_or_default(),
                    error: format!(
                        "unable to load file \"{}\" - {e}",
                        file.to_str().unwrap_or("?")
                    ),
                });
            }
        };

        match file.canonicalize() {
            Ok(path) => {
                self.files.insert(path, txt.clone());
                Ok(txt)
            }
            Err(e) => Err(PreprocessorError {
                loc: None,
                text: file.to_str().map(|x| x.to_string()).unwrap_or_default(),
                error: format!(
                    "unable to canonicalize file \"{}\" - {e}",
                    file.to_str().unwrap_or("?")
                ),
            }),
        }
    }
}

#[derive(Debug, Default)]
pub struct VirtualPreprocessorFilesystem {
    files: HashMap<PathBuf, Rc<str>>,
}

impl VirtualPreprocessorFilesystem {
    pub fn new_as(code: &str, file: &Path) -> Self {
        let mut fs = VirtualPreprocessorFilesystem::default();
        fs.files.insert(
            PathBuf::from("components/kcpu.cb"),
            include_str!("../examples/components/kcpu.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/kclock.cb"),
            include_str!("../examples/components/kclock.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/kirq.cb"),
            include_str!("../examples/components/kirq.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/kmalloc.cb"),
            include_str!("../examples/components/kmalloc.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/kserialio.cb"),
            include_str!("../examples/components/kserialio.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/ktsk.cb"),
            include_str!("../examples/components/ktsk.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/std_list.cb"),
            include_str!("../examples/components/std_list.cb").into(),
        );
        fs.files.insert(
            PathBuf::from("components/std_string.cb"),
            include_str!("../examples/components/std_string.cb").into(),
        );
        fs.files.insert(file.to_path_buf(), code.into());
        fs
    }
}

impl PreprocessorFilesystem for VirtualPreprocessorFilesystem {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, PreprocessorError> {
        if let Some(val) = self.files.get(file) {
            Ok(val.clone())
        } else {
            Err(PreprocessorError {
                loc: None,
                text: file.to_str().map(|x| x.to_string()).unwrap_or_default(),
                error: format!(
                    "no virtual file \"{}\" specified",
                    file.to_str().unwrap_or("?")
                ),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct PreprocessorError {
    pub loc: Option<PreprocessorLocation>,
    pub text: String,
    pub error: String,
}

#[derive(Debug, Clone)]
pub struct PreprocessorLine {
    pub text: String,
    pub loc: PreprocessorLocation,
}

#[derive(Debug, Clone)]
pub struct PreprocessorLocation {
    pub file: Rc<str>,
    pub line: usize,
}

impl Display for PreprocessorLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.file, self.line)
    }
}

impl Display for PreprocessorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CBP: ")?;
        if let Some(loc) = &self.loc {
            write!(f, "[{}] @ ", loc)?;
        }
        write!(f, "\"{}\" => {}", self.text, self.error)
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct IfState {
    value: bool,
    is_else: bool,
}

impl IfState {
    fn new(value: bool) -> Self {
        Self {
            value,
            is_else: false,
        }
    }

    fn get_current(&self) -> bool {
        self.value ^ self.is_else
    }
}

pub fn preprocess_code_std<I: Iterator<Item = String>>(
    text: &str,
    defs: I,
) -> Result<PreprocessorOutput, PreprocessorError> {
    preprocess_code_as_file(text, Path::new("main.cb"), defs)
}

pub fn preprocess_code_as_file<I: Iterator<Item = String>>(
    text: &str,
    file_path: &Path,
    defs: I,
) -> Result<PreprocessorOutput, PreprocessorError> {
    let mut state = PreprocessorState::new(Box::new(VirtualPreprocessorFilesystem::new_as(
        text, file_path,
    )));
    for d in defs.into_iter() {
        state.definitions.insert(d);
    }
    state.read_file(file_path)
}

pub fn read_and_preprocess<I: Iterator<Item = String>>(
    file: &Path,
    defs: I,
) -> Result<PreprocessorOutput, PreprocessorError> {
    let mut state = PreprocessorState::new(Box::new(RealPreprocessorFilesystem::default()));
    for d in defs.into_iter() {
        state.definitions.insert(d);
    }
    state.read_file(file)
}
