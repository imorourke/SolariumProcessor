use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    path::{Path, PathBuf},
    rc::Rc,
    sync::LazyLock,
};

use regex::Regex;

use crate::{TokenError, tokenize, tokenizer::Token};

pub static DEFAULT_FILES: &[(&str, &str)] = &[
    (
        "kernel/kcpu.cb",
        include_str!("../components/kernel/kcpu.cb"),
    ),
    (
        "kernel/kclock.cb",
        include_str!("../components/kernel/kclock.cb"),
    ),
    (
        "kernel/kirq.cb",
        include_str!("../components/kernel/kirq.cb"),
    ),
    (
        "kernel/kmalloc.cb",
        include_str!("../components/kernel/kmalloc.cb"),
    ),
    (
        ("kernel/kserialio.cb"),
        include_str!("../components/kernel/kserialio.cb"),
    ),
    (
        "kernel/ktsk.cb",
        include_str!("../components/kernel/ktsk.cb"),
    ),
    ("std/list.cb", include_str!("../components/std/list.cb")),
    (
        ("std/string.cb"),
        include_str!("../components/std/string.cb"),
    ),
];

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
    system_fs: VirtualFilesystem,
    definitions: HashSet<String>,
    if_statements: Vec<IfState>,
}

impl PreprocessorState {
    pub fn new(fs: Box<dyn PreprocessorFilesystem>) -> PreprocessorState {
        Self {
            filesystem: fs,
            system_fs: VirtualFilesystem::new_system(),
            definitions: HashSet::default(),
            if_statements: Vec::default(),
        }
    }

    fn read_file(&mut self, file: &Path) -> Result<PreprocessorOutput, PreprocessorError> {
        self.read_file_inner(file, 0, false)
    }

    fn read_file_inner(
        &mut self,
        file: &Path,
        level: i32,
        use_system_fs: bool,
    ) -> Result<PreprocessorOutput, PreprocessorError> {
        let fname: Rc<str> = file.to_str().unwrap().into();

        let file_text = if use_system_fs {
            self.system_fs.read_file(file)?
        } else {
            self.filesystem.read_file(file)?
        };

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
                    static SYS_REGEX: LazyLock<Regex> =
                        LazyLock::new(|| Regex::new(r"^<(?<file>.*)>$").unwrap());

                    let (file_to_load, use_system) = if let Some(m) = SYS_REGEX.captures(arg)
                        && let Some(file) = m.name("file")
                    {
                        (PathBuf::from(file.as_str()), true)
                    } else {
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
                        (current.unwrap_or(Path::new(&arg).to_path_buf()), false)
                    };

                    lines.extend(
                        self.read_file_inner(
                            &file_to_load,
                            level + 1,
                            use_system_fs || use_system,
                        )?
                        .lines,
                    );
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

#[derive(Debug)]
pub enum FilesystemError {
    FileNotFound(PathBuf),
    UnableToLoadFile(PathBuf, String),
}

impl FilesystemError {
    fn get_path(&self) -> PathBuf {
        match self {
            Self::FileNotFound(f) => f.clone(),
            Self::UnableToLoadFile(f, _) => f.clone(),
        }
    }
}

impl Display for FilesystemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FileNotFound(file) => write!(f, "file \"{}\" not found", file.display()),
            Self::UnableToLoadFile(file, err) => {
                write!(f, "unable to load file \"{}\": {err}", file.display())
            }
        }
    }
}

trait PreprocessorFilesystem: Debug {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, FilesystemError>;
}

#[derive(Debug, Default)]
struct RealFilesystem {
    files: HashMap<PathBuf, Rc<str>>,
}

impl PreprocessorFilesystem for RealFilesystem {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, FilesystemError> {
        let txt: Rc<str> = match std::fs::read_to_string(file) {
            Ok(s) => s.into(),
            Err(e) => {
                return Err(FilesystemError::UnableToLoadFile(
                    file.to_path_buf(),
                    e.to_string(),
                ));
            }
        };

        match file.canonicalize() {
            Ok(path) => {
                self.files.insert(path, txt.clone());
                Ok(txt)
            }
            Err(e) => Err(FilesystemError::UnableToLoadFile(
                file.to_path_buf(),
                e.to_string(),
            )),
        }
    }
}

#[derive(Debug, Default)]
pub struct VirtualFilesystem {
    files: HashMap<PathBuf, Rc<str>>,
}

impl VirtualFilesystem {
    pub fn new_system() -> Self {
        let mut fs = Self::default();

        for (p, c) in DEFAULT_FILES {
            fs.add_file(Path::new(p), c).unwrap();
        }

        fs
    }

    pub fn new(code: &str, file: &Path) -> Self {
        let mut fs = VirtualFilesystem::default();
        fs.add_file(file, code).unwrap();
        fs
    }

    pub fn add_file(&mut self, file: &Path, code: &str) -> Result<(), FilesystemError> {
        match self.files.entry(file.to_path_buf()) {
            std::collections::hash_map::Entry::Occupied(_) => {
                Err(FilesystemError::UnableToLoadFile(
                    file.to_path_buf(),
                    "file already exists in virtual filesystem".into(),
                ))
            }
            std::collections::hash_map::Entry::Vacant(e) => {
                e.insert(code.into());
                Ok(())
            }
        }
    }
}

impl PreprocessorFilesystem for VirtualFilesystem {
    fn read_file(&mut self, file: &Path) -> Result<Rc<str>, FilesystemError> {
        if let Some(val) = self.files.get(file) {
            Ok(val.clone())
        } else {
            Err(FilesystemError::FileNotFound(file.to_path_buf()))
        }
    }
}

#[derive(Debug, Clone)]
pub struct PreprocessorError {
    pub loc: Option<PreprocessorLocation>,
    pub text: String,
    pub error: String,
}

impl From<FilesystemError> for PreprocessorError {
    fn from(value: FilesystemError) -> Self {
        Self {
            loc: None,
            text: value.get_path().display().to_string(),
            error: value.to_string(),
        }
    }
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
    let mut state = PreprocessorState::new(Box::new(VirtualFilesystem::new(text, file_path)));
    for d in defs.into_iter() {
        state.definitions.insert(d);
    }
    state.read_file(file_path)
}

pub fn read_and_preprocess<I: Iterator<Item = String>>(
    file: &Path,
    defs: I,
) -> Result<PreprocessorOutput, PreprocessorError> {
    let mut state = PreprocessorState::new(Box::new(RealFilesystem::default()));
    for d in defs.into_iter() {
        state.definitions.insert(d);
    }
    state.read_file(file)
}
