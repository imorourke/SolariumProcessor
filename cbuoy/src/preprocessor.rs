use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    path::{Path, PathBuf},
    rc::Rc,
};

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

#[derive(Debug, Default, Clone)]
struct PreprocessorState {
    files: HashMap<PathBuf, Rc<str>>,
    definitions: HashSet<String>,
    if_statements: Vec<IfState>,
}

impl PreprocessorState {
    fn read_file(&mut self, file: &Path) -> Result<Vec<PreprocessorLine>, PreprocessorError> {
        let fname: Rc<str> = file.to_str().unwrap().into();

        let file_text = if let Some(txt) = self.files.get(file) {
            txt.clone()
        } else {
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
            self.files.insert(file.to_path_buf(), txt.clone());
            txt
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
                    let file_to_load = if let Some(p) = Path::parent(file) {
                        p.join(arg)
                    } else {
                        Path::new(&arg).to_path_buf()
                    };

                    lines.extend(self.read_file(&file_to_load)?);
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

        Ok(lines)
    }
}

pub fn read_and_preprocess<I: Iterator<Item = String>>(
    file: &Path,
    defs: I,
) -> Result<Vec<PreprocessorLine>, PreprocessorError> {
    let mut state = PreprocessorState::default();
    for d in defs.into_iter() {
        state.definitions.insert(d);
    }
    let s = state.read_file(file)?;
    if !state.if_statements.is_empty() {
        Err(PreprocessorError {
            loc: None,
            text: String::default(),
            error: "unclosed if statements remaining after processing completion".into(),
        })
    } else {
        Ok(s)
    }
}
