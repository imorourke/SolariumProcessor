mod compiler;
mod expressions;
mod functions;
mod literals;
mod parser;
mod preprocessor;
mod tokenizer;
mod typing;
mod utilities;
mod variables;

pub use compiler::{CodeGenerationOptions, ProgramType};
pub use parser::{parse, parse_str};
pub use preprocessor::{
    PreprocessorError, PreprocessorLine, preprocess_code_as_file, preprocess_code_std,
    read_and_preprocess,
};
pub use tokenizer::{TokenError, tokenize, tokenize_file, tokenize_str};

#[cfg(test)]
mod test {
    use std::path::Path;

    use jib_asm::{assemble_lines, assemble_tokens};

    use crate::{CodeGenerationOptions, parse, tokenize_file};

    static EXAMPLE_FILES: &[&str] = &[
        "examples/array_test.cb",
        "examples/default.cb",
        "examples/printing.cb",
        "examples/threading.cb",
    ];

    #[test]
    fn valid_compiling_and_assembler_output() {
        for s in EXAMPLE_FILES {
            let input_file = Path::join(&Path::new(env!("CARGO_MANIFEST_DIR")), &Path::new(s));
            let tokens = tokenize_file(&input_file).unwrap();
            let cb_out = parse(tokens, CodeGenerationOptions::default()).unwrap();
            let asm_out_main = assemble_tokens(cb_out.get_assembler().unwrap()).unwrap();
            let asm_out_duplicate =
                assemble_lines(asm_out_main.assembly_lines.iter().map(|x| x.as_ref())).unwrap();

            assert_eq!(asm_out_main.bytes, asm_out_duplicate.bytes);
        }
    }
}
