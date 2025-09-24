mod compiler;
mod expressions;
mod functions;
mod literals;
mod parser;
mod tokenizer;
mod typing;
mod utilities;
mod variables;

pub use parser::parse;
pub use tokenizer::TokenError;

#[cfg(test)]
mod test {
    use jib_asm::{assemble_lines, assemble_tokens};

    use crate::parse;

    static EXAMPLE_FILES: &[&str] = &[
        include_str!("../examples/array_test.cb"),
        include_str!("../examples/default.cb"),
        include_str!("../examples/printing.cb"),
        include_str!("../examples/threading.cb"),
    ];

    #[test]
    fn valid_compiling_and_assembler_output() {
        for s in EXAMPLE_FILES {
            let cb_out = parse(s).unwrap();
            let asm_out_main = assemble_tokens(cb_out.get_assembler().unwrap()).unwrap();
            let asm_out_duplicate =
                assemble_lines(asm_out_main.assembly_lines.iter().map(|x| x.as_ref())).unwrap();

            assert_eq!(asm_out_main.bytes, asm_out_duplicate.bytes);
        }
    }
}
