use std::{io::Write, path::PathBuf};

use cbuoy::{PreprocessorLine, ProgramType, TokenError, parse, read_and_preprocess};
use clap::Parser;
use jib_asm::assemble_tokens;

#[derive(Default, Debug, clap::Parser)]
#[command(version, about)]
struct CompilerArguments {
    #[arg()]
    input_file: PathBuf,
    #[arg(
        short = 'o',
        long = "output",
        help = "Creates a binary file with the generated machine code"
    )]
    output_binary: Option<PathBuf>,
    #[arg(
        short = 'k',
        long = "kernel",
        default_value_t = false,
        help = "Enables program generation in kernel mode"
    )]
    is_kernel_program: bool,
    #[arg(
        short = 'a',
        long = "output-ast",
        default_value_t = false,
        help = "Prints the AST to the console"
    )]
    print_ast: bool,
    #[arg(
        short = 'l',
        long = "locs",
        default_value_t = false,
        help = "Includes location/debugging information in output assembly code"
    )]
    include_locations: bool,
    #[arg(
        short = 'j',
        long = "jib",
        help = "Creates a text file with the generated assembly code"
    )]
    output_assembly: Option<PathBuf>,
    #[arg(
        long = "print-asm",
        default_value_t = false,
        help = "Prints the generated assembly to the console"
    )]
    print_assembly: bool,
    #[arg(
        short = 'c',
        long = "output-cbp",
        help = "Creates a text file containing the preprocessed source code"
    )]
    output_preproc: Option<PathBuf>,
    #[arg(
        long = "print-cbp",
        default_value_t = false,
        help = "Prints the preprocessed output to the console"
    )]
    print_preproc: bool,
    #[arg(
        short = 'D',
        long = "define",
        help = "Adds compiler definitions to define from the start of compiling"
    )]
    definitions: Vec<String>,
}

fn main() -> std::process::ExitCode {
    let args = CompilerArguments::parse();

    let preprocessed = match read_and_preprocess(&args.input_file, args.definitions.into_iter()) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            return 1.into();
        }
    };

    if args.print_preproc {
        for l in preprocessed.get_lines() {
            println!("{}", l.text);
        }
    }

    if let Some(file) = &args.output_preproc {
        match std::fs::File::create(file) {
            Ok(mut f) => {
                for l in preprocessed.get_lines() {
                    writeln!(f, "{}", l.text).unwrap();
                }
            }
            Err(e) => {
                eprintln!(
                    "unable to open output file {} - {e}",
                    file.to_str().unwrap_or("?")
                );
                return 1.into();
            }
        }
    }

    let input_tokens = {
        match preprocessed.tokenize() {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{e}");
                return 1.into();
            }
        }
    };

    let cbstate = match parse(
        input_tokens.clone(),
        cbuoy::CodeGenerationOptions {
            prog_type: if args.is_kernel_program {
                ProgramType::DEFAULT_KERNEL
            } else {
                ProgramType::Application
            },
            debug_locations: args.include_locations,
        },
    ) {
        Ok(asm) => asm,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e);
            return 1.into();
        }
    };

    if args.print_ast {
        println!("{}", cbstate.get_statements().join("\n"));
    }

    let asm = match cbstate.get_assembler() {
        Ok(v) => v,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e);
            return 1.into();
        }
    };

    let asm_out = match assemble_tokens(asm) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            return 2.into();
        }
    };

    if args.print_assembly {
        for l in &asm_out.assembly_lines {
            println!("{l}");
        }
    }

    if let Some(out) = args.output_assembly {
        match std::fs::File::create(out) {
            Ok(mut f) => {
                for l in &asm_out.assembly_lines {
                    writeln!(f, "{l}").unwrap();
                }
                if args.include_locations {
                    for l in &asm_out.assembly_debug {
                        writeln!(f, "{l}").unwrap();
                    }
                }
            }
            Err(e) => {
                eprintln!("{e}");
                return 3.into();
            }
        }
    }

    if let Some(out) = args.output_binary {
        match std::fs::File::create(out) {
            Ok(mut f) => f.write_all(&asm_out.bytes).unwrap(),
            Err(e) => {
                eprintln!("{e}");
                return 4.into();
            }
        }
    }

    std::process::ExitCode::SUCCESS
}

fn print_error(txt: &[PreprocessorLine], err: &TokenError) {
    eprintln!("Error: {}", err.msg);

    if let Some(t) = &err.token {
        for l in txt.iter() {
            if l.loc.line == t.get_loc().line && Some(&l.loc.file) == t.get_loc().file.as_ref() {
                let line = &l.text;
                eprintln!("{} >> {line}", l.loc);
                eprint!("{}    ", l.loc);
                for _ in 0..t.get_loc().column {
                    eprint!(" ");
                }
                for _ in 0..t.get_value().len() {
                    eprint!("^");
                }
                eprintln!();
                break;
            }
        }
    }
}
