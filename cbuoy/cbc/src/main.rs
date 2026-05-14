/// cbc is the command-line interface for the C/Buoy compiler. This provides a way to
/// interactively compile arbitary programs and use in various formats
use std::{io::Write, path::PathBuf};

use cblang::{
    CodeGenerationOptions, CompilerError, PreprocessorLine, ProgramType, compile,
    read_and_preprocess,
};
use clap::Parser;

/// Compiler arguments used to control how the compiler functions
#[derive(Default, Debug, Parser)]
#[command(version, about)]
struct CompilerArguments {
    /// Provides the primary input file to compile
    #[arg()]
    input_file: PathBuf,
    /// Provides the name of the generated binary file
    #[arg(
        short = 'o',
        long = "output",
        help = "Creates a binary file with the generated machine code"
    )]
    output_binary: Option<PathBuf>,
    /// If true, will use as a kernel program instead of an application, using the global
    /// kernel offset
    #[arg(
        short = 'k',
        long = "kernel",
        help = "Enables program generation in kernel mode with the provided start location"
    )]
    kernel_program: bool,
    /// The starting location to use for a kernel program
    #[arg(
        short = 'K',
        long = "kernel-start-loc",
        default_value_t = ProgramType::DEFAULT_START_OFFSET,
        help="Initial program location when generating in kernel mode"
    )]
    kernel_start_offset: u32,
    /// The stack location for a kernel program
    #[arg(
        short = 'S',
        long = "kernel-stack-loc",
        default_value_t = ProgramType::DEFAULT_STACK_LOC,
        help = "Initial stack location when generating in kernel mode",
    )]
    kernel_stack_loc: u32,
    /// Determines whether unused functions should be trimmed from the binary
    #[arg(
        short = 't',
        long = "trim",
        default_value_t = false,
        help = "Trims unused functions and variables from the generated code"
    )]
    trim_unused: bool,
    /// Provides the overlal AST to the console
    #[arg(
        short = 'a',
        long = "output-ast",
        default_value_t = false,
        help = "Prints the AST to the console"
    )]
    print_ast: bool,
    /// Adds additional debugging information to the generated assembly code
    #[arg(
        short = 'l',
        long = "locs",
        default_value_t = false,
        help = "Includes location/debugging information in output assembly code"
    )]
    include_locations: bool,
    /// Provides a text file with the raw assembly code generated, pre-assembly
    #[arg(
        short = 'j',
        long = "jib",
        help = "Creates a text file with the generated assembly code"
    )]
    output_assembly: Option<PathBuf>,
    /// Provides a text file with the generated/preprocessed source code
    #[arg(
        short = 'c',
        long = "output-cbp",
        help = "Creates a text file containing the preprocessed source code"
    )]
    output_preproc: Option<PathBuf>,
    /// Provdes any additional compiler definitions to use when compiling
    #[arg(
        short = 'D',
        long = "define",
        help = "Adds compiler definitions to define from the start of compiling"
    )]
    definitions: Vec<String>,
    /// Provides an output interface/map file to use for constants, structures, and functions
    #[arg(
        short = 'W',
        long = "write-interface",
        help = "Writes an interface file for the provided symbols"
    )]
    write_interface_file: Option<PathBuf>,
}

impl CompilerArguments {
    /// Provides the current code generation options
    fn compiler_options(&self) -> CodeGenerationOptions {
        CodeGenerationOptions {
            prog_type: if self.kernel_program {
                ProgramType::Kernel {
                    stack_loc: self.kernel_stack_loc,
                    start_offset: self.kernel_start_offset,
                }
            } else {
                ProgramType::Application
            },
            debug_locations: self.include_locations,
            trim_code: self.trim_unused,
        }
    }
}

/// Main entry function
fn main() -> std::process::ExitCode {
    let args = CompilerArguments::parse();

    // Construct the preprocessed argument list
    let preprocessed =
        match read_and_preprocess(&args.input_file, args.definitions.clone().into_iter()) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{e}");
                return std::process::ExitCode::FAILURE;
            }
        };

    // Write the preprocessed data if provided
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
                return std::process::ExitCode::FAILURE;
            }
        }
    }

    // Tokenize the input preprocessed data
    let input_tokens = {
        match preprocessed.tokenize() {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{e}");
                return std::process::ExitCode::FAILURE;
            }
        }
    };

    // Compile the program
    let cbstate = match compile(input_tokens.clone(), args.compiler_options()) {
        Ok(asm) => asm,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e.into());
            return std::process::ExitCode::FAILURE;
        }
    };

    // Prints the AST if requested
    if args.print_ast {
        println!("{}", cbstate.get_statements().join("\n"));
    }

    // Define the interface file if requested
    if let Some(interface_path) = &args.write_interface_file {
        let mut interface_file = match std::fs::File::create(interface_path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Unable to open interface file - {e}");
                return std::process::ExitCode::FAILURE;
            }
        };

        match cbstate.get_exported_interface() {
            Ok(x) => {
                // Write to the output file
                x.write_interface(&mut interface_file).unwrap();
            }
            Err(e) => {
                print_error(preprocessed.get_lines(), &e);
                return std::process::ExitCode::FAILURE;
            }
        }
    }

    // Assemble the compiled program
    let asm = match cbstate.get_assembler() {
        Ok(v) => v,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e);
            return std::process::ExitCode::FAILURE;
        }
    };

    // Output the assembly if requested
    if let Some(out) = args.output_assembly {
        match std::fs::File::create(out) {
            Ok(mut f) => {
                for l in &asm.assembly_lines {
                    writeln!(f, "{l}").unwrap();
                }
                if args.include_locations {
                    for l in &asm.assembly_debug {
                        writeln!(f, "{l}").unwrap();
                    }
                }
            }
            Err(e) => {
                eprintln!("{e}");
                return std::process::ExitCode::FAILURE;
            }
        }
    }

    // Output the binary if requested
    if let Some(out) = args.output_binary {
        match std::fs::File::create(out) {
            Ok(mut f) => f.write_all(&asm.bytes).unwrap(),
            Err(e) => {
                eprintln!("{e}");
                return std::process::ExitCode::FAILURE;
            }
        }
    }

    std::process::ExitCode::SUCCESS
}

/// Helper funciton to print an error with teh associated context
fn print_error(txt: &[PreprocessorLine], err: &CompilerError) {
    eprintln!("Error: {}", err);

    if let CompilerError::TokenError(err) = err
        && let Some(t) = &err.token
    {
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
