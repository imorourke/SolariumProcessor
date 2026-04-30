use std::{collections::HashSet, io::Write, path::PathBuf};

use cblang::{
    CodeGenerationOptions, CompilerError, PreprocessorLine, ProgramType, Type, parse,
    read_and_preprocess,
};
use clap::Parser;

#[derive(Default, Debug, Parser)]
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
        help = "Enables program generation in kernel mode with the provided start location"
    )]
    kernel_program: bool,
    #[arg(
        short = 'K',
        long = "kernel-start-loc",
        default_value_t = ProgramType::DEFAULT_START_OFFSET,
        help="Initial program location when generating in kernel mode"
    )]
    kernel_start_offset: u32,
    #[arg(
        short = 'S',
        long = "kernel-stack-loc",
        default_value_t = ProgramType::DEFAULT_STACK_LOC,
        help = "Initial stack location when generating in kernel mode",
    )]
    kernel_stack_loc: u32,
    #[arg(
        short = 't',
        long = "trim",
        default_value_t = false,
        help = "Trims unused functions and variables from the generated code"
    )]
    trim_unused: bool,
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
    #[arg(
        short = 'H',
        long = "interface-prefix",
        help = "Prefixes to include in the generation for a "
    )]
    interface_prefixes: Vec<String>,
    #[arg(
        short = 'W',
        long = "write-interface",
        help = "Writes an interface file for the provided symbols"
    )]
    write_interface_file: Option<PathBuf>,
}

impl CompilerArguments {
    fn symbol_matches(&self, s: &str) -> bool {
        self.interface_prefixes.is_empty()
            || self.interface_prefixes.iter().any(|x| s.starts_with(x))
    }
}

fn main() -> std::process::ExitCode {
    let args = CompilerArguments::parse();

    let preprocessed =
        match read_and_preprocess(&args.input_file, args.definitions.clone().into_iter()) {
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
        CodeGenerationOptions {
            prog_type: if args.kernel_program {
                ProgramType::Kernel {
                    stack_loc: args.kernel_stack_loc,
                    start_offset: args.kernel_start_offset,
                }
            } else {
                ProgramType::Application
            },
            debug_locations: args.include_locations,
            trim_code: args.trim_unused,
        },
    ) {
        Ok(asm) => asm,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e.into());
            return 1.into();
        }
    };

    if args.print_ast {
        println!("{}", cbstate.get_statements().join("\n"));
    }

    if let Some(interface_path) = &args.write_interface_file {
        let mut interface_file = match std::fs::File::create(interface_path) {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Unable to open interface file - {e}");
                return 1.into();
            }
        };

        match cbstate.get_exported_interface() {
            Ok(mut x) => {
                // Add structures in an order that allows the structures to be instantiated
                let mut any_added = true;
                let mut structs_added: HashSet<String> = HashSet::new();

                fn contains_type(known_types: &HashSet<String>, current: &Type) -> bool {
                    match current {
                        Type::Pointer(x) => contains_type(known_types, x.as_ref()),
                        Type::Struct(x) => known_types.contains(x.get_name()),
                        Type::Array(_, x) => contains_type(known_types, x.as_ref()),
                        Type::Const(x) => contains_type(known_types, x.as_ref()),
                        Type::Function(f) => {
                            let mut matches_types = true;
                            if let Some(x) = &f.return_type
                                && !contains_type(known_types, x)
                            {
                                matches_types = false;
                            }

                            for p in f.parameters.iter() {
                                if !contains_type(known_types, &p.dtype) {
                                    matches_types = false;
                                    break;
                                }
                            }

                            matches_types
                        }
                        Type::Opaque(x) => contains_type(known_types, &x.get_type(true).unwrap()),
                        Type::Primitive(_) => true,
                    }
                }

                while any_added {
                    any_added = false;
                    'struct_loop: for s in x.structs.iter() {
                        if structs_added.contains(&s.name) {
                            continue;
                        }

                        let mut tmp_added = structs_added.clone();
                        tmp_added.insert(s.name.clone());

                        for t in s.def.get_fields().values() {
                            if !contains_type(&tmp_added, &t.dtype) {
                                continue 'struct_loop;
                            }
                        }

                        writeln!(interface_file, "struct {} {{", s.name).unwrap();

                        let mut fields = s.def.get_fields().iter().collect::<Vec<_>>();
                        fields.sort_by_key(|(_, x)| x.offset);

                        for (name, field) in fields {
                            writeln!(interface_file, "    {}: {};", name, field.dtype).unwrap();
                        }

                        writeln!(interface_file, "}}").unwrap();

                        structs_added.insert(s.name.clone());
                        any_added = true;
                    }
                }

                // Add any matching constants
                x.consts.sort_by(|a, b| a.name.cmp(&b.name));
                for c in x.consts {
                    if args.symbol_matches(&c.name) {
                        writeln!(
                            interface_file,
                            "global {}: {} = {};",
                            c.name,
                            c.value.get_value().get_dtype(),
                            c.value
                        )
                        .unwrap();
                    }
                }

                // Add the output functions
                x.functions.sort_by(|a, b| a.name.cmp(&b.name));
                for f in x.functions {
                    if args.symbol_matches(&f.name) {
                        writeln!(
                            interface_file,
                            "global {}: fn({}) {} = {}u32;",
                            f.name,
                            f.def
                                .parameters
                                .iter()
                                .map(|x| x.dtype.to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                            f.def
                                .return_type
                                .map(|x| x.to_string())
                                .unwrap_or("void".into()),
                            f.loc,
                        )
                        .unwrap();
                    }
                }
            }
            Err(e) => {
                print_error(preprocessed.get_lines(), &e);
                return 3.into();
            }
        }
    }

    let asm = match cbstate.get_assembler() {
        Ok(v) => v,
        Err(e) => {
            print_error(preprocessed.get_lines(), &e);
            return 1.into();
        }
    };

    if args.print_assembly {
        for l in &asm.assembly_lines {
            println!("{l}");
        }
    }

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
                return 3.into();
            }
        }
    }

    if let Some(out) = args.output_binary {
        match std::fs::File::create(out) {
            Ok(mut f) => f.write_all(&asm.bytes).unwrap(),
            Err(e) => {
                eprintln!("{e}");
                return 4.into();
            }
        }
    }

    std::process::ExitCode::SUCCESS
}

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
