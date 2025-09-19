use std::{io::Write, path::PathBuf};

use cbuoy::{TokenError, parse};
use clap::Parser;
use jib_asm::assemble_tokens;

#[derive(Debug, clap::Parser)]
#[command(version, about)]
struct CompilerArguments {
    #[arg()]
    input_file: PathBuf,
    #[arg(
        short = 'p',
        long = "print",
        default_value_t = false,
        help = "Prints the generated assembly to the console"
    )]
    print_assembly: bool,
    #[arg(
        short = 'o',
        long = "output",
        help = "Creates a binary file with the generated machine code"
    )]
    output_binary: Option<PathBuf>,
    #[arg(
        short = 'j',
        long = "jib",
        help = "Creates a text file with the generated assembly code"
    )]
    output_assembly: Option<PathBuf>,
    #[arg(
        short = 'l',
        long = "locs",
        default_value_t = false,
        help = "Includes location/debugging information in output assembly code"
    )]
    include_locations: bool,
}

struct CbcOutput {
    assembly_text: Option<Vec<String>>,
}

impl CbcOutput {
    fn new(args: &CompilerArguments) -> Self {
        Self {
            assembly_text: if args.print_assembly || args.output_assembly.is_some() {
                Some(Vec::new())
            } else {
                None
            },
        }
    }

    fn add_string(&mut self, s: String) {
        if let Some(v) = self.assembly_text.as_mut() {
            v.push(s);
        }
    }
}

fn main() -> std::process::ExitCode {
    let args = CompilerArguments::parse();

    let input_text = {
        match std::fs::read_to_string(&args.input_file) {
            Ok(v) => v,
            Err(e) => {
                eprintln!(
                    "unable to open input file {} - {e}",
                    args.input_file.as_os_str().to_str().unwrap_or("?")
                );
                return 1.into();
            }
        }
    };

    let asm = match parse(&input_text) {
        Ok(asm) => asm,
        Err(e) => {
            print_error(&input_text, &e);
            return 1.into();
        }
    };

    let mut output = CbcOutput::new(&args);

    for i in &asm {
        output.add_string(format!("{}", i.tok));
    }

    let asm_out = match assemble_tokens(asm) {
        Ok(r) => {
            if args.include_locations {
                output.add_string(format!(";Program Start: {:04x}", r.start_address));
                output.add_string(";Labels:".to_string());
                let mut locs: Vec<(u32, &String)> =
                    r.labels.iter().map(|(k, v)| (*v, k)).collect::<Vec<_>>();
                locs.sort_by(|a, b| a.0.cmp(&b.0));
                for (v, k) in locs {
                    output.add_string(format!(";  {v:04x} => {k}"));
                }
                output.add_string(";Debug:".to_string());
                for (addr, cmt) in r.debug.iter() {
                    output.add_string(format!(";  {addr:04x} => {cmt}"));
                }
            }
            r
        }
        Err(e) => {
            eprintln!("{e}");
            return 2.into();
        }
    };

    if let Some(txt) = &output.assembly_text {
        if args.print_assembly {
            for l in txt.iter() {
                println!("{l}");
            }
        }

        if let Some(out) = args.output_assembly {
            match std::fs::File::create(out) {
                Ok(mut f) => {
                    for l in txt.iter() {
                        writeln!(f, "{l}").unwrap();
                    }
                }
                Err(e) => {
                    eprintln!("{e}");
                    return 3.into();
                }
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

fn print_error(txt: &str, err: &TokenError) {
    eprintln!("Error: {}", err.msg);

    if let Some(t) = &err.token {
        let line_num = t.get_loc().line;
        let display_num = line_num + 1;
        let line = txt.lines().nth(line_num).unwrap();
        eprintln!("{display_num} >> {line}");
        eprint!("{display_num}    ");
        for _ in 0..t.get_loc().column {
            eprint!(" ");
        }
        for _ in 0..t.get_value().len() {
            eprint!("^");
        }
        eprintln!();
    }
}
