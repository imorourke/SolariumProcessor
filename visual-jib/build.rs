use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("default.cb");
    let input_file = "../cbuoy/examples/os.cb";

    let output = cbuoy::read_and_preprocess(Path::new(input_file)).unwrap();

    let mut files = HashSet::new();
    for l in output.iter() {
        if files.insert(l.loc.file.clone()) {
            println!("cargo::rerun-if-changed={}", l.loc.file);
        }
    }

    let source_code = output
        .into_iter()
        .map(|x| x.text)
        .collect::<Vec<_>>()
        .join("\n");

    fs::write(&dest_path, source_code).unwrap();
}
