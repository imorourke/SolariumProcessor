use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_env = env::var_os("OUT_DIR").unwrap();
    let out_dir = Path::new(out_env.to_str().unwrap());

    let input_files_base = [
        "../cbuoy/examples/os.cb",
        "../cbuoy/examples/default.cb",
        "../cbuoy/examples/tests/test_kmalloc.cb",
    ];
    let mut required_files = HashSet::new();

    for in_file in input_files_base {
        let output = cbuoy::read_and_preprocess(Path::new(in_file)).unwrap();
        let out_file = Path::join(
            out_dir,
            Path::new(Path::new(in_file).file_name().unwrap().to_str().unwrap()),
        );

        for l in output.iter() {
            required_files.insert(l.loc.file.clone());
        }

        let source_code = output
            .into_iter()
            .map(|x| x.text)
            .collect::<Vec<_>>()
            .join("\n");

        fs::write(&out_file, source_code.trim()).unwrap();
    }

    for f in required_files {
        println!("cargo::rerun-if-changed={}", f);
    }
}
