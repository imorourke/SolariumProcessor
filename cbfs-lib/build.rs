extern crate cbindgen;

use std::env;

use cbindgen::Config;

fn main() {
    let crate_dir = env::var("CARGO_MANIFEST_DIR").unwrap();

    let mut cfg = Config::default();
    cfg.usize_is_size_t = true;

    cbindgen::Builder::new()
        .with_crate(crate_dir)
        .with_config(cfg)
        .with_include_guard("CBFS_LIB")
        .generate()
        .expect("Unable to generate bindings")
        .write_to_file("gen/cbfs.h");
}
