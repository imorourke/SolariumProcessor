mod filesystem;
mod fserr;

use std::path::PathBuf;

use cbfs_lib::CbFileSystem;
use clap::Parser;

use crate::filesystem::{CbfsFuse, SaveFileOptions};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(help = "the path to mount the filesystem to")]
    mount: PathBuf,
    #[arg(
        long,
        short,
        help = "file to read from (if present on mount) or write/create to save filesystem to when unmounted (unless the read-only base is set)"
    )]
    base_file: Option<PathBuf>,
    #[arg(
        long,
        short,
        help = "if set, will only read the base file, but will not write to it"
    )]
    read_only_base: bool,
    #[arg(long, short, help = "zeros out any unused sectors on close")]
    zero_unused: bool,
    #[arg(
        long,
        short = 'S',
        default_value_t = 1024,
        help = "the number of sectors to create if a a filesystem is loaded"
    )]
    sector_num: u16,
    #[arg(
        long,
        short = 's',
        default_value_t = 512,
        help = "the size of each sector in bytes to use if a new filesystem is created"
    )]
    sector_size: u16,
    #[arg(
        long,
        short,
        help = "the name of the volume to use if a new filesystem is created"
    )]
    name: Option<String>,
    #[arg(short, long, help = "show verbose statistics")]
    verbose: bool,
    #[arg(
        long,
        help = "save generated file as a sparse file (only applies to new files without the override flag)"
    )]
    sparse: bool,
    #[arg(
        short,
        long,
        help = "save generated file as a gzip file (only applies to new files without the override flag)"
    )]
    gzip: bool,
    #[arg(
        long,
        help = "overrides the loaded file options from a loaded cbfs file with the ones provide via command line"
    )]
    save_option_override: bool,
}

impl Args {
    fn save_options(&self) -> SaveFileOptions {
        SaveFileOptions {
            zero_unused: self.zero_unused,
            save_gzip: self.gzip,
            save_sparse: self.sparse,
            read_only_base: self.read_only_base,
            override_save_options: self.save_option_override,
        }
    }
}

fn main() {
    let args = Args::parse();

    if args.verbose {
        simple_logger::SimpleLogger::new().init().unwrap();
    }

    let mut save_options = args.save_options();

    let fs = if let Some(orig) = &args.base_file
        && orig.exists()
    {
        let (hdr, fs) = CbFileSystem::open(orig).unwrap();

        if !save_options.override_save_options {
            save_options.save_gzip = hdr.is_compressed();
            save_options.save_sparse = hdr.is_sparse();
        }

        CbfsFuse::new(fs, Some(orig), save_options)
    } else {
        CbfsFuse::new(
            CbFileSystem::new(
                args.name.as_ref().map(|x| x.as_ref()).unwrap_or(""),
                args.sector_size,
                args.sector_num,
            )
            .unwrap(),
            args.base_file.as_deref(),
            save_options,
        )
    };

    fuser::mount2(fs, args.mount, &[]).unwrap();
}
