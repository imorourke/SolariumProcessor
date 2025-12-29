mod filesystem;
mod fserr;

use std::path::PathBuf;

use cbfs::CbFileSystem;
use clap::Parser;

use crate::filesystem::CbfsFuse;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(
        long,
        short,
        help = "file to read from (if present on mount) or write/create to save filesystem to when unmounted"
    )]
    base_file: Option<PathBuf>,
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
    #[arg(help = "the path to mount the filesystem to")]
    mount: PathBuf,
    #[arg(short, long, help = "show verbose statistics", default_value_t = false)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    if args.verbose {
        simple_logger::SimpleLogger::new().init().unwrap();
    }

    let fs = if let Some(orig) = &args.base_file
        && orig.exists()
    {
        CbfsFuse::new(CbFileSystem::open(orig).unwrap(), Some(orig))
    } else {
        CbfsFuse::new(
            CbFileSystem::new(
                args.name.as_ref().map(|x| x.as_ref()).unwrap_or(""),
                args.sector_size,
                args.sector_num,
            )
            .unwrap(),
            args.base_file.as_deref(),
        )
    };

    fuser::mount2(fs, args.mount, &[]).unwrap();
}
