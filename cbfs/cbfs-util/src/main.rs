use cbfs_lib::{
    CbContainerHeader, CbContainerOptions, CbEntryType, CbError, CbFileSystem, open_container,
    save_container,
};
use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Arguments {
    #[command(subcommand)]
    options: CommandOptions,
}

#[derive(Debug, Subcommand)]
enum CommandOptions {
    Create(CreateOptions),
    Modify(ModifyOptions),
    List(ListOptions),
    Info(InfoOptions),
}

#[derive(Debug, Parser)]
struct CreateOptions {
    file: PathBuf,
    #[arg(
        short = 's',
        long = "secsize",
        help = "sector size in bytes",
        default_value_t = 256
    )]
    secsize: u16,
    #[arg(
        short = 'S',
        long = "seccount",
        help = "sector count",
        default_value_t = 32768
    )]
    seccount: u16,
    #[arg(short = 'g', long = "gz", help = "enable gzip compression")]
    gzip: bool,
    #[arg(short = 'c', long = "sparse", help = "enable sparse file support")]
    sparse: bool,
    #[arg(short = 'n', long = "name", help = "filesystem name")]
    name: Option<String>,
}

#[derive(Debug, Parser)]
struct ModifyOptions {
    #[arg(help = "image file to modify")]
    file: PathBuf,
    #[arg(short = 'g', long = "gz", help = "enable gzip compression")]
    gzip: bool,
    #[arg(short = 'c', long = "sparse", help = "enable sparse file support")]
    sparse: bool,
    #[arg(short = 'z', long = "zero", help = "zero unused sectors")]
    zero: bool,
    #[arg(short = 'n', long = "name", help = "filesystem name")]
    name: Option<String>,
}

#[derive(Debug, Parser)]
struct ListOptions {
    #[arg(help = "image file to read")]
    file: PathBuf,
    #[arg(short = 'f', long = "files", help = "list contained files")]
    files: bool,
    #[arg(short = 'F', long = "folders", help = "list contained folder")]
    folders: bool,
}

#[derive(Debug, Parser)]
struct InfoOptions {
    #[arg(help = "image file to read")]
    file: PathBuf,
}

fn main() {
    let args = Arguments::parse();
    match args.options {
        CommandOptions::Create(opt) => {
            let name = opt.name.as_deref().unwrap_or("cbfs");
            let fs = CbFileSystem::new(name, opt.secsize, opt.seccount)
                .expect("unable to create filesystem");
            let header =
                CbContainerHeader::new(CbContainerOptions::from_flags(opt.sparse, opt.gzip));
            save_container(&header, &fs, &opt.file).expect("unable to write fs to a file");
        }
        CommandOptions::Modify(opt) => {
            let (mut header, mut filesystem) =
                open_container(&opt.file).expect("unable to open file");
            header.set_compressed(opt.gzip);
            header.set_sparse(opt.sparse);
            if opt.zero {
                filesystem
                    .zero_unused_sectors()
                    .expect("unable to zero sectors");
            }
            if let Some(n) = opt.name.as_ref() {
                filesystem
                    .set_vol_name(n)
                    .expect("unable to set filesystem name");
            }
            save_container(&header, &filesystem, &opt.file).expect("unable to write fs to file");
        }
        CommandOptions::List(opt) => {
            let (_, fs) = open_container(&opt.file).expect("unable to open file");

            fn folder_entry_vals(
                path_so_far: &str,
                fs: &CbFileSystem,
                node: u16,
                opt: &ListOptions,
            ) -> Result<(), CbError> {
                if opt.folders {
                    if node == fs.root_sector() {
                        println!("/");
                    } else {
                        println!("{path_so_far}");
                    }
                }
                for n in fs.directory_listing(node)? {
                    match n.get_entry_type() {
                        CbEntryType::File => {
                            if opt.files {
                                println!("{path_so_far}/{}", n.get_name())
                            }
                        }
                        CbEntryType::Directory => {
                            folder_entry_vals(
                                &format!("{path_so_far}/{}", n.get_name()),
                                fs,
                                n.base_block.get(),
                                opt,
                            )?;
                        }
                        _ => (),
                    }
                }

                Ok(())
            }

            folder_entry_vals("", &fs, fs.root_sector(), &opt).expect("unable to list entries")
        }
        CommandOptions::Info(opt) => {
            let (header, filesystem) = open_container(&opt.file).expect("unable to open file");

            println!("Container:");
            if header.is_compressed() {
                println!("  Compressed");
            }
            if header.is_sparse() {
                println!("  Sparse");
            }
            println!("Sector Info:",);

            println!("  {} byte sectors", filesystem.sector_size(),);
            println!("  {} sectors", filesystem.sector_count(),);

            println!(
                "  {} total bytes",
                filesystem.sector_size() as u32 * filesystem.sector_count() as u32
            );

            println!(
                "  {} / {} free sectors",
                filesystem.num_free_sectors(),
                filesystem.sector_count()
            );
            println!("  {} root entries", filesystem.num_primary_entries(),)
        }
    }
}
