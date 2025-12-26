use std::{
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use cbfs::{self, CbEntryType, CbFileSystem};
use clap::Parser;
use fuser::{self, FileAttr, FileType};
use libc::{ENOENT, ENOSYS};
use time::{Date, Time};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, short)]
    base_file: Option<PathBuf>,
    #[arg(long, short, default_value_t = 0)]
    new_file_size: u64,
    #[arg(long, short)]
    mount_point: PathBuf,
}

#[derive(Debug)]
struct CbfsFuse {
    fs: CbFileSystem,
    base_file: Option<PathBuf>,
}

impl CbfsFuse {
    fn get_node(&self, ino: u64) -> Option<u16> {
        if ino == 1 {
            Some(self.fs.header.root_sector.get())
        } else if ino <= (u16::MAX as u64) {
            self.fs.get_node_valid(ino as u16).map_or(None, |x| Some(x))
        } else {
            None
        }
    }

    fn fs_type(entry: CbEntryType) -> Option<FileType> {
        match entry {
            CbEntryType::File => Some(FileType::RegularFile),
            CbEntryType::Directory => Some(FileType::Directory),
            _ => None,
        }
    }
}

impl Drop for CbfsFuse {
    fn drop(&mut self) {
        if let Some(base) = &self.base_file {
            self.fs.write_to_file(base).unwrap();
        }
    }
}

impl fuser::Filesystem for CbfsFuse {
    fn getattr(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: Option<u64>,
        reply: fuser::ReplyAttr,
    ) {
        if let Some(n) = self.get_node(ino)
            && let Ok(hdr) = self.fs.get_node_header(n)
            && let Some(fstype) = Self::fs_type(hdr.get_entry_type())
        {
            println!("getattr(ino={} -> {})", ino, n);
            let ts = SystemTime::now();
            let attr = FileAttr {
                ino: 1,
                size: hdr.get_payload_size() as u64,
                blocks: self.fs.num_sectors_for_node(n) as u64,
                atime: ts,
                mtime: ts,
                ctime: ts,
                crtime: ts,
                kind: fstype,
                perm: 0o755,
                nlink: 0,
                uid: 0,
                gid: 0,
                rdev: 0,
                flags: 0,
                blksize: self.fs.header.sector_size.get() as u32,
            };
            let ttl = Duration::from_millis(10);
            if ino == 1 {
                reply.attr(&ttl, &attr);
            } else {
                reply.error(ENOSYS);
            }
        } else {
            reply.error(ENOSYS);
        }
    }

    fn readdir(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        offset: i64,
        mut reply: fuser::ReplyDirectory,
    ) {
        println!("readdir(ino={}, fh={}, offset={})", ino, fh, offset);
        if ino == 1 {
            if offset == 0 {
                if !reply.add(1, 0, FileType::Directory, &Path::new(".")) {
                    reply.error(ENOSYS);
                    return;
                }
                if !reply.add(1, 1, FileType::Directory, &Path::new("..")) {
                    reply.error(ENOSYS);
                    return;
                }
            }
            reply.ok();
        } else {
            reply.error(ENOENT);
        }
    }
}

fn main() {
    let args = Args::parse();

    let fs = CbfsFuse {
        fs: CbFileSystem::new("test", 1024, 512).unwrap(),
        base_file: None,
    };

    fuser::mount2(fs, args.mount_point, &[]).unwrap();
}
