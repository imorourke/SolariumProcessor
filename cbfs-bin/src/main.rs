use std::{
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use cbfs::{self, CbEntryHeader, CbEntryType, CbFileSystem, string_to_array};
use clap::Parser;
use fuser::{self, FileAttr, FileType};
use libc::{ENOENT, ENOSYS};

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
    fn get_node(&self, ino: u64) -> u16 {
        if ino == 1 {
            self.fs.header.root_sector.get()
        } else if ino <= (u16::MAX as u64) {
            self.fs.get_node_valid(ino as u16).map_or(0, |x| x)
        } else {
            0
        }
    }

    fn get_ino(&self, node: u16) -> u64 {
        if node == self.fs.header.root_sector.get() {
            1
        } else {
            node as u64
        }
    }

    fn fs_type(entry: CbEntryType) -> Option<FileType> {
        match entry {
            CbEntryType::File => Some(FileType::RegularFile),
            CbEntryType::Directory => Some(FileType::Directory),
            _ => None,
        }
    }

    fn get_attr(&self, ino: u64) -> Option<FileAttr> {
        let n = self.get_node(ino);
        if let Ok(hdr) = self.fs.get_node_header(n)
            && let Some(fstype) = Self::fs_type(hdr.get_entry_type())
        {
            let ts = SystemTime::now();
            Some(FileAttr {
                ino: ino,
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
            })
        } else {
            None
        }
    }

    fn save_fs(&self) {
        if let Some(base) = &self.base_file {
            self.fs.write_to_file(base).unwrap();
        }
    }
}

impl Drop for CbfsFuse {
    fn drop(&mut self) {
        println!("Dropping!");
        self.save_fs();
    }
}

impl fuser::Filesystem for CbfsFuse {
    fn getattr(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: Option<u64>,
        reply: fuser::ReplyAttr,
    ) {
        let n = self.get_node(ino);
        if let Some(attr) = self.get_attr(ino) {
            println!("getattr(ino={} -> {})", ino, n);
            let ttl = Duration::from_secs(1);
            if ino == 1 {
                reply.attr(&ttl, &attr);
            } else {
                reply.error(ENOSYS);
            }
        } else {
            println!("getattr(ino={})", ino);
            reply.error(ENOSYS);
        }
    }

    fn read(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        offset: i64,
        size: u32,
        flags: i32,
        lock_owner: Option<u64>,
        reply: fuser::ReplyData,
    ) {
        println!("read(ino={ino})");
    }

    fn create(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        mode: u32,
        umask: u32,
        flags: i32,
        reply: fuser::ReplyCreate,
    ) {
        println!("create(parent={parent}, name={})", name.to_str().unwrap());
    }

    fn unlink(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        println!("unlink(parent={parent}, name={})", name.to_str().unwrap());
    }

    fn write(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        offset: i64,
        data: &[u8],
        write_flags: u32,
        flags: i32,
        lock_owner: Option<u64>,
        reply: fuser::ReplyWrite,
    ) {
        println!("write(ino={ino})");
    }

    fn setxattr(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        name: &std::ffi::OsStr,
        _value: &[u8],
        flags: i32,
        position: u32,
        reply: fuser::ReplyEmpty,
    ) {
        println!("setxattr(ino={ino})");
    }

    fn flush(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        lock_owner: u64,
        reply: fuser::ReplyEmpty,
    ) {
        println!("flush(ino={ino})");
    }

    fn forget(&mut self, _req: &fuser::Request<'_>, ino: u64, _nlookup: u64) {
        println!("forget(ino={ino}");
    }

    fn fsync(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        println!("fsync(ino={ino})");
        self.save_fs();
        reply.ok();
    }

    fn fsyncdir(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        fh: u64,
        datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        println!("fsyncdir(ino={ino})");
        self.save_fs();
        reply.ok();
    }

    fn mknod(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        mode: u32,
        umask: u32,
        rdev: u32,
        reply: fuser::ReplyEntry,
    ) {
        println!("mknod(parent={parent})");
    }

    fn statfs(&mut self, _req: &fuser::Request<'_>, ino: u64, reply: fuser::ReplyStatfs) {
        println!("stat(ino={ino}");
    }

    fn mkdir(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        mode: u32,
        umask: u32,
        reply: fuser::ReplyEntry,
    ) {
        println!("mkdir(parent={}, name={})", parent, name.to_str().unwrap());
        let pnode = self.get_node(parent);
        let val = self
            .fs
            .mkentryn(pnode, name.to_str().unwrap(), CbEntryType::Directory, &[]);
        if let Ok(new_node) = val {
            reply.entry(
                &Duration::from_secs(1),
                &self.get_attr(self.get_ino(new_node)).unwrap(),
                0,
            );
        } else if let Err(err) = val {
            eprintln!("  mkdir error {err:?}");
            reply.error(ENOSYS);
        }
    }

    fn open(&mut self, _req: &fuser::Request<'_>, ino: u64, _flags: i32, reply: fuser::ReplyOpen) {
        println!("open(ino={ino})");
    }

    fn rmdir(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        println!("rmdir(ino={parent}, name={})", name.to_str().unwrap());
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
        let n = self.get_node(ino);
        if let Ok(hdr) = self.fs.get_node_header(n)
            && hdr.get_entry_type() == CbEntryType::Directory
        {
            let dirs = self
                .fs
                .get_directory_listing(n)
                .unwrap()
                .into_iter()
                .map(|h| (h as u64, self.fs.get_node_header(h).unwrap()))
                .collect::<Vec<_>>();

            let parent_dirs = [
                (
                    n as u64,
                    CbEntryHeader {
                        attributes: hdr.attributes,
                        entry_type: hdr.entry_type,
                        modification_time: hdr.payload_size,
                        name: string_to_array(".").unwrap(),
                        parent: hdr.parent,
                        payload_size: hdr.payload_size,
                    },
                ),
                (
                    self.get_ino(hdr.parent.get()),
                    CbEntryHeader {
                        attributes: 0,
                        entry_type: CbEntryType::Directory as u8,
                        modification_time: 0.into(),
                        name: string_to_array("..").unwrap(),
                        parent: 0.into(),
                        payload_size: 0.into(),
                    },
                ),
            ];

            let all_dirs = parent_dirs
                .into_iter()
                .chain(dirs.into_iter())
                .collect::<Vec<_>>();

            for (i, (n, d)) in all_dirs.iter().enumerate() {
                println!("  {i} => [{n}] :: {}", d.get_name());
            }

            if offset == 0 {
                for (i, (node, hdr)) in all_dirs.into_iter().enumerate() {
                    if reply.add(
                        node,
                        i as i64 + 2,
                        Self::fs_type(hdr.get_entry_type()).unwrap(),
                        Path::new(&hdr.get_name()),
                    ) {
                        reply.ok();
                        return;
                    }
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

    let fs = if let Some(orig) = &args.base_file
        && orig.exists()
    {
        CbfsFuse {
            fs: CbFileSystem::open(orig).unwrap(),
            base_file: Some(orig.to_path_buf()),
        }
    } else {
        CbfsFuse {
            fs: CbFileSystem::new("test", 1024, 512).unwrap(),
            base_file: args.base_file,
        }
    };

    fuser::mount2(fs, args.mount_point, &[]).unwrap();
}
