use std::{
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

use cbfs::{self, CbEntryHeader, CbEntryType, CbFileSystem, CbfsError, string_to_array};
use clap::Parser;
use fuser::{self, FileAttr, FileType};
use libc::{ENOENT, ENOSYS, ENOTDIR, S_IFREG};

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
        short = 'n',
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
    volume_name: Option<String>,
    #[arg(help = "the path to mount the filesystem to")]
    mount_point: PathBuf,
    #[arg(short, long, help = "show verbose statistics", default_value_t = false)]
    verbose: bool,
}

#[derive(Debug)]
struct CbfsFuse {
    fs: CbFileSystem,
    base_file: Option<PathBuf>,
    uid: u32,
    gid: u32,
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

    fn fs_type(entry: CbEntryType) -> Result<FileType, CbFuseErr> {
        match entry {
            CbEntryType::File => Ok(FileType::RegularFile),
            CbEntryType::Directory => Ok(FileType::Directory),
            _ => Err(CbFuseErr::Other),
        }
    }

    fn get_fs_attr_ino(&self, ino: u64) -> Result<FileAttr, CbFuseErr> {
        let n = self.get_node(ino);
        let hdr = self.fs.get_node_header(n)?;
        let fstype = Self::fs_type(hdr.get_entry_type())?;
        let ts = SystemTime::now();
        Ok(FileAttr {
            ino,
            size: hdr.get_payload_size() as u64,
            blocks: self.fs.num_sectors_for_node(n) as u64,
            atime: ts,
            mtime: ts,
            ctime: ts,
            crtime: ts,
            kind: fstype,
            perm: 0o755,
            nlink: 0,
            uid: self.uid,
            gid: self.gid,
            rdev: 0,
            flags: 0,
            blksize: self.fs.header.sector_size.get() as u32,
        })
    }

    fn save_fs(&self) {
        if let Some(base) = &self.base_file {
            self.fs.write_to_file(base).unwrap();
        }
    }

    fn get_node_from_parent(&self, parent: u64, name: &str) -> Result<u16, CbFuseErr> {
        let entries = self.fs.get_directory_listing(self.get_node(parent))?;
        for e in entries.iter().copied() {
            let hdr = self.fs.get_node_header(e)?;
            if hdr.get_name() == name {
                return Ok(e);
            }
        }

        Err(CbFuseErr::NoEntry)
    }

    fn delete_node(&mut self, node: u16, tval: Option<CbEntryType>) -> Result<(), CbFuseErr> {
        if let Ok(hdr) = self.fs.get_node_header(node)
            && (tval.is_none() || (hdr.get_entry_type() == tval.unwrap()))
        {
            self.fs.rmnode(node)?;
            Ok(())
        } else {
            Err(CbFuseErr::NoEntry)
        }
    }
}

impl Drop for CbfsFuse {
    fn drop(&mut self) {
        self.save_fs();
    }
}

impl fuser::Filesystem for CbfsFuse {
    fn init(
        &mut self,
        req: &fuser::Request<'_>,
        _config: &mut fuser::KernelConfig,
    ) -> Result<(), libc::c_int> {
        self.uid = req.uid();
        self.gid = req.gid();
        Ok(())
    }

    fn create(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        _mode: u32,
        _umask: u32,
        _flags: i32,
        reply: fuser::ReplyCreate,
    ) {
        match self.fs.mkentryn(
            self.get_node(parent),
            name.to_str().unwrap(),
            CbEntryType::File,
            &[],
        ) {
            Ok(new_node) => match self.get_fs_attr_ino(self.get_ino(new_node)) {
                Ok(attr) => {
                    reply.created(&Duration::from_secs(5), &attr, 0, 0, 0);
                }
                Err(err) => {
                    reply.error(err.get_code());
                }
            },
            Err(err) => {
                reply.error(CbFuseErr::from(err).get_code());
            }
        }
    }

    fn getattr(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: Option<u64>,
        reply: fuser::ReplyAttr,
    ) {
        match self.get_fs_attr_ino(ino) {
            Ok(attr) => {
                let ttl = Duration::from_secs(1);
                reply.attr(&ttl, &attr);
            }
            Err(e) => {
                reply.error(e.get_code());
            }
        }
    }

    fn setattr(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _mode: Option<u32>,
        _uid: Option<u32>,
        _gid: Option<u32>,
        size: Option<u64>,
        _atime: Option<fuser::TimeOrNow>,
        _mtime: Option<fuser::TimeOrNow>,
        _ctime: Option<SystemTime>,
        _fh: Option<u64>,
        _crtime: Option<SystemTime>,
        _chgtime: Option<SystemTime>,
        _bkuptime: Option<SystemTime>,
        _flags: Option<u32>,
        reply: fuser::ReplyAttr,
    ) {
        if let Some(new_size) = size
            && self
                .fs
                .set_node_byte_size(self.get_node(ino), new_size as u32)
                .is_err()
        {
            reply.error(ENOSYS);
            return;
        }

        reply.attr(&Duration::from_secs(5), &self.get_fs_attr_ino(ino).unwrap());
    }

    fn lookup(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEntry,
    ) {
        match self.get_node_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.get_fs_attr_ino(self.get_ino(node)) {
                Ok(attr) => {
                    reply.entry(&Duration::from_secs(10), &attr, 0);
                }
                Err(err) => reply.error(err.get_code()),
            },
            Err(err) => reply.error(err.get_code()),
        }
    }

    fn fsync(
        &mut self,
        _req: &fuser::Request<'_>,
        _ino: u64,
        _fh: u64,
        _datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        self.save_fs();
        reply.ok();
    }

    fn fsyncdir(
        &mut self,
        _req: &fuser::Request<'_>,
        _ino: u64,
        _fh: u64,
        _datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        self.save_fs();
        reply.ok();
    }

    fn flush(
        &mut self,
        _req: &fuser::Request<'_>,
        _ino: u64,
        _fh: u64,
        _lock_owner: u64,
        reply: fuser::ReplyEmpty,
    ) {
        reply.ok();
    }

    fn mknod(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        mode: u32,
        _umask: u32,
        _rdev: u32,
        reply: fuser::ReplyEntry,
    ) {
        const FILE_MODE_VAL: u32 = S_IFREG as u32;
        if (mode & FILE_MODE_VAL) == FILE_MODE_VAL {
            match self.fs.mkentryn(
                self.get_node(parent),
                name.to_str().unwrap(),
                CbEntryType::File,
                &[],
            ) {
                Ok(new_node) => match self.get_fs_attr_ino(self.get_ino(new_node)) {
                    Ok(attr) => {
                        reply.entry(&Duration::from_secs(5), &attr, 0);
                    }
                    Err(err) => {
                        reply.error(err.get_code());
                    }
                },
                Err(err) => {
                    reply.error(CbFuseErr::from(err).get_code());
                }
            }
        } else {
            reply.error(ENOSYS);
        }
    }

    fn mkdir(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        _mode: u32,
        _umask: u32,
        reply: fuser::ReplyEntry,
    ) {
        println!("{}:{}", _req.uid(), _req.gid());
        let pnode = self.get_node(parent);
        match self
            .fs
            .mkentryn(pnode, name.to_str().unwrap(), CbEntryType::Directory, &[])
        {
            Ok(new_node) => match self.get_fs_attr_ino(self.get_ino(new_node)) {
                Ok(attr) => reply.entry(&Duration::from_secs(1), &attr, 0),
                Err(err) => {
                    reply.error(err.get_code());
                }
            },
            Err(err) => {
                reply.error(CbFuseErr::from(err).get_code());
            }
        }
    }

    fn readdir(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: fuser::ReplyDirectory,
    ) {
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

            let all_dirs = parent_dirs.into_iter().chain(dirs).collect::<Vec<_>>();

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

    fn unlink(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        match self.get_node_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.delete_node(node, Some(CbEntryType::File)) {
                Ok(()) => reply.ok(),
                Err(err) => reply.error(err.get_code()),
            },
            Err(err) => reply.error(err.get_code()),
        }
    }

    fn rmdir(
        &mut self,
        _req: &fuser::Request<'_>,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        match self.get_node_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.delete_node(node, Some(CbEntryType::Directory)) {
                Ok(()) => {
                    println!("Success!");
                    reply.ok();
                }
                Err(err) => {
                    println!("Error 2: {:?}", err);
                    reply.error(err.get_code());
                }
            },
            Err(err) => {
                println!("Error 1");
                reply.error(err.get_code());
            }
        }
    }

    fn read(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyData,
    ) {
        if let Ok((hdr, data)) = self.fs.get_node_data(self.get_node(ino))
            && hdr.get_entry_type() == CbEntryType::File
        {
            if offset < 0 {
                reply.error(ENOSYS);
            } else {
                reply.data(
                    &data[(offset as usize)..(offset as usize + size as usize).min(data.len())],
                );
            }
        } else {
            reply.error(ENOENT);
        }
    }

    fn write(
        &mut self,
        _req: &fuser::Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        data: &[u8],
        _write_flags: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyWrite,
    ) {
        if let Ok((hdr, mut fdata)) = self.fs.get_node_data(self.get_node(ino)) {
            if offset < 0 {
                reply.error(ENOSYS);
            } else {
                let req_size = (offset as u32 + data.len() as u32) as usize;
                if hdr.get_payload_size() < req_size {
                    fdata.resize(req_size, 0);
                }

                for (dst, src) in fdata
                    .iter_mut()
                    .skip(offset as usize)
                    .take(data.len())
                    .zip(data.iter())
                {
                    *dst = *src
                }

                if self
                    .fs
                    .set_entry_data(self.get_node(ino), hdr, &fdata)
                    .is_ok()
                {
                    reply.written(data.len() as u32);
                } else {
                    reply.error(ENOSYS);
                }
            }
        } else {
            reply.error(ENOSYS);
        }
    }

    fn statfs(&mut self, _req: &fuser::Request<'_>, _ino: u64, reply: fuser::ReplyStatfs) {
        let free_sectors = self.fs.num_free_sectors();
        let num_entries = match self.fs.num_entries(self.fs.header.root_sector.get()) {
            Ok(count) => count,
            Err(e) => {
                reply.error(CbFuseErr::from(e).get_code());
                return;
            }
        };

        reply.statfs(
            self.fs.header.sector_count.get() as u64,
            free_sectors as u64,
            free_sectors as u64,
            num_entries as u64,
            free_sectors as u64,
            self.fs.header.sector_size.get() as u32,
            CbEntryHeader::NAME_SIZE as u32,
            self.fs.header.sector_size.get() as u32,
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CbFuseErr {
    Other,
    NotDirectory,
    NoEntry,
}

impl CbFuseErr {
    fn get_code(&self) -> i32 {
        match self {
            Self::NotDirectory => ENOTDIR,
            Self::NoEntry => ENOENT,
            _ => ENOSYS,
        }
    }
}

impl From<CbfsError> for CbFuseErr {
    fn from(value: CbfsError) -> Self {
        match value {
            CbfsError::PathNotFound(_) => Self::NoEntry,
            CbfsError::InvalidEntry(_) => Self::NoEntry,
            CbfsError::EntryNotDirectory => Self::NotDirectory,
            _ => Self::Other,
        }
    }
}

fn main() {
    let args = Args::parse();

    if args.verbose {
        simple_logger::SimpleLogger::new().init().unwrap();
    }

    let fs = if let Some(orig) = &args.base_file
        && orig.exists()
    {
        CbfsFuse {
            fs: CbFileSystem::open(orig).unwrap(),
            base_file: Some(orig.to_path_buf()),
            uid: 0,
            gid: 0,
        }
    } else {
        CbfsFuse {
            fs: CbFileSystem::new(
                args.volume_name.as_ref().map(|x| x.as_ref()).unwrap_or(""),
                args.sector_size,
                args.sector_num,
            )
            .unwrap(),
            base_file: args.base_file,
            uid: 0,
            gid: 0,
        }
    };

    fuser::mount2(fs, args.mount_point, &[]).unwrap();
}
