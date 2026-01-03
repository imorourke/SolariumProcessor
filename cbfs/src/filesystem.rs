use std::{
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::fserr::CbFuseErr;
use cbfs_lib::{CbDateTime, CbEntryHeader, CbEntryType, CbFileSystem, string_to_array};
use fuser::{self, FileAttr, FileType};
use libc::{ENOENT, ENOSYS, S_IFREG};

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct SaveFileOptions {
    pub zero_unused: bool,
    pub save_gzip: bool,
    pub save_sparse: bool,
    pub read_only_base: bool,
    pub override_save_options: bool,
}

#[derive(Debug)]
pub struct CbfsFuse {
    fs: CbFileSystem,
    base_file: Option<PathBuf>,
    save_options: SaveFileOptions,
}

impl CbfsFuse {
    const ROOT_INO: u64 = 1;

    pub fn new(fs: CbFileSystem, base_file: Option<&Path>, save_options: SaveFileOptions) -> Self {
        Self {
            fs,
            base_file: base_file.map(|x| x.to_owned()),
            save_options,
        }
    }

    fn get_entry_id(&self, ino: u64) -> u16 {
        if ino == Self::ROOT_INO {
            self.fs.header.root_sector.get()
        } else if ino <= (u16::MAX as u64) {
            self.fs.get_entry_valid(ino as u16).map_or(0, |x| x)
        } else {
            0
        }
    }

    fn get_ino(&self, entry: u16) -> u64 {
        if entry == self.fs.header.root_sector.get() {
            Self::ROOT_INO
        } else {
            entry as u64
        }
    }

    fn fs_type(entry: CbEntryType) -> Result<FileType, CbFuseErr> {
        match entry {
            CbEntryType::File => Ok(FileType::RegularFile),
            CbEntryType::Directory => Ok(FileType::Directory),
            _ => Err(CbFuseErr::Other),
        }
    }

    fn get_fs_attr_ino(&self, req: &fuser::Request, ino: u64) -> Result<FileAttr, CbFuseErr> {
        let n = self.get_entry_id(ino);
        let hdr = self.fs.entry_header(n)?;

        let ts: SystemTime = hdr.get_modification_time().try_into().unwrap_or(UNIX_EPOCH);
        Ok(FileAttr {
            ino,
            size: hdr.get_payload_size() as u64,
            blocks: self.fs.num_sectors_for_entry(n) as u64,
            atime: ts,
            mtime: ts,
            ctime: ts,
            crtime: ts,
            kind: Self::fs_type(hdr.get_entry_type())?,
            perm: 0o755,
            nlink: 0,
            uid: req.uid(),
            gid: req.gid(),
            rdev: 0,
            flags: 0,
            blksize: self.fs.header.sector_size.get() as u32,
        })
    }

    pub fn save_fs(&mut self) -> Result<(), CbFuseErr> {
        if !self.save_options.read_only_base
            && let Some(base) = &self.base_file
        {
            if self.save_options.zero_unused {
                self.fs.zero_unused_sectors().unwrap();
            }

            match self.fs.write_fs_to_file(
                base,
                self.save_options.save_sparse,
                self.save_options.save_gzip,
            ) {
                Ok(_) => Ok(()),
                Err(e) => Err(CbFuseErr::from(e)),
            }
        } else {
            Ok(())
        }
    }

    fn get_entry_from_parent(&self, parent: u64, name: &str) -> Result<u16, CbFuseErr> {
        let entries = self.fs.directory_listing(self.get_entry_id(parent))?;
        for e in entries.iter().copied() {
            let hdr = self.fs.entry_header(e)?;
            if hdr.get_name() == name {
                return Ok(e);
            }
        }

        Err(CbFuseErr::NoEntry)
    }

    fn delete_entry(&mut self, node: u16, tval: Option<CbEntryType>) -> Result<(), CbFuseErr> {
        if let Ok(hdr) = self.fs.entry_header(node)
            && (tval.is_none() || (hdr.get_entry_type() == tval.unwrap()))
        {
            self.fs.delete_entry(node)?;
            Ok(())
        } else {
            Err(CbFuseErr::NoEntry)
        }
    }
}

impl Drop for CbfsFuse {
    fn drop(&mut self) {
        self.save_fs().unwrap();
    }
}

impl fuser::Filesystem for CbfsFuse {
    fn setvolname(
        &mut self,
        _req: &fuser::Request<'_>,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        match self.fs.header.set_name(name.to_str().unwrap()) {
            Ok(()) => reply.ok(),
            Err(err) => reply.error(CbFuseErr::from(err).get_code()),
        }
    }

    fn statfs(&mut self, _req: &fuser::Request, _ino: u64, reply: fuser::ReplyStatfs) {
        let free_sectors = self.fs.num_free_sectors();
        let num_entries = match self
            .fs
            .num_entries_within_entry(self.fs.header.root_sector.get())
        {
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

    fn lookup(
        &mut self,
        req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEntry,
    ) {
        match self.get_entry_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.get_fs_attr_ino(req, self.get_ino(node)) {
                Ok(attr) => {
                    reply.entry(&Duration::from_secs(10), &attr, 0);
                }
                Err(err) => reply.error(err.get_code()),
            },
            Err(err) => reply.error(err.get_code()),
        }
    }

    fn access(
        &mut self,
        _req: &fuser::Request<'_>,
        _ino: u64,
        _mask: i32,
        reply: fuser::ReplyEmpty,
    ) {
        reply.ok();
    }

    fn readdir(
        &mut self,
        _req: &fuser::Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: fuser::ReplyDirectory,
    ) {
        let n = self.get_entry_id(ino);
        if let Ok(hdr) = self.fs.entry_header(n)
            && hdr.get_entry_type() == CbEntryType::Directory
        {
            let dirs = self
                .fs
                .directory_listing(n)
                .unwrap()
                .into_iter()
                .map(|h| (h as u64, self.fs.entry_header(h)))
                .collect::<Vec<_>>();

            let parent_dirs = [
                (
                    n as u64,
                    Ok(CbEntryHeader {
                        attributes: hdr.attributes,
                        entry_type: hdr.entry_type,
                        modification_time: hdr.modification_time,
                        name: string_to_array(".").unwrap(),
                        parent: hdr.parent,
                        payload_size: hdr.payload_size,
                    }),
                ),
                (
                    self.get_ino(hdr.parent.get()),
                    Ok(CbEntryHeader {
                        attributes: 0,
                        entry_type: CbEntryType::Directory as u8,
                        modification_time: CbDateTime::default(),
                        name: string_to_array("..").unwrap(),
                        parent: 0.into(),
                        payload_size: 0.into(),
                    }),
                ),
            ];

            let all_dirs = parent_dirs.into_iter().chain(dirs).collect::<Vec<_>>();

            for (i, (node, hdr_res)) in all_dirs.into_iter().enumerate().skip(offset as usize) {
                let fst = match Self::fs_type(hdr.get_entry_type()) {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("unknown entry type for {:?} - {e:?}", hdr.get_entry_type());
                        continue;
                    }
                };

                match hdr_res {
                    Ok(hdr) => {
                        if reply.add(node, i as i64 + 1, fst, Path::new(&hdr.get_name())) {
                            reply.ok();
                            return;
                        }
                    }
                    Err(e) => {
                        eprintln!("Error for node {node} - {e:?}");
                    }
                }
            }

            reply.ok();
        } else {
            reply.error(ENOENT);
        }
    }

    fn create(
        &mut self,
        req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        _mode: u32,
        _umask: u32,
        _flags: i32,
        reply: fuser::ReplyCreate,
    ) {
        match self.fs.create_entry(
            self.get_entry_id(parent),
            name.to_str().unwrap(),
            CbEntryType::File,
            &[],
        ) {
            Ok(new_node) => match self.get_fs_attr_ino(req, self.get_ino(new_node)) {
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

    fn mknod(
        &mut self,
        req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        mode: u32,
        _umask: u32,
        _rdev: u32,
        reply: fuser::ReplyEntry,
    ) {
        #[allow(clippy::unnecessary_cast)] // Allow for other OS types
        const FILE_MODE_VAL: u32 = S_IFREG as u32;
        if (mode & FILE_MODE_VAL) == FILE_MODE_VAL {
            match self.fs.create_entry(
                self.get_entry_id(parent),
                name.to_str().unwrap(),
                CbEntryType::File,
                &[],
            ) {
                Ok(new_node) => match self.get_fs_attr_ino(req, self.get_ino(new_node)) {
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
        req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        _mode: u32,
        _umask: u32,
        reply: fuser::ReplyEntry,
    ) {
        let pnode = self.get_entry_id(parent);
        match self
            .fs
            .create_entry(pnode, name.to_str().unwrap(), CbEntryType::Directory, &[])
        {
            Ok(new_node) => match self.get_fs_attr_ino(req, self.get_ino(new_node)) {
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

    fn rename(
        &mut self,
        _req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        newparent: u64,
        newname: &std::ffi::OsStr,
        _flags: u32,
        reply: fuser::ReplyEmpty,
    ) {
        match self.get_entry_from_parent(parent, name.to_str().unwrap()) {
            Ok(entry) => match self.fs.move_entry(
                entry,
                self.get_entry_id(newparent),
                Some(newname.to_str().unwrap()),
            ) {
                Ok(()) => reply.ok(),
                Err(e) => reply.error(CbFuseErr::from(e).get_code()),
            },
            Err(e) => {
                reply.error(e.get_code());
            }
        }
    }

    fn unlink(
        &mut self,
        _req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        match self.get_entry_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.delete_entry(node, Some(CbEntryType::File)) {
                Ok(()) => reply.ok(),
                Err(err) => reply.error(err.get_code()),
            },
            Err(err) => reply.error(err.get_code()),
        }
    }

    fn rmdir(
        &mut self,
        _req: &fuser::Request,
        parent: u64,
        name: &std::ffi::OsStr,
        reply: fuser::ReplyEmpty,
    ) {
        match self.get_entry_from_parent(parent, name.to_str().unwrap()) {
            Ok(node) => match self.delete_entry(node, Some(CbEntryType::Directory)) {
                Ok(()) => {
                    reply.ok();
                }
                Err(err) => {
                    reply.error(err.get_code());
                }
            },
            Err(err) => {
                reply.error(err.get_code());
            }
        }
    }

    fn getattr(
        &mut self,
        req: &fuser::Request,
        ino: u64,
        _fh: Option<u64>,
        reply: fuser::ReplyAttr,
    ) {
        match self.get_fs_attr_ino(req, ino) {
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
        req: &fuser::Request,
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
                .set_entry_payload_byte_size(self.get_entry_id(ino), new_size as u32)
                .is_err()
        {
            reply.error(ENOSYS);
            return;
        }

        if let Some(mtime) = _mtime {
            let ts = match mtime {
                fuser::TimeOrNow::Now => SystemTime::now(),
                fuser::TimeOrNow::SpecificTime(t) => t,
            };

            let cbdt: CbDateTime = ts.into();

            match self.fs.entry_header(self.get_entry_id(ino)) {
                Ok(mut hdr) => {
                    hdr.set_modification_time(cbdt);

                    if let Err(e) = self.fs.set_entry_header(self.get_entry_id(ino), hdr) {
                        reply.error(CbFuseErr::from(e).get_code());
                        return;
                    }
                }
                Err(e) => {
                    reply.error(CbFuseErr::from(e).get_code());
                    return;
                }
            }
        }

        if let Some(ts) = _ctime {
            let cbdt: CbDateTime = ts.into();

            match self.fs.entry_header(self.get_entry_id(ino)) {
                Ok(mut hdr) => {
                    hdr.set_modification_time(cbdt);

                    if let Err(e) = self.fs.set_entry_header(self.get_entry_id(ino), hdr) {
                        reply.error(CbFuseErr::from(e).get_code());
                        return;
                    }
                }
                Err(e) => {
                    reply.error(CbFuseErr::from(e).get_code());
                    return;
                }
            }
        }

        reply.attr(
            &Duration::from_secs(5),
            &self.get_fs_attr_ino(req, ino).unwrap(),
        );
    }

    fn read(
        &mut self,
        _req: &fuser::Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyData,
    ) {
        if let Ok((hdr, data)) = self.fs.entry_data(self.get_entry_id(ino))
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
        _req: &fuser::Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        data: &[u8],
        _write_flags: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyWrite,
    ) {
        if let Ok((hdr, mut fdata)) = self.fs.entry_data(self.get_entry_id(ino)) {
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
                    .set_entry_data(self.get_entry_id(ino), hdr, &fdata)
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

    fn flush(
        &mut self,
        _req: &fuser::Request,
        _ino: u64,
        _fh: u64,
        _lock_owner: u64,
        reply: fuser::ReplyEmpty,
    ) {
        reply.ok();
    }

    fn fsync(
        &mut self,
        _req: &fuser::Request,
        _ino: u64,
        _fh: u64,
        _datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        match self.save_fs() {
            Ok(_) => reply.ok(),
            Err(e) => reply.error(e.get_code()),
        }
    }

    fn fsyncdir(
        &mut self,
        _req: &fuser::Request,
        _ino: u64,
        _fh: u64,
        _datasync: bool,
        reply: fuser::ReplyEmpty,
    ) {
        match self.save_fs() {
            Ok(_) => reply.ok(),
            Err(e) => reply.error(e.get_code()),
        }
    }
}
