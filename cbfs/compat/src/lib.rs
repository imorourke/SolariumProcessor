use std::{
    ffi::{CStr, c_char},
    path::{Path, PathBuf},
};

use cbfs_lib::{
    CbContainer, CbDate, CbDateTime, CbDirectoryEntry, CbEntryType, CbFileHeader, CbFileSystem,
    CbTime, CbfsError,
};

pub const CBFS_VOLUME_NAME_SIZE: usize = 32;
pub const CBFS_DIRECTORY_NAME_SIZE: usize = 60;

pub struct CbFs(CbFileSystem);

#[repr(C)]
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum CbFsEntryType {
    #[default]
    Unknown,
    File,
    Directory,
}

impl From<CbEntryType> for CbFsEntryType {
    fn from(value: CbEntryType) -> Self {
        match value {
            CbEntryType::Directory => Self::Directory,
            CbEntryType::File => Self::File,
            _ => Self::Unknown,
        }
    }
}

impl From<CbFsEntryType> for CbEntryType {
    fn from(value: CbFsEntryType) -> Self {
        match value {
            CbFsEntryType::Directory => Self::Directory,
            CbFsEntryType::File => Self::File,
            _ => Self::Unknown,
        }
    }
}

#[repr(C)]
#[derive(Debug, Default, Clone, Copy, Eq, PartialEq)]
pub struct CbFsTime {
    pub year: i16,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
    pub hundredths: u8,
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_time_to_millis(time: *const CbFsTime) -> i64 {
    if let Some(t) = unsafe { time.as_ref() } {
        let dt: CbDateTime = t.clone().into();
        dt.to_posix_millis().unwrap_or_default()
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_millis_to_time(millis: i64) -> CbFsTime {
    CbDateTime::from_posix_millis(millis)
        .map(|x| x.into())
        .unwrap_or_default()
}

impl From<CbDateTime> for CbFsTime {
    fn from(value: CbDateTime) -> Self {
        Self {
            year: value.date.year.get(),
            month: value.date.month,
            day: value.date.day,
            hour: value.time.hour,
            minute: value.time.minute,
            second: value.time.second,
            hundredths: value.time.hundredths,
        }
    }
}

impl From<CbFsTime> for CbDateTime {
    fn from(value: CbFsTime) -> Self {
        Self {
            date: CbDate {
                year: value.year.into(),
                month: value.month,
                day: value.day,
            },
            time: CbTime {
                hour: value.hour,
                minute: value.minute,
                second: value.second,
                hundredths: value.hundredths,
            },
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CbFsEntry {
    pub entry_id: u16,
    pub entry_type: CbFsEntryType,
    pub size_bytes: u32,
    pub size_blocks: u32,
    pub last_time: CbFsTime,
    pub name: [c_char; CBFS_DIRECTORY_NAME_SIZE],
}

impl Default for CbFsEntry {
    fn default() -> Self {
        Self {
            entry_id: 0,
            name: [0; _],
            entry_type: CbFsEntryType::Unknown,
            size_blocks: 0,
            size_bytes: 0,
            last_time: CbFsTime::default(),
        }
    }
}

#[repr(C)]
#[derive(Default, Debug, Clone, Copy, Eq, PartialEq)]
pub struct CbFsStats {
    pub num_blocks: u16,
    pub block_size: u16,
    pub free_blocks: u16,
    pub entry_blocks: u16,
    pub root_node: u16,
    pub name: [c_char; CBFS_VOLUME_NAME_SIZE as usize],
}

#[derive(Default, Clone)]
pub struct CbFsDirectoryList {
    entries: Vec<CbFsEntry>,
}

impl CbFsDirectoryList {
    pub fn new<T: IntoIterator<Item = CbFsEntry>>(vals: T) -> Self {
        Self {
            entries: vals.into_iter().collect(),
        }
    }
}

#[repr(i32)]
pub enum CbFsResult {
    Success = 0,
    InvalidEntry,
    EntryNotFound,
    EntryNotDirectory,
    EntryNotFile,
    DuplicateName,
    InvalidDateTime,
    InvalidName,
    InvalidConfig,
    NoSpace,
    NullProvided,
    UnknownError,
}

impl From<CbfsError> for CbFsResult {
    fn from(value: CbfsError) -> Self {
        match value {
            CbfsError::DuplicateName(_) => Self::DuplicateName,
            CbfsError::EntryInvalid(_) => Self::InvalidEntry,
            CbfsError::EntryNotDirectory(_) => Self::EntryNotDirectory,
            CbfsError::EntryNotFile(_) => Self::EntryNotFile,
            CbfsError::InvalidDateTime => Self::InvalidDateTime,
            CbfsError::InvalidName => Self::InvalidName,
            CbfsError::InvalidSectorCount(_) => Self::InvalidConfig,
            CbfsError::NonZeroDirectoryData => Self::InvalidConfig,
            CbfsError::SectorSizeTooSmall(_) => Self::InvalidConfig,
            CbfsError::PathNotFound(_) => Self::EntryNotFound,
            CbfsError::TableFull => Self::NoSpace,
            CbfsError::UnknownEntryType(_) => Self::InvalidEntry,
            CbfsError::UnknownError(_) => Self::UnknownError,
        }
    }
}

fn get_path(p: *const c_char) -> Option<PathBuf> {
    if p.is_null() {
        None
    } else {
        Some(Path::new(unsafe { CStr::from_ptr(p) }.to_str().unwrap()).to_path_buf())
    }
}

fn c_to_str(c: *const c_char) -> Option<String> {
    if c.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(c) }.to_str().unwrap().to_string())
    }
}

fn entry_for_path<T: AsRef<Path>>(
    fs: &CbFs,
    path: Option<T>,
) -> Result<CbDirectoryEntry, CbfsError> {
    if let Some(path) = path {
        let mut current_entry = CbDirectoryEntry {
            base_block: fs.0.header.root_sector,
            attributes: 0,
            entry_type: CbEntryType::Directory.into(),
            name: [0; _],
        };

        if path.as_ref().has_root() {
            'parts: for p in path.as_ref().iter().skip(1) {
                let dirs = fs.0.directory_listing(current_entry.base_block.get())?;
                for d in dirs {
                    if d.get_name() == p.to_str().unwrap() {
                        current_entry = d;
                        continue 'parts;
                    }
                }

                return Err(CbfsError::PathNotFound(p.to_str().unwrap().to_string()));
            }

            Ok(current_entry)
        } else {
            Err(CbfsError::InvalidName)
        }
    } else {
        Err(CbfsError::InvalidName)
    }
}

/// Compatibility function to create a filesystem with the provided parameters
#[unsafe(no_mangle)]
pub extern "C" fn cbfs_create(
    name: *const c_char,
    sector_size: u16,
    sector_count: u16,
    backing_file: *const c_char,
    randomize: bool,
) -> *mut CbFs {
    let name_str = c_to_str(name).unwrap_or(String::default());
    let file_path = get_path(backing_file);

    let mut cbfs = if let Some(fp) = &file_path
        && fp.exists()
    {
        if let Ok(container) = CbContainer::open(fp) {
            Some(container.filesystem) // TODO - Save Container?
        } else {
            None
        }
    } else {
        CbFileSystem::new(&name_str, sector_size, sector_count).ok()
    };

    if let Some(fs) = cbfs.as_mut() {
        fs.randomize_sectors(randomize);
    }

    cbfs.map_or(std::ptr::null_mut(), |fs| Box::into_raw(Box::new(CbFs(fs))))
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_stats(fs: *const CbFs, stats: *mut CbFsStats) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(stats) = unsafe { stats.as_mut() }
    {
        stats.num_blocks = fs.0.header.sector_count.get();
        stats.block_size = fs.0.header.sector_size.get();
        stats.entry_blocks = fs.0.base_entries.len() as u16;
        stats.free_blocks = fs.0.num_free_sectors() as u16;
        stats.name = fs.0.header.volume_name.map(|x| x as i8);
        stats.root_node = fs.0.header.root_sector.get();

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_parent_node(fs: *const CbFs, node: u16, parent: *mut u16) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(parent) = unsafe { parent.as_mut() }
    {
        *parent = if node == fs.0.header.root_sector.get() {
            0
        } else {
            match fs.0.entry_header(node) {
                Ok(x) => x.get_parent(),
                Err(e) => return e.into(),
            }
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_truncate(fs: *mut CbFs, id: u16, size: u32) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() } {
        match fs.0.set_entry_payload_byte_size(id, size) {
            Ok(_) => CbFsResult::Success,
            Err(e) => e.into(),
        }
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_set_time(fs: *mut CbFs, id: u16, time: *const CbFsTime) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() } {
        let mut hdr = match fs.0.entry_header(id) {
            Err(e) => return e.into(),
            Ok(h) => h,
        };
        if let Some(time) = unsafe { time.as_ref() } {
            hdr.modification_time = (*time).into();
        } else {
            hdr.modification_time = chrono::Utc::now().into();
        }

        match fs.0.set_entry_header(id, hdr) {
            Ok(_) => (),
            Err(e) => return e.into(),
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

fn cbfs_entry_from_dir_val(
    fs: &CbFileSystem,
    dir_hdr: &CbDirectoryEntry,
) -> Result<CbFsEntry, CbfsError> {
    let hdr = fs.entry_header(dir_hdr.base_block.get())?;

    Ok(CbFsEntry {
        entry_id: dir_hdr.base_block.get(),
        entry_type: dir_hdr.get_entry_type().into(),
        name: dir_hdr.get_name_raw().map(|x| x as i8),
        size_bytes: hdr.get_payload_size() as u32,
        last_time: hdr.get_modification_time().into(),
        size_blocks: fs.num_sectors_for_entry(dir_hdr.base_block.get()) as u32,
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_entry_by_path(
    fs: *const CbFs,
    path: *const c_char,
    entry: *mut CbFsEntry,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(path) = get_path(path)
    {
        let dir_entry = match entry_for_path(fs, Some(path)) {
            Ok(e) => e,
            Err(e) => return e.into(),
        };

        let inner = match cbfs_entry_from_dir_val(&fs.0, &dir_entry) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };

        if let Some(entry) = unsafe { entry.as_mut() } {
            *entry = inner;
        }

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_entry(fs: *const CbFs, id: u16, entry: *mut CbFsEntry) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() } {
        let entry_hdr = match fs.0.entry_header(id) {
            Ok(v) => v,
            Err(e) => return e.into(),
        };

        let dir_entry = if entry_hdr.parent.get() == 0 {
            CbDirectoryEntry {
                base_block: fs.0.header.root_sector,
                attributes: 0,
                entry_type: CbEntryType::Directory.into(),
                name: [0; _],
            }
        } else {
            match fs.0.directory_entry(id) {
                Ok(v) => v,
                Err(e) => return e.into(),
            }
        };

        let inner = match cbfs_entry_from_dir_val(&fs.0, &dir_entry) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };

        if let Some(entry) = unsafe { entry.as_mut() } {
            *entry = inner;
        }

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_dir(
    fs: *mut CbFs,
    dir: u16,
    listing: *mut *mut CbFsDirectoryList,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(listing) = unsafe { listing.as_mut() }
    {
        fn compat_gen(fs: &CbFs, x: CbDirectoryEntry) -> Result<CbFsEntry, CbfsError> {
            let tv: CbFsTime =
                fs.0.entry_header(x.base_block.get())?
                    .get_modification_time()
                    .into();

            Ok(CbFsEntry {
                entry_id: x.base_block.get(),
                size_blocks: 0,
                entry_type: x.get_entry_type().into(),
                size_bytes: 0,
                name: x.name.map(|c| c as i8),
                last_time: tv,
            })
        }

        let dirs = match fs.0.directory_listing(dir) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };
        let dirs_compat = match dirs
            .into_iter()
            .map(|x| compat_gen(&fs, x))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(x) => x,
            Err(e) => return e.into(),
        };

        *listing = Box::into_raw(Box::new(CbFsDirectoryList::new(dirs_compat)));

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_dir_size(listing: *const CbFsDirectoryList) -> u32 {
    if let Some(listing) = unsafe { listing.as_ref() } {
        listing.entries.len() as u32
    } else {
        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_dir_entry(
    listing: *const CbFsDirectoryList,
    index: u32,
    entry: *mut CbFsEntry,
) -> CbFsResult {
    if let Some(listing) = unsafe { listing.as_ref() }
        && let Some(entry) = unsafe { entry.as_mut() }
    {
        if let Some(val) = listing.entries.get(index as usize) {
            *entry = *val;
            CbFsResult::Success
        } else {
            CbFsResult::EntryNotFound
        }
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_dir_destroy(listing: *mut CbFsDirectoryList) {
    if !listing.is_null() {
        drop(unsafe { Box::from_raw(listing) });
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_dir_list_destroy(listing: *mut CbFsDirectoryList) {
    if !listing.is_null() {
        drop(unsafe { Box::from_raw(listing) });
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_entry_data(
    fs: *mut CbFs,
    id: u16,
    offset: u32,
    data: *mut u8,
    size: *mut u32,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && !data.is_null()
        && !size.is_null()
    {
        let (_, entry_data) = match fs.0.entry_data(id) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };

        let slice = entry_data
            .iter()
            .skip(offset as usize)
            .take(unsafe { *size } as usize);
        unsafe { *size = slice.len() as u32 };

        for (i, d) in slice.enumerate() {
            unsafe { *data.add(i) = *d };
        }

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_write_entry_data(
    fs: *mut CbFs,
    id: u16,
    offset: u32,
    data: *const u8,
    size: *mut u32,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() }
        && !data.is_null()
        && !size.is_null()
    {
        let (hdr, mut entry_data) = match fs.0.entry_data(id) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };

        {
            let slice = entry_data
                .iter_mut()
                .skip(offset as usize)
                .take(unsafe { *size } as usize);
            unsafe { *size = slice.len() as u32 };

            for (i, d) in slice.enumerate() {
                *d = unsafe { *data.add(i) };
            }
        }

        match fs.0.set_entry_data(id, hdr, &entry_data) {
            Ok(_) => (),
            Err(e) => return e.into(),
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

#[repr(C)]
pub struct CbFsSaveOption {
    gzip: bool,
    sparse: bool,
    zero_unused: bool,
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_save(
    fs: *mut CbFs,
    backing_file: *const c_char,
    options: CbFsSaveOption,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(backing_file) = get_path(backing_file)
    {
        let mut fs_tmp = fs.0.clone();

        if options.zero_unused {
            match fs_tmp.zero_unused_sectors() {
                Ok(_) => (),
                Err(e) => return e.into(),
            };
        }

        match CbContainer::new(CbFileHeader::new(options.sparse, options.gzip), fs_tmp)
            .write_fs_to_file(&backing_file)
        {
            Ok(_) => CbFsResult::Success,
            Err(e) => e.into(),
        }
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_create_entry(
    fs: *mut CbFs,
    path: *const c_char,
    entry_type: CbFsEntryType,
    entry_out: *mut CbFsEntry,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() }
        && let Some(path) = get_path(path)
    {
        if let Ok(entry) = entry_for_path(fs, Some(&path)) {
            if entry_type == entry.get_entry_type().into() {
                if entry.get_entry_type() == CbEntryType::File {
                    match fs.0.set_entry_payload_byte_size(entry.base_block.get(), 0) {
                        Ok(()) => (),
                        Err(e) => return e.into(),
                    };
                }
                CbFsResult::Success
            } else {
                CbFsResult::NullProvided
            }
        } else if let Ok(parent) = entry_for_path(fs, path.parent()) {
            let new_name = path.file_name().unwrap().to_str().unwrap();
            let id_val =
                match fs
                    .0
                    .create_entry(parent.base_block.get(), new_name, entry_type.into(), &[])
                {
                    Ok(x) => x,
                    Err(e) => return e.into(),
                };

            if let Some(eo) = unsafe { entry_out.as_mut() } {
                eo.entry_id = id_val;
            }

            CbFsResult::Success
        } else {
            CbFsResult::EntryNotFound
        }
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_rename_entry(
    fs: *mut CbFs,
    path: *const c_char,
    new_path: *const c_char,
    replace: bool,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() }
        && let Some(path) = get_path(path)
        && let Some(new_path) = get_path(new_path)
    {
        if path == new_path {
            CbFsResult::Success
        } else if let Ok(entry) = entry_for_path(fs, Some(&path))
            && let Ok(parent) = entry_for_path(fs, new_path.parent())
        {
            match fs.0.move_entry(
                entry.base_block.get(),
                parent.base_block.get(),
                Some(new_path.file_name().unwrap().to_str().unwrap()),
                replace,
            ) {
                Ok(()) => CbFsResult::Success,
                Err(e) => e.into(),
            }
        } else {
            CbFsResult::NullProvided
        }
    } else {
        CbFsResult::NullProvided
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_remove_entry(
    fs: *mut CbFs,
    path: *const c_char,
    entry_type: CbFsEntryType,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() }
        && let Some(path) = get_path(path)
    {
        let entry = match entry_for_path(fs, Some(&path)) {
            Ok(v) => v,
            Err(e) => return e.into(),
        };

        if entry_type == CbFsEntryType::Unknown || (entry_type == entry.get_entry_type().into()) {
            match fs.0.delete_entry(entry.base_block.get()) {
                Ok(_) => CbFsResult::Success,
                Err(e) => e.into(),
            }
        } else {
            CbFsResult::InvalidEntry
        }
    } else {
        CbFsResult::NullProvided
    }
}

/// Compatibility function to detroy the provided filesystem
#[unsafe(no_mangle)]
pub extern "C" fn cbfs_destroy(fs: *mut CbFs) {
    if !fs.is_null() {
        unsafe { drop(Box::from_raw(fs)) }
    }
}
