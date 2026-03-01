use std::{
    ffi::{CStr, c_char},
    path::{Path, PathBuf},
};

use cbfs_lib::{
    CbContainer, CbContainerHeader, CbDate, CbDateTime, CbDirectoryEntry, CbEntryType, CbError,
    CbFileSystem, CbTime,
};

pub const CBFS_VOLUME_NAME_SIZE: usize = 32;
pub const CBFS_DIRECTORY_NAME_SIZE: usize = 60;

#[derive(Debug)]
pub struct CbFs {
    fs: CbFileSystem,
    flags: CbContainerHeader,
}

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

/// Converts the given time structure to a millisecond count
/// # Safety
/// This function should be called with a non-null pointer to a CbFsTime structure
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_time_to_millis(time: *const CbFsTime) -> i64 {
    if let Some(t) = unsafe { time.as_ref() } {
        let dt: CbDateTime = (*t).into();
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
    pub name: [c_char; CBFS_VOLUME_NAME_SIZE],
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
    ContainerError,
    UnknownError,
}

impl From<CbError> for CbFsResult {
    fn from(value: CbError) -> Self {
        match value {
            CbError::NameExists(_) => Self::DuplicateName,
            CbError::EntryInvalid(_) => Self::InvalidEntry,
            CbError::EntryNotDirectory(_) => Self::EntryNotDirectory,
            CbError::EntryNotFile(_) => Self::EntryNotFile,
            CbError::InvalidDateTime => Self::InvalidDateTime,
            CbError::InvalidName => Self::InvalidName,
            CbError::InvalidSectorCount(_) => Self::InvalidConfig,
            CbError::NonZeroDirectoryData => Self::InvalidConfig,
            CbError::SectorSizeTooSmall(_) => Self::InvalidConfig,
            CbError::PathNotFound(_) => Self::EntryNotFound,
            CbError::TableFull => Self::NoSpace,
            CbError::UnknownEntryType(_) => Self::InvalidEntry,
            CbError::ContainerError(_) => Self::ContainerError,
            CbError::UnknownError(_) => Self::UnknownError,
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

fn entry_for_path<T: AsRef<Path>>(fs: &CbFs, path: Option<T>) -> Result<CbDirectoryEntry, CbError> {
    if let Some(path) = path {
        let mut current_entry = CbDirectoryEntry {
            base_block: fs.fs.header.root_sector,
            attributes: 0,
            entry_type: CbEntryType::Directory.into(),
            name: [0; _],
        };

        if path.as_ref().has_root() {
            'parts: for p in path.as_ref().iter().skip(1) {
                let dirs = fs.fs.directory_listing(current_entry.base_block.get())?;
                for d in dirs {
                    if d.get_name() == p.to_str().unwrap() {
                        current_entry = d;
                        continue 'parts;
                    }
                }

                return Err(CbError::PathNotFound(p.to_str().unwrap().to_string()));
            }

            Ok(current_entry)
        } else {
            Err(CbError::InvalidName)
        }
    } else {
        Err(CbError::InvalidName)
    }
}

/// Compatibility function to create a filesystem with the provided parameters
/// # Safety
/// This function should be called with a non-null pointer to a C string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_open(backing_file: *const c_char, randomize: bool) -> *mut CbFs {
    let file_path = get_path(backing_file);

    let (header, mut cbfs) = if let Some(fp) = &file_path
        && fp.exists()
    {
        if let Ok(container) = CbContainer::open(fp) {
            (container.header, container.filesystem)
        } else {
            return std::ptr::null_mut();
        }
    } else {
        return std::ptr::null_mut();
    };

    cbfs.randomize_sectors(randomize);

    Box::into_raw(Box::new(CbFs {
        flags: header,
        fs: cbfs,
    }))
}

/// Provides statistics for the filesystem
/// # Safety
/// This function should be called with a non-null pointer to a CbFs structure and CbFsStats structure
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_get_stats(fs: *const CbFs, stats: *mut CbFsStats) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(stats) = unsafe { stats.as_mut() }
    {
        stats.num_blocks = fs.fs.header.sector_count.get();
        stats.block_size = fs.fs.header.sector_size.get();
        stats.entry_blocks = fs.fs.base_entries.len() as u16;
        stats.free_blocks = fs.fs.num_free_sectors() as u16;
        stats.name = fs.fs.header.volume_name.map(|x| x as i8);
        stats.root_node = fs.fs.header.root_sector.get();

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

/// Provides the parent of a node, if one exists
/// # Safety
/// This function should be called with a non-null pointer to a CbFs structure and node pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_get_parent_node(
    fs: *const CbFs,
    node: u16,
    parent: *mut u16,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(parent) = unsafe { parent.as_mut() }
    {
        *parent = if node == fs.fs.header.root_sector.get() {
            0
        } else {
            match fs.fs.entry_header(node) {
                Ok(x) => x.get_parent(),
                Err(e) => return e.into(),
            }
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

/// Sets the entry payload size of a given entry
/// # Safety
/// This function should be called with a non-null pointer to a CbFs structure
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_truncate(fs: *mut CbFs, id: u16, size: u32) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() } {
        match fs.fs.set_entry_payload_byte_size(id, size) {
            Ok(_) => CbFsResult::Success,
            Err(e) => e.into(),
        }
    } else {
        CbFsResult::NullProvided
    }
}

/// Sets the modification time of a given entry by id
/// # Safety
/// This function should be called with a non-null pointer to a CbFs and CbFsTime
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_entry_set_time(
    fs: *mut CbFs,
    id: u16,
    time: *const CbFsTime,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() } {
        let mut hdr = match fs.fs.entry_header(id) {
            Err(e) => return e.into(),
            Ok(h) => h,
        };
        if let Some(time) = unsafe { time.as_ref() } {
            hdr.modification_time = (*time).into();
        } else {
            hdr.modification_time = chrono::Utc::now().into();
        }

        match fs.fs.set_entry_header(id, hdr) {
            Ok(_) => (),
            Err(e) => return e.into(),
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

/// Provides the dentry for a directory header value
fn cbfs_entry_from_dir_val(
    fs: &CbFileSystem,
    dir_hdr: &CbDirectoryEntry,
) -> Result<CbFsEntry, CbError> {
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

/// Provides the given entry header by path
/// # Safety
/// This function should be called with a non-null pointer to a CbFs and CbFsEntry
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_get_entry_by_path(
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

        let inner = match cbfs_entry_from_dir_val(&fs.fs, &dir_entry) {
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

/// Provides the given entry header by entry id
/// # Safety
/// This function should be called with a non-null pointer to a CbFs and CbFsEntry
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_get_entry(
    fs: *const CbFs,
    id: u16,
    entry: *mut CbFsEntry,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() } {
        let entry_hdr = match fs.fs.entry_header(id) {
            Ok(v) => v,
            Err(e) => return e.into(),
        };

        let dir_entry = if entry_hdr.parent.get() == 0 {
            CbDirectoryEntry {
                base_block: fs.fs.header.root_sector,
                attributes: 0,
                entry_type: CbEntryType::Directory.into(),
                name: [0; _],
            }
        } else {
            match fs.fs.directory_entry(id) {
                Ok(v) => v,
                Err(e) => return e.into(),
            }
        };

        let inner = match cbfs_entry_from_dir_val(&fs.fs, &dir_entry) {
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

/// Reads the entries provided in a directory directory entry
/// # Safety
/// This function should be called with a non-null pointer to a CbFs,
/// and the provided CbFsDirectoryList should be destroyed when done
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_read_dir(
    fs: *mut CbFs,
    dir: u16,
    listing: *mut *mut CbFsDirectoryList,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(listing) = unsafe { listing.as_mut() }
    {
        fn compat_gen(fs: &CbFs, x: CbDirectoryEntry) -> Result<CbFsEntry, CbError> {
            let tv: CbFsTime = fs
                .fs
                .entry_header(x.base_block.get())?
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

        let dirs = match fs.fs.directory_listing(dir) {
            Ok(x) => x,
            Err(e) => return e.into(),
        };
        let dirs_compat = match dirs
            .into_iter()
            .map(|x| compat_gen(fs, x))
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

/// Provides the size of the number of entries in the directory list
/// # Safety
/// This function should be called with a non-null pointer to a CbFsDirectoryList
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_read_dir_size(listing: *const CbFsDirectoryList) -> u32 {
    if let Some(listing) = unsafe { listing.as_ref() } {
        listing.entries.len() as u32
    } else {
        0
    }
}

/// Reads the given index from the directory list
/// # Safety
/// This function should be called with a non-null pointer to a CbFsDirectoryList entry value
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_read_dir_entry(
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

/// Destroys the provided directory listing structure
/// # Safety
/// This function should be called with a non-null pointer to a CbFsDirectoryList
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_read_dir_destroy(listing: *mut CbFsDirectoryList) {
    if !listing.is_null() {
        drop(unsafe { Box::from_raw(listing) });
    }
}

/// Reads data from the provided entry in the filesystem
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_read_entry_data(
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
        let (_, entry_data) = match fs.fs.entry_data(id) {
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

/// Writes data to the provided entry in the filesystem
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_write_entry_data(
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
        let (hdr, mut entry_data) = match fs.fs.entry_data(id) {
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

        match fs.fs.set_entry_data(id, hdr, &entry_data) {
            Ok(_) => (),
            Err(e) => return e.into(),
        };

        CbFsResult::Success
    } else {
        CbFsResult::NullProvided
    }
}

/// Saves the provided CbFs struct to the path provided
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and path
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_save(
    fs: *mut CbFs,
    backing_file: *const c_char,
    zero_unused: bool,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_ref() }
        && let Some(backing_file) = get_path(backing_file)
    {
        let mut fs_tmp = fs.fs.clone();

        if zero_unused {
            match fs_tmp.zero_unused_sectors() {
                Ok(_) => (),
                Err(e) => return e.into(),
            };
        }

        match CbContainer::new(fs.flags, fs_tmp).write_fs_to_file(&backing_file) {
            Ok(_) => CbFsResult::Success,
            Err(e) => e.into(),
        }
    } else {
        CbFsResult::NullProvided
    }
}

/// Creates a new entry of the provided type at the given path
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and path
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_create_entry(
    fs: *mut CbFs,
    path: *const c_char,
    entry_type: CbFsEntryType,
    entry_out: *mut CbFsEntry,
    truncate: bool,
) -> CbFsResult {
    if let Some(fs) = unsafe { fs.as_mut() }
        && let Some(path) = get_path(path)
    {
        if let Ok(entry) = entry_for_path(fs, Some(&path)) {
            if truncate {
                if entry_type == entry.get_entry_type().into() {
                    if entry.get_entry_type() == CbEntryType::File {
                        match fs.fs.set_entry_payload_byte_size(entry.base_block.get(), 0) {
                            Ok(()) => (),
                            Err(e) => return e.into(),
                        };
                    }
                    CbFsResult::Success
                } else {
                    CbFsResult::EntryNotFile
                }
            } else {
                CbFsResult::DuplicateName
            }
        } else if let Ok(parent) = entry_for_path(fs, path.parent()) {
            let new_name = path.file_name().unwrap().to_str().unwrap();
            let id_val =
                match fs
                    .fs
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

/// Renames the provided entry
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and paths
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_rename_entry(
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
            match fs.fs.move_entry(
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

/// Removes an entry of the specified type at the provided path from the filesystem
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct and path
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_remove_entry(
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
            match fs.fs.delete_entry(entry.base_block.get()) {
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
/// # Safety
/// This function should be called with a non-null pointer to a CbFs struct
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cbfs_destroy(fs: *mut CbFs) {
    if !fs.is_null() {
        unsafe { drop(Box::from_raw(fs)) }
    }
}
