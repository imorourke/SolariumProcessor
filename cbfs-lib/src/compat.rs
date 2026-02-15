use std::{
    ffi::{CStr, c_char},
    path::Path,
};

use crate::{
    CbDirectoryEntry, CbEntryType, CbFileSystem, VOLUME_NAME_SIZE, entries::DIRECTORY_NAME_SIZE,
    string_to_array,
};

#[repr(C)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct CbCompatEntry {
    pub entry_id: u16,
    pub name: [c_char; DIRECTORY_NAME_SIZE],
    pub entry_type: CbEntryType,
    pub size_bytes: u32,
    pub size_blocks: u32,
}

impl Default for CbCompatEntry {
    fn default() -> Self {
        Self {
            entry_id: 0,
            name: [0; _],
            entry_type: CbEntryType::Unknown,
            size_blocks: 0,
            size_bytes: 0,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct CbCompatFsStats {
    pub num_blocks: u16,
    pub block_size: u16,
    pub free_blocks: u16,
    pub entry_blocks: u16,
    pub name: [c_char; VOLUME_NAME_SIZE],
}

impl Default for CbCompatFsStats {
    fn default() -> Self {
        Self {
            num_blocks: 0,
            block_size: 0,
            free_blocks: 0,
            entry_blocks: 0,
            name: [0; _],
        }
    }
}

#[repr(C)]
#[derive(Default, Clone, Copy)]
pub struct CbCompatDirListing {
    pub size: u32,
    pub entries: *const CbCompatEntry,
}

fn c_to_str(s: *const c_char) -> Option<String> {
    if s.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(s) }.to_str().unwrap().to_string())
    }
}

fn entry_for_path(fs: &CbFileSystem, path: &str) -> Option<CbDirectoryEntry> {
    let mut current_entry = CbDirectoryEntry {
        base_block: fs.header.root_sector,
        attributes: 0,
        entry_type: CbEntryType::Directory.into(),
        name: [0; _],
    };

    let parts = path.strip_prefix('/').unwrap();
    if parts.len() > 0 {
        'parts: for p in parts.split('/') {
            let dirs = fs
                .directory_listing(current_entry.base_block.get())
                .unwrap(); // TODO
            for d in dirs {
                if d.get_name() == p {
                    current_entry = d;
                    continue 'parts;
                }
            }

            println!("no directory found for '{path}'");
            return None;
        }
    }

    Some(current_entry)
}

/// Compatibility function to create a filesystem with the provided parameters
#[unsafe(no_mangle)]
pub extern "C" fn cbfs_create(
    name: *const c_char,
    sector_size: u16,
    sector_count: u16,
    backing_file: *const c_char,
) -> *mut CbFileSystem {
    let name_str = c_to_str(name).unwrap_or(String::default());
    let file_path = c_to_str(backing_file).map(|x| Path::new(&x).to_path_buf());

    let cbfs = if let Some(fp) = &file_path
        && fp.exists()
    {
        if let Ok((_, fs)) = CbFileSystem::open(fp) {
            Some(fs)
        } else {
            None
        }
    } else {
        if let Some(fp) = file_path
            && !fp.exists()
        {
            println!("Unable to load provided file {fp:?}")
        }

        let mut fs_tmp = CbFileSystem::new(&name_str, sector_size, sector_count).ok();

        if let Some(fs) = &mut fs_tmp {
            fs.create_entry(
                fs.header.root_sector.get(),
                "hello.txt",
                CbEntryType::File,
                b"Hello, world!\n",
            )
            .unwrap(); // TODO

            let tmp = fs
                .create_entry(
                    fs.header.root_sector.get(),
                    "testing",
                    CbEntryType::Directory,
                    &[],
                )
                .unwrap(); // TODO
            fs.create_entry(
                tmp,
                "inner_file.txt",
                CbEntryType::File,
                b"This is a super file...\n",
            )
            .unwrap(); // TODO
        }

        fs_tmp
    };

    cbfs.map_or(std::ptr::null_mut(), |fs| Box::into_raw(Box::new(fs)))
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_stats(fs: *const CbFileSystem, stats: *mut CbCompatFsStats) -> i32 {
    if fs.is_null() || stats.is_null() {
        return -1;
    } else {
        let fs = unsafe { &*fs };
        let stats = unsafe { &mut *stats };

        stats.num_blocks = fs.header.sector_count.get();
        stats.block_size = fs.header.sector_size.get();
        stats.entry_blocks = fs.base_entries.len() as u16;
        stats.free_blocks = stats.num_blocks - stats.entry_blocks;
        stats.name = fs.header.volume_name.map(|x| x as i8);

        0
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_get_entry(
    fs: *const CbFileSystem,
    name: *const c_char,
    entry: *mut CbCompatEntry,
) -> i32 {
    if fs.is_null() || name.is_null() {
        return -1;
    }

    let fs = unsafe { &*fs };

    let current = match entry_for_path(fs, &c_to_str(name).unwrap()) {
        Some(e) => e,
        None => return -1,
    };

    if !entry.is_null() {
        let entry = unsafe { &mut *entry };
        entry.entry_id = current.base_block.get();
        entry.entry_type = current.get_entry_type();
        entry.name = string_to_array(&current.get_name())
            .unwrap()
            .map(|x| x as i8); // TODO
        entry.size_bytes = fs
            .entry_header(current.base_block.get())
            .unwrap()
            .get_payload_size() as u32; // TODO
    }

    0
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_dir(fs: *mut CbFileSystem, dir: u16) -> CbCompatDirListing {
    let mut entry = CbCompatDirListing::default();

    if !fs.is_null() {
        let fs = unsafe { &*fs };

        let dirs = fs.directory_listing(dir).unwrap(); // TODO - UNWRAP!
        let dirs_compat = dirs
            .into_iter()
            .map(|x| CbCompatEntry {
                entry_id: x.base_block.get(),
                size_blocks: 0,
                entry_type: x.get_entry_type(),
                size_bytes: 0,
                name: x.name.map(|c| c as i8),
            })
            .collect::<Vec<_>>();

        entry.size = dirs_compat.len() as u32;
        entry.entries = Box::into_raw(dirs_compat.into_boxed_slice()).cast();
    }

    entry
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_read_entry_data(
    fs: *mut CbFileSystem,
    entry: u16,
    offset: u32,
    data: *mut u8,
    size: *mut u32,
) -> i32 {
    if fs.is_null() || data.is_null() || size.is_null() {
        return -1;
    }

    let fs = unsafe { &*fs };
    let (_, entry_data) = fs.entry_data(entry).unwrap(); // TODO

    let slice = entry_data
        .iter()
        .skip(offset as usize)
        .take(unsafe { *size } as usize);
    unsafe { *size = slice.len() as u32 };

    for (i, d) in slice.enumerate() {
        unsafe { *data.add(i) = *d };
    }

    0
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_destroy_dir_listing(listing: *const CbCompatDirListing) {
    if listing.is_null() {
        return;
    }

    drop(unsafe { Box::from_raw(listing.cast_mut()) });
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_save(fs: *mut CbFileSystem, backing_file: *const c_char) -> i32 {
    if fs.is_null() || backing_file.is_null() {
        return -1;
    }

    let file_path = Path::new(unsafe { CStr::from_ptr(backing_file) }.to_str().unwrap());

    let fs = unsafe { &*fs };
    fs.write_fs_to_file(file_path, false, true).unwrap(); // TODO

    0
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_mkdir(fs: *mut CbFileSystem, path: *const c_char) -> i32 {
    if fs.is_null() || path.is_null() {
        return -1;
    }

    let fs = unsafe { &mut *fs };
    let path = c_to_str(path).unwrap();

    let mut current_entry = CbDirectoryEntry {
        base_block: fs.header.root_sector,
        attributes: 0,
        entry_type: CbEntryType::Directory.into(),
        name: [0; _],
    };

    let parts = path.strip_prefix('/').unwrap();
    if parts.len() > 0 {
        let path_parts = parts.split('/').collect::<Vec<_>>();
        let new_name = path_parts.last().unwrap();
        let root_parts = &path_parts[..path_parts.len() - 1];

        'parts: for p in root_parts {
            let dirs = fs
                .directory_listing(current_entry.base_block.get())
                .unwrap(); // TODO
            for d in dirs {
                if d.get_name() == *p {
                    current_entry = d;
                    continue 'parts;
                }
            }

            println!("no directory found for '{path}'");
            return -1;
        }

        if current_entry.get_entry_type() != CbEntryType::Directory {
            return -1;
        }

        fs.create_entry(
            current_entry.base_block.get(),
            new_name,
            CbEntryType::Directory,
            &[],
        )
        .unwrap(); // TODO

        0
    } else {
        -1
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn cbfs_remove_entry(
    fs: *mut CbFileSystem,
    path: *const c_char,
    entry_type: CbEntryType,
) -> i32 {
    if fs.is_null() || path.is_null() {
        return -1;
    }

    let fs = unsafe { &mut *fs };
    let entry = match entry_for_path(fs, &c_to_str(path).unwrap()) {
        Some(e) => e,
        None => return -1,
    };

    if entry_type == CbEntryType::Unknown || (entry_type == entry.get_entry_type()) {
        fs.delete_entry(entry.base_block.get()).unwrap(); // TODO
        0
    } else {
        -1
    }
}

/// Compatibility function to detroy the provided filesystem
#[unsafe(no_mangle)]
pub extern "C" fn cbfs_destroy(fs: *mut CbFileSystem) {
    if !fs.is_null() {
        unsafe { drop(Box::from_raw(fs)) }
    }
}
