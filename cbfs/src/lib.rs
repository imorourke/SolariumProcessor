//! CBFS is a simple file-allocation-table style filesystem that allows for simple
//! data storage within the SPC and C/Buoy systems.

mod datetime;
mod entries;
mod names;

use std::{
    fmt::Debug,
    fs::OpenOptions,
    io::{Seek, SeekFrom, Write},
    path::Path,
    time::SystemTime,
};

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

pub use crate::{
    datetime::CbDateTime,
    entries::{CbEntryHeader, CbEntryType},
    names::{StringArrayError, string_to_array},
};

/// The volume header is present at the beginning of the filesystem, starting
/// from byte 0 of the disk format.
#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbVolumeHeader {
    pub version: U16,
    pub sector_size: U16,
    pub sector_count: U16,
    pub volume_name: [u8; 16],
    pub root_sector: U16,
    reserved: [u8; 40],
}

impl CbVolumeHeader {
    /// The current version of the filesystem, allowing for processing of
    /// different-verisoned disks if the format changes over time.
    pub const CURRENT_VERSION: u16 = 1;

    /// The size of each table element
    const ENTRY_TABLE_ELEMENT_SIZE: usize = std::mem::size_of::<u16>();

    /// Creates a new disk format with the provided sector size and sector count entries
    pub fn new(sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let min_sector_size = std::mem::size_of::<Self>().min(std::mem::size_of::<CbEntryHeader>());

        if (sector_size as usize) < min_sector_size {
            return Err(CbfsError::SectorSizeTooSmall(sector_size));
        } else if sector_count == CbFileSystem::NODE_END {
            return Err(CbfsError::InvalidSectorCount(sector_count));
        }

        let mut header = Self {
            version: Self::CURRENT_VERSION.into(),
            sector_size: sector_size.into(),
            sector_count: sector_count.into(),
            root_sector: 0.into(),
            volume_name: string_to_array("")?,
            reserved: [0; _],
        };

        let table_size = (header.sector_count.get() as usize) * Self::ENTRY_TABLE_ELEMENT_SIZE;
        header.root_sector =
            U16::new((table_size.div_ceil(header.sector_size.get() as usize) + 1) as u16);

        if header.root_sector.get() >= sector_count {
            return Err(CbfsError::InvalidSectorCount(sector_count));
        }

        Ok(header)
    }

    /// Sets the name for the filesystem
    pub fn set_name(&mut self, name: &str) -> Result<(), CbfsError> {
        self.volume_name = string_to_array(name)?;
        Ok(())
    }

    /// Provides the overall volume byte size
    pub fn volume_byte_size(&self) -> u64 {
        self.sector_size.get() as u64 * self.sector_count.get() as u64
    }

    /// Provides the size of the data sector in bytes. This is equal to the volume byte size
    /// minus the sectors that are taken up by the header and the file allocation table.
    pub fn data_sector_size(&self) -> u64 {
        (self.sector_count.get() as u64 - self.root_sector.get() as u64)
            * self.sector_size.get() as u64
    }
}

/// Provides error message information regarding issues with the filesystem
#[derive(Debug)]
pub enum CbfsError {
    EntryInvalid(u16),
    EntryNotFile(u16),
    EntryNotDirectory(u16),
    NonZeroDirectoryData,
    DuplicateName(String),
    PathNotFound(String),
    UnknownEntryType(u8),
    InvalidName,
    TableFull,
    InvalidDateTime,
    InvalidSectorCount(u16),
    SectorSizeTooSmall(u16),
    IoError(std::io::Error),
}

impl From<StringArrayError> for CbfsError {
    fn from(value: StringArrayError) -> Self {
        match value {
            StringArrayError::InvalidName => Self::InvalidName,
        }
    }
}

impl From<std::io::Error> for CbfsError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

/// Provides the core filesystem entries
#[derive(Debug, Clone)]
pub struct CbFileSystem {
    /// The header associated with the current filesystem
    pub header: CbVolumeHeader,
    /// The entry allocation table
    entries: Vec<u16>,
    /// The raw data sector values. This only contains the data after the allocation table, and does
    /// not include the entry sectors or the header sector
    data: Vec<u8>,
}

impl CbFileSystem {
    /// Sentinel for the end of a node
    const NODE_END: u16 = 0xFFFF;

    /// Creates a new filesystem in memory
    pub fn new(name: &str, sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let mut header = CbVolumeHeader::new(sector_size, sector_count)?;
        header.set_name(name)?;
        let sector_count = header.sector_count.get();

        let data = vec![0u8; header.data_sector_size() as usize];
        let mut entries = vec![0u16; sector_count as usize];

        for n in entries
            .iter_mut()
            .take(header.root_sector.get() as usize + 1)
        {
            *n = Self::NODE_END;
        }

        let root_entry = CbEntryHeader {
            attributes: 0,
            entry_type: CbEntryType::Directory as u8,
            modification_time: CbDateTime::from(SystemTime::now()),
            name: string_to_array("")?,
            parent: U16::new(0),
            payload_size: U32::new(0),
        };

        let mut fs = Self {
            data,
            entries,
            header,
        };

        fs.set_entry_data(fs.header.root_sector.get(), root_entry, &[])?;

        Ok(fs)
    }

    /// Opens a filesystem disk image and loads into memory
    pub fn open(path: &Path) -> Result<Self, CbfsError> {
        let all_data = std::fs::read(path).unwrap();

        let header =
            CbVolumeHeader::read_from_bytes(&all_data[..std::mem::size_of::<CbVolumeHeader>()])
                .unwrap();

        let sect_size = header.sector_size.get() as usize;
        let sect_count = header.sector_count.get() as usize;

        let entries = all_data
            [sect_size..(sect_size + CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE * sect_count)]
            .chunks(CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE)
            .map(|x| U16::read_from_bytes(x).unwrap().get())
            .collect::<Vec<_>>();

        let data = all_data[(sect_size * header.root_sector.get() as usize)..].to_vec();

        assert_eq!(sect_count, entries.len());
        assert_eq!(
            data.len(),
            sect_size * (sect_count - header.root_sector.get() as usize)
        );

        Ok(Self {
            header,
            entries,
            data,
        })
    }

    /// Saves the current in-memory filesystme to the provided file
    pub fn write_fs_to_file(&self, file: &Path) -> Result<(), CbfsError> {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file)?;

        let sect_size = self.header.sector_size.get() as u64;

        f.set_len(self.header.volume_byte_size())?;
        f.seek(SeekFrom::Start(0))?;
        f.write_all(self.header.as_bytes())?;
        f.seek(SeekFrom::Start(sect_size))?;

        for n in self.entries.iter().copied() {
            f.write_all(&U16::new(n).to_bytes()).unwrap();
        }

        assert_eq!(self.data.len() as u64, self.header.data_sector_size());

        f.seek(SeekFrom::Start(
            sect_size * (self.header.root_sector.get() as u64),
        ))?;
        f.write_all(&self.data).unwrap();

        assert_eq!(f.stream_position()?, self.header.volume_byte_size());

        Ok(())
    }

    /// Determines if the entry is an end entry marked by the sentinel
    const fn entry_is_end(entry: u16) -> bool {
        entry & Self::NODE_END == Self::NODE_END
    }

    /// Determines if the entry value is valid, and if so, returns the validated entry
    pub const fn get_entry_valid(&self, entry: u16) -> Result<u16, CbfsError> {
        if entry >= self.header.root_sector.get() && entry < self.header.sector_count.get() {
            Ok(entry)
        } else {
            Err(CbfsError::EntryInvalid(entry))
        }
    }

    /// Provides the starting index within the data vector for the provided entry
    fn get_data_start_idx(&self, entry: u16) -> Result<usize, CbfsError> {
        let idx = (self.get_entry_valid(entry)? - self.header.root_sector.get()) as usize;
        Ok(idx * self.header.sector_size.get() as usize)
    }

    /// Provides the entry type associated with the given entry
    fn entry_type(&self, entry: u16) -> Result<CbEntryType, CbfsError> {
        match self.data.get(self.get_data_start_idx(entry)?).copied() {
            Some(val) => Ok(CbEntryType::from(val)),
            None => Err(CbfsError::EntryInvalid(entry)),
        }
    }

    /// Provides true if the given entry value is a boolean
    fn entry_is_dir(&self, entry: u16) -> Result<bool, CbfsError> {
        Ok(self.entry_type(entry)? == CbEntryType::Directory)
    }

    /// Provides the resulting entry for a given absolute path within the filesystem, using
    /// '/' characters as path separators
    pub fn get_entry_for_path(&self, path: &str) -> Result<u16, CbfsError> {
        let mut current = self.header.root_sector.get() as usize;
        let parts = path
            .split('/')
            .filter(|x| !x.is_empty())
            .collect::<Vec<_>>();

        // Iterate over each part
        'outer: for p in parts.iter() {
            // Get the directory listing for the current directory
            for d in self.directory_listing(current as u16)? {
                // Obtain the entry header and check if the name matches
                let entry = self.entry_header(d)?;
                if &entry.get_name() == p {
                    current = d as usize;
                    continue 'outer;
                }
            }

            // If we didn't continue above, mark path not found
            return Err(CbfsError::PathNotFound(path.into()));
        }

        Ok(current as u16)
    }

    /// Iterates through the directory tree to detemrine if the entry is a root entry
    fn entry_is_primary(&self, entry: u16) -> Result<bool, CbfsError> {
        let mut current = vec![self.header.root_sector.get()];

        while let Some(e) = current.pop() {
            if e == entry {
                return Ok(true);
            } else if self.entry_is_dir(e)? {
                for d in self.directory_listing(e)? {
                    current.push(d);
                }
            }
        }

        Ok(false)
    }

    /// Provides the full entry header for the provided entry
    pub fn entry_header(&self, entry: u16) -> Result<CbEntryHeader, CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        let idx = self.get_data_start_idx(entry)?;
        let id = self.data[idx];
        Ok(match CbEntryType::from(id) {
            CbEntryType::File | CbEntryType::Directory => CbEntryHeader::read_from_bytes(
                &self.data[idx..(idx + std::mem::size_of::<CbEntryHeader>())],
            )
            .unwrap(),
            _ => return Err(CbfsError::UnknownEntryType(id)),
        })
    }

    /// Sets only the header portion of a given entry
    pub fn set_entry_header(&mut self, entry: u16, hdr: CbEntryHeader) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        let idx = self.get_data_start_idx(entry)?;
        for (dst, src) in self.data[idx..(idx + std::mem::size_of::<CbEntryHeader>())]
            .iter_mut()
            .zip(hdr.as_bytes())
        {
            *dst = *src;
        }
        Ok(())
    }

    /// Provides the entries associated with the current directory
    pub fn directory_listing(&self, entry: u16) -> Result<Vec<u16>, CbfsError> {
        if !self.entry_is_dir(entry)? {
            return Err(CbfsError::EntryNotDirectory(entry));
        }

        let (_, data) = self.entry_data(entry)?;
        let mut dirs = Vec::new();

        for e in data
            .chunks(CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE)
            .map(|x| U16::read_from_bytes(x).unwrap().get())
        {
            if e != 0 {
                dirs.push(e);
            }
        }

        Ok(dirs)
    }

    /// Provides all the raw data (header and payload together) for the requested entry
    fn entry_data_raw(&self, mut entry: u16) -> Result<Vec<u8>, CbfsError> {
        let mut raw_data = Vec::new();
        if self.entries[self.get_entry_valid(entry)? as usize] == 0 {
            return Err(CbfsError::EntryNotFile(entry));
        }

        while entry != Self::NODE_END {
            assert_ne!(entry, 0);
            let idx = self.get_data_start_idx(entry)?;
            raw_data.extend(&self.data[idx..(idx + self.header.sector_size.get() as usize)]);
            entry = self.entries[entry as usize];
        }

        Ok(raw_data)
    }

    /// Provides the data associated with the provided entry
    pub fn entry_data(&self, entry: u16) -> Result<(CbEntryHeader, Vec<u8>), CbfsError> {
        let raw_data = self.entry_data_raw(entry)?;
        let hdr = CbEntryHeader::read_from_bytes(&raw_data[..std::mem::size_of::<CbEntryHeader>()])
            .unwrap();
        let hdr_size = hdr.get_header_size();

        Ok((hdr, raw_data[hdr_size..hdr.get_total_size()].to_vec()))
    }

    /// Provides the number of sectors associated with the entry
    pub fn num_sectors_for_entry(&self, entry: u16) -> usize {
        if entry == 0 {
            return 0;
        }

        let mut count = 0;
        let mut current = entry;
        while current != Self::NODE_END {
            count += 1;
            current = self.entries[current as usize];
            assert_ne!(0, current);
        }

        count
    }

    /// Returns the next free sector
    fn next_free_sector(&self) -> Result<u16, CbfsError> {
        self.entries
            .iter()
            .enumerate()
            .filter(|(_, x)| **x == 0)
            .map(|(i, _)| i as u16)
            .next()
            .map_or(Err(CbfsError::TableFull), |x| Ok(x))
    }

    /// Returns the number of free sectors remaining
    pub fn num_free_sectors(&self) -> usize {
        self.entries.iter().filter(|x| **x == 0).count()
    }

    /// Provides the number of entries within an entry, including the current entry itself.
    /// For a file entry, this will only return 1
    /// For a directory entry, this will itself and all entries within folders and subfolders
    pub fn num_entries_within_entry(&self, entry: u16) -> Result<usize, CbfsError> {
        let hdr = self.entry_header(entry)?;
        let mut count = 1;
        if hdr.get_entry_type() == CbEntryType::Directory {
            for e in self.directory_listing(entry)? {
                count += self.num_entries_within_entry(e)?;
            }
            Ok(count)
        } else {
            Ok(count)
        }
    }

    /// Sets the number of sectors for the given entry
    pub fn set_num_sectors_for_entry(
        &mut self,
        mut entry: u16,
        count: u16,
    ) -> Result<(), CbfsError> {
        // Check that there is enough space left in the table
        let num_free = self.entries.iter().copied().filter(|x| *x == 0).count();

        let num_current = self.num_sectors_for_entry(entry);
        let num_required = self.required_sectors_for_size(count as usize);
        let num_new = num_required as i64 - num_current as i64;

        if num_new > 0 && num_new > num_free as i64 {
            return Err(CbfsError::TableFull);
        }

        // Set the required nodes
        let mut current_count = 0;

        // Iterate through each entry
        while !Self::entry_is_end(entry) {
            // Determine the next entry, saving while we rewrite the current
            let mut next = self.entries[entry as usize];

            // If we haven't yet reached the target, set appropriately.
            // If we have reached the target, set all remaining entries linked as free
            if current_count < count {
                current_count += 1;

                // Mark the node as the ned if we have reached the required cound
                // If the entry is not the node end, do nothing.
                // If the entry is marked as the current end, we need to extend the entries,
                // obtaining the next free sector and adding to the list.
                if current_count == count {
                    self.entries[entry as usize] = Self::NODE_END;
                } else if next == Self::NODE_END {
                    next = self.next_free_sector()?;
                    self.entries[entry as usize] = next;
                    self.entries[next as usize] = Self::NODE_END;
                }
            } else {
                self.entries[entry as usize] = 0;
            }

            entry = next;
        }

        Ok(())
    }

    /// Provides the sector ID values associated with the current entry
    pub fn sector_ids_for_entry(&self, entry: u16) -> Result<Vec<u16>, CbfsError> {
        let mut current = self.get_entry_valid(entry)?;
        let mut vals = Vec::new();
        while !Self::entry_is_end(current) {
            vals.push(current);
            current = self.entries[current as usize];
        }
        assert_eq!(vals.len(), self.num_sectors_for_entry(entry));
        Ok(vals)
    }

    /// Determines the number of sectors that would be required for the given byte size.
    fn required_sectors_for_size(&self, size: usize) -> usize {
        size.div_ceil(self.header.sector_size.get() as usize)
    }

    /// Sets the raw (header + payload) data together for an entry
    pub fn set_entry_data_raw(&mut self, entry: u16, data: &[u8]) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);

        let required_sectors = self.required_sectors_for_size(data.len());
        self.set_num_sectors_for_entry(entry, required_sectors as u16)?;

        let file_nodes = self.sector_ids_for_entry(entry)?;

        assert_eq!(file_nodes.len(), required_sectors);
        let sec_size = self.header.sector_size.get() as usize;

        for (d, idx) in data.chunks(sec_size).zip(file_nodes) {
            let data_idx = self.get_data_start_idx(idx)?;

            for (dst, src) in self.data[data_idx..(data_idx + sec_size)]
                .iter_mut()
                .zip(d.iter())
            {
                *dst = *src;
            }
        }

        Ok(())
    }

    /// Sets the entry header and payload data for a given entry
    pub fn set_entry_data(
        &mut self,
        entry: u16,
        mut header: CbEntryHeader,
        data: &[u8],
    ) -> Result<(), CbfsError> {
        header.payload_size.set(data.len() as u32);
        let mut new_data = header.as_bytes().to_vec();
        new_data.extend(data);
        self.set_entry_data_raw(entry, &new_data)
    }

    /// Sets the payload size for the provided entry
    pub fn set_entry_payload_byte_size(&mut self, entry: u16, size: u32) -> Result<(), CbfsError> {
        let (hdr, mut data) = self.entry_data(entry)?;
        if hdr.get_entry_type() == CbEntryType::File {
            data.resize(size as usize, 0);
            self.set_entry_data(entry, hdr, &data)
        } else {
            Err(CbfsError::EntryNotFile(entry))
        }
    }

    /// Creates a new entry with the provided parent entry
    pub fn create_entry(
        &mut self,
        parent: u16,
        name: &str,
        entry_type: CbEntryType,
        data: &[u8],
    ) -> Result<u16, CbfsError> {
        // Check for a dupliate name within a directory
        for d in self.directory_listing(parent)? {
            if self.entry_header(d)?.get_name() == name {
                return Err(CbfsError::DuplicateName(name.into()));
            }
        }

        // If providing a directory entry, the data field must be empty
        if entry_type == CbEntryType::Directory && data.len() != 0 {
            return Err(CbfsError::NonZeroDirectoryData);
        }

        let new_node = self.next_free_sector()?;
        self.entries[new_node as usize] = Self::NODE_END;

        let new_hdr = CbEntryHeader {
            attributes: 0,
            entry_type: entry_type as u8,
            modification_time: CbDateTime::from(SystemTime::now()),
            name: string_to_array(name)?,
            parent: U16::new(parent),
            payload_size: U32::new(data.len() as u32),
        };

        self.add_entry_to_directory(parent, new_node)?;
        self.set_entry_data(new_node, new_hdr, data)?;

        Ok(new_node)
    }

    /// Deletes an entry. If the entry is a directory, all contained files and
    /// folders will also be deleted.
    pub fn delete_entry(&mut self, entry: u16) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);

        if self.entry_is_dir(entry)? {
            for n in self.directory_listing(entry)? {
                self.delete_entry(n)?;
            }
        }

        let hdr = self.entry_header(entry)?;
        let pnode = hdr.parent.get();
        if pnode != 0 {
            self.remove_entry_from_directory(pnode, entry)?;
        }

        self.set_num_sectors_for_entry(entry, 0)?;

        Ok(())
    }

    /// Removes an entry from a directory. This will not actually delete the entry, but only
    /// remove it from the directory entry table.
    fn remove_entry_from_directory(&mut self, parent: u16, entry: u16) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(parent)?);
        assert!(self.entry_is_primary(entry)?);

        let (hdr, mut data) = self.entry_data(parent)?;
        assert_eq!(hdr.get_payload_size(), data.len());

        for d in data
            .chunks_mut(CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE)
            .map(|x| U16::mut_from_bytes(x).unwrap())
        {
            if d.get() == entry {
                *d = U16::new(0);
            }
        }

        self.set_entry_data(parent, hdr, &data)?;
        Ok(())
    }

    /// Adds an entry to the provided directory table.
    fn add_entry_to_directory(&mut self, parent: u16, entry: u16) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(parent)?);

        let (mut hdr, mut data) = self.entry_data(parent)?;
        assert_eq!(hdr.get_payload_size(), data.len());
        let mut found = false;

        for d in data
            .chunks_mut(CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE)
            .map(|x| U16::mut_from_bytes(x).unwrap())
        {
            if d.get() == 0 {
                d.set(entry);
                found = true;
                break;
            }
        }

        if !found {
            hdr.set_payload_size(hdr.get_payload_size() + CbVolumeHeader::ENTRY_TABLE_ELEMENT_SIZE);
            data.extend_from_slice(U16::new(entry).as_bytes());
        }

        self.set_entry_data(parent, hdr, &data)?;

        Ok(())
    }

    /// Moves an entry to a new parent directory
    pub fn move_entry(
        &mut self,
        entry: u16,
        new_parent: u16,
        new_name: Option<&str>,
    ) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        assert!(self.entry_is_primary(new_parent)?);

        if !self.entry_is_dir(new_parent)? {
            return Err(CbfsError::EntryNotDirectory(new_parent));
        } else if entry == self.header.root_sector.get() {
            return Err(CbfsError::EntryInvalid(entry));
        }

        let mut hdr = self.entry_header(entry)?;

        if let Some(n) = new_name {
            hdr.set_name(n)?;
        }

        let target_name = hdr.get_name();

        for e in self.directory_listing(new_parent)? {
            if self.entry_header(e)?.get_name() == target_name {
                return Err(CbfsError::DuplicateName(target_name));
            }
        }

        let pnode = hdr.parent.get();
        hdr.parent = new_parent.into();

        self.remove_entry_from_directory(pnode, entry)?;
        self.add_entry_to_directory(new_parent, entry)?;
        self.set_entry_header(entry, hdr)?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{CbEntryType, CbFileSystem};

    #[test]
    fn test_file_nodes() {
        // Do Nothing
    }

    #[test]
    fn test_directory_create() {
        let mut fs = CbFileSystem::new("test", 1024, 512).unwrap();
        let dir_abc = fs
            .create_entry(
                fs.header.root_sector.get(),
                "abc",
                CbEntryType::Directory,
                &[],
            )
            .unwrap();
        let dir_defg = fs
            .create_entry(
                fs.header.root_sector.get(),
                "defg",
                CbEntryType::Directory,
                &[],
            )
            .unwrap();

        let file_a_data_in = "Hello, world!\n".bytes().collect::<Vec<_>>();

        let file_a = fs
            .create_entry(
                fs.header.root_sector.get(),
                "a.txt",
                CbEntryType::File,
                &file_a_data_in,
            )
            .unwrap();

        let file_b_data_in = "Hello, ABC!\n".bytes().collect::<Vec<_>>();
        let file_b = fs
            .create_entry(dir_abc, "a.txt", CbEntryType::File, &file_b_data_in)
            .unwrap();

        println!("File A = {file_a}");
        println!("File B = {file_b}");

        let (file_a_hdr, file_a_data) = fs.entry_data(file_a).unwrap();
        assert_eq!(file_a_hdr.payload_size.get() as usize, file_a_data.len());
        assert_eq!(file_a_hdr.payload_size.get() as usize, file_a_data_in.len());
        assert_eq!(file_a_data, file_a_data_in);

        let (file_b_hdr, file_b_data) = fs.entry_data(file_b).unwrap();
        assert_eq!(file_b_hdr.payload_size.get() as usize, file_b_data.len());
        assert_eq!(file_b_hdr.payload_size.get() as usize, file_b_data_in.len());
        assert_eq!(file_b_data, file_b_data_in);

        assert_eq!(fs.directory_listing(dir_defg).unwrap().len(), 0);
    }
}
