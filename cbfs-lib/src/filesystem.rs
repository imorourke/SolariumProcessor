#[cfg(feature = "rand")]
use std::cell::RefCell;
#[cfg(feature = "time")]
use std::time::SystemTime;
use std::{collections::HashSet, fmt::Debug};

#[cfg(feature = "rand")]
use rand::RngExt;
use zerocopy::{
    FromBytes, IntoBytes,
    big_endian::{U16, U32},
};

use crate::{CbfsError, volume::CbVolumeHeader};
use crate::{
    datetime::CbDateTime,
    entries::{CbDirectoryEntry, CbEntryHeader, CbEntryType},
    names::string_to_array,
};

/// Provides the core filesystem entries
#[derive(Debug, Clone)]
pub struct CbFileSystem {
    /// The header associated with the current filesystem
    pub header: CbVolumeHeader,
    /// The entry allocation table
    pub entries: Box<[u16]>,
    /// The raw data sector values. This only contains the data after the allocation table, and does
    /// not include the entry sectors or the header sector
    data: Box<[u8]>,
    /// Defines which entries are base/primary entries
    pub base_entries: HashSet<u16>,
    #[cfg(feature = "rand")]
    /// Allows randomization of the file system entries
    randomize_entries: Option<RefCell<rand::rngs::ThreadRng>>,
}

impl CbFileSystem {
    /// Sentinel for the end of a node
    pub const NODE_END: u16 = 0xFFFF;

    /// Creates a new filesystem in memory
    pub fn new(name: &str, sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let mut header = CbVolumeHeader::new(sector_size, sector_count)?;
        header.set_name(name)?;
        let sector_count = header.sector_count.get();

        let data = std::iter::repeat_n(0, header.data_sector_size() as usize).collect();
        let mut entries: Box<[u16]> = std::iter::repeat_n(0, sector_count as usize).collect();

        for n in &mut entries[0..=header.root_sector.get() as usize] {
            *n = Self::NODE_END;
        }

        #[cfg(feature = "time")]
        let modification_time = CbDateTime::from(SystemTime::now());
        #[cfg(not(feature = "time"))]
        let modification_time = CbDateTime::default();

        let root_entry = CbEntryHeader {
            parent: U16::new(0),
            entry_type: CbEntryType::Directory as u8,
            reserved: 0,
            payload_size: U32::new(0),
            modification_time,
        };

        let mut fs = Self {
            data,
            entries,
            header,
            base_entries: HashSet::new(),
            #[cfg(feature = "rand")]
            randomize_entries: None,
        };
        fs.base_entries.insert(fs.header.root_sector.get());

        fs.set_entry_data(fs.header.root_sector.get(), root_entry, &[])?;

        Ok(fs)
    }

    /// Reads raw data stream into a CBFS file system
    pub fn from_bytes(data: &[u8]) -> Result<CbFileSystem, CbfsError> {
        let header =
            CbVolumeHeader::read_from_bytes(&data[0..std::mem::size_of::<CbVolumeHeader>()])
                .unwrap(); // TODO

        let sect_size = header.sector_size.get() as usize;
        let sect_count = header.sector_count.get() as usize;
        let entry_size = std::mem::size_of::<u16>();

        let entries: Box<[u16]> = data[sect_size..(sect_size + entry_size * sect_count)]
            .chunks(entry_size)
            .map(|x| U16::read_from_bytes(x).unwrap().get())
            .collect::<_>(); // TODO

        let data = &data[(sect_size * header.root_sector.get() as usize)..];

        let mut fs = CbFileSystem {
            header,
            entries,
            data: data.into(),
            base_entries: HashSet::new(),
            #[cfg(feature = "rand")]
            randomize_entries: None,
        };

        // Perform DFS to read the base/primary node lists
        let mut entry_stack = vec![fs.header.root_sector.get()];
        while let Some(e) = entry_stack.pop() {
            if fs.base_entries.insert(e) {
                if fs.entry_is_dir(e)? {
                    for d in fs.directory_listing(e)? {
                        entry_stack.push(d.base_block.get());
                    }
                }
            } else {
                panic!("duplicate file mapping provided!");
            }
        }

        Ok(fs)
    }

    /// Obtains the current file values as a byte sream
    pub fn as_bytes(&self) -> Result<Vec<u8>, CbfsError> {
        let mut voldata = vec![0u8; self.header.volume_byte_size() as usize];
        let sect_size = self.header.sector_size.get() as usize;

        for (dst, src) in voldata.iter_mut().zip(self.header.as_bytes()) {
            *dst = *src;
        }

        for (dst, src) in voldata.iter_mut().skip(sect_size).zip(
            self.entries
                .iter()
                .copied()
                .flat_map(|x| U16::new(x).to_bytes()),
        ) {
            *dst = src;
        }

        for (dst, src) in voldata
            .iter_mut()
            .skip(sect_size * self.header.root_sector.get() as usize)
            .zip(self.data.iter())
        {
            *dst = *src;
        }

        Ok(voldata)
    }

    /// Goes through each sector and zeros out any unused data sectors
    pub fn zero_unused_sectors(&mut self) -> Result<(), CbfsError> {
        let sector_size = self.header.sector_size.get() as usize;
        for (i, s) in self.entries.iter().enumerate() {
            if *s == 0 {
                let idx = self.get_sector_start_idx(i as u16)?;
                self.data[idx..(idx + sector_size)].fill(0);
            }
        }
        Ok(())
    }

    /// Enables or disables randomization of sector values
    #[cfg(feature = "rand")]
    pub fn randomize_sectors(&mut self, enabled: bool) {
        if enabled {
            if self.randomize_entries.is_none() {
                self.randomize_entries = Some(RefCell::new(rand::rng()))
            }
        } else {
            self.randomize_entries = None
        }
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

    /// Provides the starting index within the data vector for the provided sector
    fn get_sector_start_idx(&self, sector: u16) -> Result<usize, CbfsError> {
        let idx = (self.get_entry_valid(sector)? - self.header.root_sector.get()) as usize;
        Ok(idx * self.header.sector_size.get() as usize)
    }

    /// Provides the data slice associated with the given sector
    pub fn get_sector_data(&self, sector: u16) -> Result<&[u8], CbfsError> {
        let idx = self.get_sector_start_idx(sector)?;
        Ok(&self.data[idx..(idx + self.header.sector_size.get() as usize)])
    }

    /// Provides the mutable data slice associated with the given sector
    fn get_sector_data_mut(&mut self, sector: u16) -> Result<&mut [u8], CbfsError> {
        let idx = self.get_sector_start_idx(sector)?;
        Ok(&mut self.data[idx..(idx + self.header.sector_size.get() as usize)])
    }

    /// Provides the entry type associated with the given entry
    fn entry_type(&self, entry: u16) -> Result<CbEntryType, CbfsError> {
        match self.data.get(self.get_sector_start_idx(entry)?).copied() {
            Some(val) => Ok(CbEntryType::from(val)),
            None => Err(CbfsError::EntryInvalid(entry)),
        }
    }

    /// Provides true if the given entry value is a boolean
    fn entry_is_dir(&self, entry: u16) -> Result<bool, CbfsError> {
        Ok(self.entry_type(entry)? == CbEntryType::Directory)
    }

    /// Iterates through the directory tree to detemrine if the entry is a root entry
    fn entry_is_primary(&self, entry: u16) -> Result<bool, CbfsError> {
        Ok(self.base_entries.contains(&entry))
    }

    /// Provides the full entry header for the provided entry
    pub fn entry_header(&self, entry: u16) -> Result<CbEntryHeader, CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        let idx = self.get_sector_start_idx(entry)?;
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
        let idx = self.get_sector_start_idx(entry)?;
        for (dst, src) in self.data[idx..(idx + std::mem::size_of::<CbEntryHeader>())]
            .iter_mut()
            .zip(hdr.as_bytes())
        {
            *dst = *src;
        }
        Ok(())
    }

    /// Provides the entries associated with the current directory
    pub fn directory_listing(&self, entry: u16) -> Result<Vec<CbDirectoryEntry>, CbfsError> {
        if !self.entry_is_dir(entry)? {
            return Err(CbfsError::EntryNotDirectory(entry));
        }

        let (_, data) = self.entry_data(entry)?;
        let mut dirs = Vec::new();

        for e in data
            .chunks(std::mem::size_of::<CbDirectoryEntry>())
            .map(|x| CbDirectoryEntry::read_from_bytes(x).unwrap())
        {
            if e.base_block.get() != 0 {
                dirs.push(e);
            }
        }

        Ok(dirs)
    }

    /// Provides the directory entry associated with the given node in a parent directory
    pub fn directory_entry(&self, target: u16) -> Result<CbDirectoryEntry, CbfsError> {
        let ent_hdr = self.entry_header(target)?;
        let entry = ent_hdr.get_parent();

        if !self.entry_is_dir(entry)? {
            return Err(CbfsError::EntryNotDirectory(entry));
        }

        let n = self.get_entry_valid(target)?;

        let (_, data) = self.entry_data(entry)?;

        for e in data
            .chunks(std::mem::size_of::<CbDirectoryEntry>())
            .map(|x| CbDirectoryEntry::read_from_bytes(x).unwrap())
        {
            if e.base_block.get() == n {
                return Ok(e);
            }
        }

        Err(CbfsError::EntryInvalid(target))
    }

    /// Provides all the raw data (header and payload together) for the requested entry
    fn entry_data_raw(&self, mut entry: u16) -> Result<Vec<u8>, CbfsError> {
        let mut raw_data = Vec::new();
        if self.entries[self.get_entry_valid(entry)? as usize] == 0 {
            return Err(CbfsError::EntryNotFile(entry));
        }

        while entry != Self::NODE_END {
            assert_ne!(entry, 0);
            raw_data.extend(self.get_sector_data(entry)?);
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

    /// Returns the next free sector based on the given sector algorithm
    fn next_free_sector(&self) -> Result<u16, CbfsError> {
        #[cfg(feature = "rand")]
        if let Some(mut r) = self.randomize_entries.as_ref().map(|x| x.borrow_mut()) {
            let mut current =
                r.random_range(self.header.root_sector.get()..self.header.sector_count.get());
            let init = current;

            while self.entries[current as usize] != 0 {
                current = ((current + 1) % self.header.sector_count.get())
                    .max(self.header.root_sector.get());
                if current == init {
                    return Err(CbfsError::TableFull);
                }
            }

            return Ok(current);
        }

        self.entries
            .iter()
            .enumerate()
            .skip(self.header.root_sector.get() as usize)
            .filter(|(_, x)| **x == 0)
            .map(|(i, _)| i as u16)
            .next()
            .ok_or(CbfsError::TableFull)
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
                count += self.num_entries_within_entry(e.base_block.get())?;
            }
            Ok(count)
        } else {
            Ok(count)
        }
    }

    /// Determines the number of "primary/base" entries in the filesystem
    pub fn num_primary_entries(&self) -> usize {
        self.base_entries.len()
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
        let num_required = self.required_sectors_for_raw_size(count as usize);
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
    fn required_sectors_for_raw_size(&self, size: usize) -> usize {
        size.div_ceil(self.header.sector_size.get() as usize)
    }

    /// Determines the number of sectors that are required for the given payload byte size
    fn required_sectors_for_payload_size(&self, size: usize) -> usize {
        self.required_sectors_for_raw_size(size + std::mem::size_of::<CbEntryHeader>())
    }

    /// Sets the raw (header + payload) data together for an entry
    pub fn set_entry_data_raw(&mut self, entry: u16, data: &[u8]) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);

        let required_sectors = self.required_sectors_for_raw_size(data.len());
        self.set_num_sectors_for_entry(entry, required_sectors as u16)?;

        let file_nodes = self.sector_ids_for_entry(entry)?;

        assert_eq!(file_nodes.len(), required_sectors);
        let sec_size = self.header.sector_size.get() as usize;

        for (d, idx) in data.chunks(sec_size).zip(file_nodes) {
            for (dst, src) in self.get_sector_data_mut(idx)?.iter_mut().zip(d.iter()) {
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
            if d.get_name() == name {
                return Err(CbfsError::DuplicateName(name.into()));
            }
        }

        // If providing a directory entry, the data field must be empty
        if entry_type == CbEntryType::Directory && !data.is_empty() {
            return Err(CbfsError::NonZeroDirectoryData);
        }

        let new_entry = self.next_free_sector()?;
        self.entries[new_entry as usize] = Self::NODE_END;

        #[cfg(feature = "time")]
        let modification_time = CbDateTime::from(SystemTime::now());
        #[cfg(not(feature = "time"))]
        let modification_time = CbDateTime::default();

        let dir_ent = CbDirectoryEntry {
            base_block: U16::new(new_entry),
            attributes: 0,
            entry_type: entry_type as u8,
            name: string_to_array(name)?,
        };

        let new_hdr = CbEntryHeader {
            parent: U16::new(parent),
            entry_type: entry_type as u8,
            reserved: 0,
            modification_time,
            payload_size: U32::new(data.len() as u32),
        };

        self.base_entries.insert(new_entry);
        self.add_entry_to_directory(parent, dir_ent)?;
        self.set_entry_data(new_entry, new_hdr, data)?;

        Ok(new_entry)
    }

    /// Deletes an entry. If the entry is a directory, all contained files and
    /// folders will also be deleted.
    pub fn delete_entry(&mut self, entry: u16) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);

        if self.entry_is_dir(entry)? {
            for n in self.directory_listing(entry)? {
                self.delete_entry(n.base_block.get())?;
            }
        }

        let hdr = self.entry_header(entry)?;
        let pnode = hdr.parent.get();
        if pnode != 0 {
            self.remove_entry_from_directory(pnode, entry)?;
        } else {
            assert_eq!(entry, self.header.root_sector.get());
        }

        self.set_num_sectors_for_entry(entry, 0)?;
        self.base_entries.remove(&entry);

        if pnode != 0 {
            self.trim_directory(pnode)?;
        }

        Ok(())
    }

    /// Trims the current directory, reducing the number of sectors used if possible
    fn trim_directory(&mut self, entry: u16) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        if !self.entry_is_dir(entry)? {
            return Err(CbfsError::EntryNotDirectory(entry));
        }

        let num_sectors = self.num_sectors_for_entry(entry);
        let dir_entries = self.directory_listing(entry)?;
        let required_sectors = self.required_sectors_for_payload_size(
            dir_entries.len() * std::mem::size_of::<CbDirectoryEntry>(),
        );

        assert!(required_sectors <= num_sectors);

        if required_sectors < num_sectors {
            let mut new_data = Vec::new();
            for d in dir_entries.into_iter() {
                new_data.extend(d.as_bytes());
            }
            self.set_entry_data(entry, self.entry_header(entry)?, &new_data)?;
        }

        Ok(())
    }

    /// Removes an entry from a directory. This will not actually delete the entry, but only
    /// remove it from the directory entry table.
    fn remove_entry_from_directory(
        &mut self,
        parent: u16,
        entry: u16,
    ) -> Result<CbDirectoryEntry, CbfsError> {
        assert!(self.entry_is_primary(parent)?);
        assert!(self.entry_is_primary(entry)?);

        let (hdr, mut data) = self.entry_data(parent)?;
        assert_eq!(hdr.get_payload_size(), data.len());

        let mut result_val = None;

        for d in data
            .chunks_mut(std::mem::size_of::<CbDirectoryEntry>())
            .map(|x| CbDirectoryEntry::mut_from_bytes(x).unwrap())
        {
            if d.base_block.get() == entry {
                d.base_block = U16::new(0);
                assert!(result_val.is_none());
                result_val = Some(*d);
            }
        }

        self.set_entry_data(parent, hdr, &data)?;
        Ok(result_val.unwrap())
    }

    /// Adds an entry to the provided directory table.
    fn add_entry_to_directory(
        &mut self,
        parent: u16,
        entry: CbDirectoryEntry,
    ) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(parent)?);

        let (mut hdr, mut data) = self.entry_data(parent)?;
        assert_eq!(hdr.get_payload_size(), data.len());
        let mut found = false;

        for d in data
            .chunks_mut(std::mem::size_of::<CbDirectoryEntry>())
            .map(|x| CbDirectoryEntry::mut_from_bytes(x).unwrap())
        {
            if d.base_block.get() == 0 {
                *d = entry;
                found = true;
                break;
            }
        }

        if !found {
            hdr.set_payload_size(hdr.get_payload_size() + std::mem::size_of::<CbDirectoryEntry>());
            data.extend_from_slice(entry.as_bytes());
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
        overwrite: bool,
    ) -> Result<(), CbfsError> {
        assert!(self.entry_is_primary(entry)?);
        assert!(self.entry_is_primary(new_parent)?);

        if !self.entry_is_dir(new_parent)? {
            return Err(CbfsError::EntryNotDirectory(new_parent));
        } else if entry == self.header.root_sector.get() {
            return Err(CbfsError::EntryInvalid(entry));
        }

        let mut ent_hdr = self.entry_header(entry)?;
        let mut dir_hdr = self.directory_entry(entry)?;

        if let Some(n) = new_name {
            dir_hdr.set_name(n)?;
        }

        let target_name = dir_hdr.get_name();

        for e in self.directory_listing(new_parent)? {
            if e.get_name() == target_name {
                if overwrite {
                    self.delete_entry(e.base_block.get())?;
                } else {
                    return Err(CbfsError::DuplicateName(target_name));
                }
            }
        }

        let pnode = ent_hdr.parent.get();
        ent_hdr.parent = new_parent.into();

        self.remove_entry_from_directory(pnode, entry)?;
        self.add_entry_to_directory(new_parent, dir_hdr)?;
        self.set_entry_header(entry, ent_hdr)?;
        self.trim_directory(pnode)?;

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{CbEntryType, CbFileSystem};

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

    #[test]
    fn test_file_move_sectors() {
        let mut fs = CbFileSystem::new("test", 1024, 512).unwrap();

        let root = fs.header.root_sector.get();
        let num_free = fs.num_free_sectors();
        let num_primary = fs.num_primary_entries();

        let file_a = fs
            .create_entry(root, "a.txt", CbEntryType::File, b"file_a\n")
            .unwrap();
        let file_b = fs
            .create_entry(root, "b.txt", CbEntryType::File, b"file_b\n")
            .unwrap();

        assert!(fs.entry_is_primary(file_a).unwrap());
        assert!(fs.entry_is_primary(file_b).unwrap());

        fs.move_entry(file_a, root, Some("b.txt"), true).unwrap();

        assert!(fs.entry_is_primary(file_a).unwrap());
        assert!(!fs.entry_is_primary(file_b).unwrap());

        fs.delete_entry(file_a).unwrap();

        assert_eq!(num_primary, 1);
        assert_eq!(num_primary, fs.num_primary_entries());

        assert_eq!(num_free, fs.num_free_sectors())
    }

    #[test]
    fn test_file_folder_opers() {
        let mut fs = CbFileSystem::new("test", 512, 32768).unwrap();
        #[cfg(feature = "rand")]
        fs.randomize_sectors(true);

        assert_eq!(1, fs.num_primary_entries());

        let init_free = fs.num_free_sectors();
        let root = fs.header.root_sector.get();

        for i in 0..2 {
            let mut entries_to_delete = Vec::new();

            for j in 0..100 {
                let file_data = (0..j)
                    .map(|x| ((x + i + j) % 256) as u8)
                    .collect::<Vec<_>>();
                let new_file = fs
                    .create_entry(
                        root,
                        &format!("file_{j}.bin"),
                        CbEntryType::File,
                        &file_data,
                    )
                    .unwrap();
                entries_to_delete.push(new_file);

                assert_eq!(
                    fs.entry_header(new_file).unwrap().get_payload_size(),
                    file_data.len()
                );
            }

            for k in 0..10 {
                let folder_val = fs
                    .create_entry(root, &format!("folder_{k}"), CbEntryType::Directory, &[])
                    .unwrap();
                entries_to_delete.push(folder_val);

                for j in (0..100).rev() {
                    let file_data = (0..j)
                        .map(|x| ((x + i + j + k) % 256) as u8)
                        .collect::<Vec<_>>();
                    let new_file = fs
                        .create_entry(
                            folder_val,
                            &format!("abc_{j}.bin"),
                            CbEntryType::File,
                            &file_data,
                        )
                        .unwrap();

                    assert_eq!(
                        fs.entry_header(new_file).unwrap().get_payload_size(),
                        file_data.len()
                    );
                }
            }

            for k in 0..10 {
                let folder_val = fs
                    .create_entry(root, &format!("merged_{k}"), CbEntryType::Directory, &[])
                    .unwrap();
                entries_to_delete.push(folder_val);

                let mut inner_files = Vec::new();

                for j in (0..100).rev() {
                    let file_data = (0..j)
                        .map(|x| ((x + i + j + k) % 256) as u8)
                        .collect::<Vec<_>>();
                    let new_file = fs
                        .create_entry(
                            folder_val,
                            &format!("abcd_{j}.bin"),
                            CbEntryType::File,
                            &file_data,
                        )
                        .unwrap();
                    inner_files.push(new_file);

                    assert_eq!(
                        fs.entry_header(new_file).unwrap().get_payload_size(),
                        file_data.len()
                    );
                }

                for fi in inner_files {
                    fs.move_entry(fi, folder_val, Some("merged.bin"), true)
                        .unwrap();
                }
            }

            assert!(init_free > fs.num_free_sectors());

            for f in entries_to_delete {
                fs.delete_entry(f).unwrap();
            }

            assert_eq!(init_free, fs.num_free_sectors());
        }
        assert_eq!(init_free, fs.num_free_sectors());
        assert_eq!(1, fs.num_primary_entries());
    }
}
