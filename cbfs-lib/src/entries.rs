use std::fmt::Debug;

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

use crate::{CbfsError, datetime::CbDateTime, names::array_to_string, string_to_array};

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbDirectoryEntry {
    pub base_block: U16,
    pub attributes: u8,
    pub entry_type: u8,
    pub name: [u8; Self::NAME_SIZE],
}

impl CbDirectoryEntry {
    pub fn get_entry_type(&self) -> CbEntryType {
        CbEntryType::from(self.entry_type)
    }

    pub const NAME_SIZE: usize = 60;

    pub fn get_name(&self) -> String {
        array_to_string(&self.name)
    }

    pub fn set_name(&mut self, s: &str) -> Result<(), CbfsError> {
        self.name = string_to_array(s)?;
        Ok(())
    }
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbEntryHeader {
    pub entry_type: u8,
    pub reserved: u8,
    pub parent: U16,
    pub modification_time: CbDateTime,
    pub payload_size: U32,
}

impl CbEntryHeader {
    pub fn get_payload_size(&self) -> usize {
        self.payload_size.get() as usize
    }

    pub fn set_payload_size(&mut self, s: usize) {
        self.payload_size = U32::new(s as u32);
    }

    pub const fn get_header_size(&self) -> usize {
        std::mem::size_of::<CbEntryHeader>()
    }

    pub fn get_parent(&self) -> u16 {
        self.parent.get()
    }

    pub fn get_total_size(&self) -> usize {
        self.get_payload_size() + self.get_header_size()
    }

    pub fn get_entry_type(&self) -> CbEntryType {
        CbEntryType::from(self.entry_type)
    }

    pub fn get_modification_time(&self) -> CbDateTime {
        self.modification_time
    }

    pub fn set_modification_time(&mut self, time: CbDateTime) {
        self.modification_time = time;
    }
}

#[repr(u8)]
#[derive(Debug, Default, Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum CbEntryType {
    #[default]
    Unknown = 0,
    Directory = 1,
    File = 2,
}

impl From<u8> for CbEntryType {
    fn from(value: u8) -> Self {
        const DIR_ID: u8 = CbEntryType::Directory as u8;
        const FILE_ID: u8 = CbEntryType::File as u8;
        match value {
            DIR_ID => CbEntryType::Directory,
            FILE_ID => CbEntryType::File,
            _ => CbEntryType::Unknown,
        }
    }
}

impl From<CbEntryType> for u8 {
    fn from(value: CbEntryType) -> Self {
        value as u8
    }
}
