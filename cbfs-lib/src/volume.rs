use std::fmt::Debug;

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

use crate::{CbfsError, filesystem::CbFileSystem, names::array_to_string};
pub use crate::{
    entries::{CbDirectoryEntry, CbEntryHeader},
    names::string_to_array,
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
    pub root_sector: U16,
    pub flags: U32,
    pub volume_name: [u8; Self::VOLUME_NAME_SIZE],
}

impl CbVolumeHeader {
    /// The current version of the filesystem, allowing for processing of
    /// different-verisoned disks if the format changes over time.
    const CURRENT_VERSION: u16 = 1;

    /// The number of characters allowed in the volume header
    pub const VOLUME_NAME_SIZE: usize = 32;

    /// The size of each table element
    const ENTRY_TABLE_ELEMENT_SIZE: usize = std::mem::size_of::<u16>();

    /// Creates a new disk format with the provided sector size and sector count entries
    pub fn new(sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let min_sector_size = std::mem::size_of::<Self>()
            .max(std::mem::size_of::<CbEntryHeader>())
            .max(std::mem::size_of::<CbDirectoryEntry>());

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
            flags: 0.into(),
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

    /// Provides the name for the filesystem
    pub fn get_name(&self) -> String {
        array_to_string(&self.volume_name)
    }

    /// Provides the raw volume name array
    pub fn get_name_raw(&self) -> [u8; Self::VOLUME_NAME_SIZE] {
        self.volume_name
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
