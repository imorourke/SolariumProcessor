use std::fmt::Display;

use zerocopy::{FromBytes, IntoBytes, KnownLayout, big_endian::U32};

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout)]
pub struct CbVolumeHeader {
    sector_size: U32,
    sector_count: U32,
    volume_name: [u8; 16],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CbDateTime {
    pub year: u8,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
}

impl From<u32> for CbDateTime {
    fn from(value: u32) -> Self {
        Self {
            year: 0,
            month: 0,
            day: 0,
            hour: 0,
            minute: 0,
            second: 0,
        }
    }
}

impl From<CbDateTime> for u32 {
    fn from(value: CbDateTime) -> Self {
        0
    }
}

impl Display for CbDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}/{}/{} {}:{}:{}",
            self.year,
            self.month + 1,
            self.day + 1,
            self.hour,
            self.minute,
            self.second
        )
    }
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout)]
pub struct CbDirectoryHeader {
    name: [u8; 12],
    attributes: u8,
    reserved: u8,
    next_block: U32,
    modification_time: U32,
    byte_size: U32,
}

impl CbDirectoryHeader {
    pub const ATT_DIRECTORY: u8 = 1 << 0;

    pub fn is_directory(&self) -> bool {
        (self.attributes & Self::ATT_DIRECTORY) != 0
    }
}
