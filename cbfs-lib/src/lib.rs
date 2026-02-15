//! CBFS is a simple file-allocation-table style filesystem that allows for simple
//! data storage within the SPC and C/Buoy systems.

mod container;
mod datetime;
mod entries;
mod filesystem;
mod names;
mod volume;

use std::fmt::Debug;

pub use crate::{
    datetime::{CbDate, CbDateTime, CbTime},
    entries::{CbDirectoryEntry, CbEntryHeader, CbEntryType},
    names::{StringArrayError, string_to_array},
};

pub use filesystem::CbFileSystem;
pub use volume::CbVolumeHeader;
pub use container::{CbContainer, CbFileHeader};

/// Provides error message information regarding issues with the filesystem
#[derive(Debug, Clone)]
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
    UnknownError(String),
}

impl From<std::io::Error> for CbfsError {
    fn from(value: std::io::Error) -> Self {
        Self::UnknownError(format!("IO error - {value}"))
    }
}

impl From<StringArrayError> for CbfsError {
    fn from(value: StringArrayError) -> Self {
        match value {
            StringArrayError::InvalidName => Self::InvalidName,
        }
    }
}
