//! CBFS is a simple file-allocation-table style filesystem that allows for simple
//! data storage within the SPC and C/Buoy systems.

mod container;
mod datetime;
mod entries;
mod filesystem;
mod names;
mod volume;

use std::fmt::{Debug, Display};

pub use crate::{
    datetime::{CbDate, CbDateTime, CbTime},
    entries::{CbDirectoryEntry, CbEntryHeader, CbEntryType},
    names::{StringArrayError, string_to_array},
};

pub use container::{CbContainer, CbFileHeader};
pub use filesystem::CbFileSystem;
pub use volume::CbVolumeHeader;

/// Provides error message information regarding issues with the filesystem
#[derive(Debug, Clone)]
pub enum CbfsError {
    EntryInvalid(u16),
    EntryNotFile(u16),
    EntryNotDirectory(u16),
    NonZeroDirectoryData,
    NameExists(String),
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

impl Display for CbfsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EntryInvalid(val) => write!(f, "entry {val} invalid"),
            Self::EntryNotFile(val) => write!(f, "entry {val} not a file"),
            Self::EntryNotDirectory(val) => write!(f, "entry {val} not a directory"),
            Self::NonZeroDirectoryData => write!(f, "non-zero directory data"),
            Self::NameExists(name) => write!(f, "name {name} already exists in path"),
            Self::PathNotFound(path) => write!(f, "path '{path}' not found"),
            Self::UnknownEntryType(etype) => write!(f, "unknown entry type {etype} found"),
            Self::InvalidName => write!(f, "invalid name provided"),
            Self::TableFull => write!(f, "sector table full"),
            Self::InvalidDateTime => write!(f, "invalid date/time detected"),
            Self::InvalidSectorCount(count) => write!(f, "invalid sector count {count} specified"),
            Self::SectorSizeTooSmall(size) => write!(f, "sector size {size} too small"),
            Self::UnknownError(error) => write!(f, "unknown cbfs error: {error}"),
        }
    }
}
