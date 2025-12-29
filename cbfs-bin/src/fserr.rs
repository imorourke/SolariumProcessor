use cbfs::CbfsError;
use libc::{ENOENT, ENOSYS, ENOTDIR};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CbFuseErr {
    Other,
    NotDirectory,
    NoEntry,
}

impl CbFuseErr {
    pub fn get_code(&self) -> i32 {
        match self {
            Self::NotDirectory => ENOTDIR,
            Self::NoEntry => ENOENT,
            _ => ENOSYS,
        }
    }
}

impl From<CbfsError> for CbFuseErr {
    fn from(value: CbfsError) -> Self {
        match value {
            CbfsError::PathNotFound(_) => Self::NoEntry,
            CbfsError::EntryInvalid(_) => Self::NoEntry,
            CbfsError::EntryNotDirectory(_) => Self::NotDirectory,
            _ => Self::Other,
        }
    }
}
