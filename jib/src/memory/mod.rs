mod memory_map;
mod segment_ro;
mod segment_rw;

use core::fmt;

pub use memory_map::MemoryMap;
pub use segment_ro::ReadOnlySegment;
pub use segment_rw::ReadWriteSegment;

/// Provides error conditions for memory segment parameters
#[derive(Debug, Clone, Copy)]
pub enum MemoryError {
    InvalidMemoryAccess(u32),
    InvalidMemoryWrite(u32, u8),
    ReadOnlyMemory(u32),
    OverlappingSegment(u32),
    EmptySegment(u32),
    InvalidAddress(u32),
    IndexBounds(usize),
}

impl fmt::Display for MemoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidMemoryAccess(loc) => write!(f, "Invalid Memory Access 0x{loc:08x}"),
            Self::InvalidMemoryWrite(loc, data) => {
                write!(f, "Invalid Memory Access 0x{loc:08x} = {data}")
            }
            Self::ReadOnlyMemory(loc) => write!(f, "Read Only Memory 0x{loc:08x}"),
            Self::OverlappingSegment(loc) => write!(f, "Overlapping Segment at 0x{loc:08x}"),
            Self::EmptySegment(loc) => write!(f, "Empty Segment 0x{loc:08x}"),
            Self::InvalidAddress(loc) => write!(f, "Invalid Address 0x{loc:08x}"),
            Self::IndexBounds(loc) => write!(f, "Index Bounds 0x{loc:08x}"),
        }
    }
}

pub enum MemorySegmentError {
    InvalidMemoryAccess(u32),
    ReadOnlyMemory(u32),
    InvalidMemoryWrite(u32, u8),
}

pub trait MemorySegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError>;

    /// Provides the range of values at the specified memory location
    fn get_range(&self, offset: u32, vals: &mut [u8]) -> Result<(), MemorySegmentError> {
        if !self.within(offset) {
            return Err(MemorySegmentError::InvalidMemoryAccess(offset));
        }
        for (i, x) in vals.iter_mut().enumerate() {
            *x = self.get(offset + i as u32)?;
        }
        Ok(())
    }

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.get(offset)
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError>;

    /// Sets the word at the provided memory location with the given data values
    fn set_range(&mut self, offset: u32, vals: &[u8]) -> Result<(), MemorySegmentError> {
        if !self.within(offset) {
            return Err(MemorySegmentError::InvalidMemoryAccess(offset));
        }
        for (i, x) in vals.iter().enumerate() {
            self.set(offset + i as u32, *x)?;
        }
        Ok(())
    }

    /// Provides the length of the memory segment
    fn len(&self) -> u32;

    /// Resets the memory segment
    fn reset(&mut self);

    /// Determines that the offset is within the memory segment
    fn within(&self, offset: u32) -> bool {
        offset < self.len()
    }

    /// Determines whether the memory map is empty
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
