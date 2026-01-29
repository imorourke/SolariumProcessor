use alloc::boxed::Box;
use alloc::vec;

use super::{MemorySegment, MemorySegmentError};

/// Provides a read-write memory segment type
pub struct ReadWriteSegment {
    data: Box<[u8]>,
}

impl ReadWriteSegment {
    /// Defines a new memory segment with empty data, zero, in each memory location
    pub fn new(size: usize) -> Self {
        // Create the memory segment
        Self {
            data: vec![0; size].into(),
        }
    }
}

impl MemorySegment for ReadWriteSegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if self.within(offset) {
            Ok(self.data[offset as usize])
        } else {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        }
    }

    /// Sets the word at the requested memory location with the given data
    fn set(&mut self, offset: u32, data: u8) -> Result<(), MemorySegmentError> {
        if self.within(offset) {
            self.data[offset as usize] = data;
            Ok(())
        } else {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        }
    }

    /// Sets the requested word range at the provided base address
    fn set_range(&mut self, offset: u32, vals: &[u8]) -> Result<(), MemorySegmentError> {
        if self.within(offset) && offset + vals.len() as u32 <= self.len() {
            for (i, v) in vals.iter().enumerate() {
                self.data[offset as usize + i] = *v;
            }
            Ok(())
        } else {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        }
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        // Reset all data values to 0 if not read only
        self.data.fill(0);
    }

    /// Provides the length of the memory segment
    fn len(&self) -> u32 {
        self.data.len() as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const MEM_MAX_SIZE: u32 = u16::MAX as u32;

    /// Test the initialization of the memory segment
    #[test]
    fn test_init() {
        // Define the base and the size
        let size = 1024;

        // Create the segment
        let mem = ReadWriteSegment::new(size);

        // Ensure that the expected values match
        assert_eq!(mem.len(), size as u32);

        // Iterate over memory items to check that the correct values are set
        for i in 0..MEM_MAX_SIZE {
            let is_within = i < size as u32;
            assert_eq!(mem.within(i), is_within);

            match mem.get(i) {
                Ok(_) => assert!(is_within),
                Err(_) => assert!(!is_within),
            };
        }
    }

    /// Provide a default memory segment for testing
    fn get_default_test_segment() -> ReadWriteSegment {
        let size = 1024;
        ReadWriteSegment::new(size)
    }

    /// Test setting a memory location above the top address
    #[test]
    fn test_panic_set_above() {
        let mut mem = get_default_test_segment();
        let result = mem.set(mem.len(), 32);
        assert!(result.is_err());
    }

    /// Test getting a memory location above the top address
    #[test]
    fn test_panic_get_above() {
        let mem = get_default_test_segment();
        let result = mem.get(mem.len());
        assert!(result.is_err());
    }

    /// Test the initial base offset value
    #[test]
    fn test_offset_base() {
        let size = 1024;

        let mut mem = ReadWriteSegment::new(size);

        for i in 0..size {
            let success = mem.set(i as u32, ((i + 1) % 256) as u8);
            assert!(success.is_ok());
        }

        for i in 0..MEM_MAX_SIZE {
            let should_be_within = i < size as u32;
            assert_eq!(mem.within(i), should_be_within);

            let val = mem.get(i);
            assert_eq!(val.is_ok(), should_be_within);
            assert_eq!(val.is_err(), !should_be_within);

            if let Ok(mem_val) = val {
                assert_eq!(mem_val, ((i + 1) % 256) as u8);
            }
        }
    }
}
