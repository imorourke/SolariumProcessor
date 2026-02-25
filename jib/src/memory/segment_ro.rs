use alloc::vec::Vec;

use super::{MemorySegment, MemorySegmentError};

/// Provides a read-write memory segment type
pub struct ReadOnlySegment {
    data: Vec<u8>,
}

impl ReadOnlySegment {
    /// Defines a new memory segment with empty data, zero, in each memory location
    pub fn new(data: Vec<u8>) -> Self {
        // Create the memory segment
        Self { data }
    }
}

impl MemorySegment for ReadOnlySegment {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if let Some(d) = self.data.get(offset as usize) {
            Ok(*d)
        } else {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        }
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, _: u8) -> Result<(), MemorySegmentError> {
        Err(MemorySegmentError::ReadOnlyMemory(offset))
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        // Do Nothing
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
        let mem = ReadOnlySegment::new((0..size).map(|_| 0).collect());

        // Ensure that the expected values match
        assert_eq!(mem.len(), size);

        // Iterate over memory items to check that the correct values are set
        for i in 0..MEM_MAX_SIZE {
            let is_within = i < size;
            assert_eq!(mem.within(i), is_within);

            match mem.get(i) {
                Ok(_) => assert!(is_within),
                Err(_) => assert!(!is_within),
            };
        }
    }

    /// Provide a default memory segment for testing
    fn get_default_test_segment() -> ReadOnlySegment {
        let size = 1024;

        ReadOnlySegment::new((0..size).map(|_| 0).collect())
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
        let size = u8::MAX;

        let mut mem = ReadOnlySegment::new((0..size).map(|i| i + 1).collect());

        for i in 0..size {
            let success = mem.set(i as u32, i + 1);
            assert!(success.is_err());
        }

        for i in 0..MEM_MAX_SIZE {
            let should_be_within = i < size as u32;
            assert_eq!(mem.within(i), should_be_within);

            let val = mem.get(i);
            assert_eq!(val.is_ok(), should_be_within);
            assert_eq!(val.is_err(), !should_be_within);

            if let Ok(mem_val) = val {
                assert_eq!(mem_val as usize, (i + 1) as usize);
            }
        }
    }
}
