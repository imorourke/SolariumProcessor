use super::{MemoryError, MemorySegment, MemorySegmentError};

use core::cell::RefCell;

use alloc::rc::Rc;
use alloc::vec::Vec;
use core::mem::size_of;

struct SegmentData {
    base: u32,
    seg: Rc<RefCell<dyn MemorySegment>>,
}

impl SegmentData {
    fn within(&self, addr: u32) -> bool {
        addr >= self.base && addr < self.top()
    }

    fn top(&self) -> u32 {
        self.base + self.seg.borrow().len()
    }

    fn len(&self) -> u32 {
        self.seg.borrow().len()
    }

    fn get(&self, addr: u32) -> Result<u8, MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow().get(offset);
        self.segment_to_memory(res)
    }

    fn set(&self, addr: u32, val: u8) -> Result<(), MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow_mut().set(offset, val);
        self.segment_to_memory(res)
    }

    fn set_range(&self, addr: u32, vals: &[u8]) -> Result<(), MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow_mut().set_range(offset, vals);
        self.segment_to_memory(res)
    }

    fn inspect(&self, addr: u32) -> Result<u8, MemoryError> {
        let offset = addr - self.base;
        let res = self.seg.borrow().inspect(offset);
        self.segment_to_memory(res)
    }

    fn segment_to_memory<T>(
        &self,
        result: Result<T, MemorySegmentError>,
    ) -> Result<T, MemoryError> {
        match result {
            Err(MemorySegmentError::InvalidMemoryAccess(offset)) => {
                Err(MemoryError::InvalidMemoryAccess(self.base + offset))
            }
            Err(MemorySegmentError::ReadOnlyMemory(offset)) => {
                Err(MemoryError::ReadOnlyMemory(self.base + offset))
            }
            Err(MemorySegmentError::InvalidMemoryWrite(offset, data)) => {
                Err(MemoryError::InvalidMemoryWrite(self.base + offset, data))
            }
            Ok(v) => Ok(v),
        }
    }
}

pub struct MemoryMap {
    segments: Vec<SegmentData>,
}

macro_rules! GetSetInspectUnsignedType {
    ( $get_name: ident, $set_name: ident, $inspect_name: ident, $type: ident ) => {
        pub fn $get_name(&mut self, address: u32) -> Result<$type, MemoryError> {
            let mut bytes = [0; size_of::<$type>()];
            for i in 0..bytes.len() {
                bytes[i] = self.get(address + i as u32)?;
            }
            Ok($type::from_be_bytes(bytes))
        }

        pub fn $set_name(&mut self, address: u32, val: $type) -> Result<(), MemoryError> {
            for (i, v) in val.to_be_bytes().iter().enumerate() {
                self.set(address + i as u32, *v)?;
            }
            Ok(())
        }

        pub fn $inspect_name(&self, address: u32) -> Result<$type, MemoryError> {
            let mut bytes = [0; size_of::<$type>()];
            for i in 0..bytes.len() {
                bytes[i] = self.inspect(address + i as u32)?;
            }
            Ok($type::from_be_bytes(bytes))
        }
    };
}

impl MemoryMap {
    pub fn new() -> Self {
        MemoryMap {
            segments: Vec::new(),
        }
    }

    pub fn add_segment(
        &mut self,
        base: u32,
        seg: Rc<RefCell<dyn MemorySegment>>,
    ) -> Result<(), MemoryError> {
        let new_seg = SegmentData { base, seg };

        let top = base as usize + new_seg.seg.borrow().len() as usize;
        if top > u32::MAX as usize {
            return Err(MemoryError::IndexBounds(top));
        } else if new_seg.seg.borrow().is_empty() {
            return Err(MemoryError::EmptySegment(base));
        }

        for sd in self.segments.iter() {
            if new_seg.within(sd.base)
                || new_seg.within(sd.top() - 1)
                || sd.within(new_seg.base)
                || sd.within(new_seg.top() - 1)
            {
                return Err(MemoryError::OverlappingSegment(base));
            }
        }

        self.segments.push(new_seg);
        Ok(())
    }

    pub fn get(&self, address: u32) -> Result<u8, MemoryError> {
        let data = self.get_segment(address)?;
        data.get(address)
    }

    pub fn inspect(&self, address: u32) -> Result<u8, MemoryError> {
        let data = self.get_segment(address)?;
        data.inspect(address)
    }

    pub fn set(&mut self, address: u32, val: u8) -> Result<(), MemoryError> {
        let data = self.get_segment(address)?;
        data.set(address, val)
    }

    pub fn set_range(&mut self, address: u32, vals: &[u8]) -> Result<(), MemoryError> {
        let end_addr = address + vals.len() as u32;
        let mut offset = 0;
        while address + offset < end_addr {
            let data = self.get_segment(address)?;
            let end_val = (offset + data.len()).min(vals.len() as u32);
            let slice_values = &vals[offset as usize..end_val as usize];
            data.set_range(address + offset, slice_values)?;
            offset += slice_values.len() as u32;
        }
        Ok(())
    }

    fn get_segment(&self, address: u32) -> Result<&SegmentData, MemoryError> {
        for m in self.segments.iter() {
            if m.within(address) {
                return Ok(m);
            }
        }

        Err(MemoryError::InvalidAddress(address))
    }

    pub fn reset(&mut self) {
        for s in self.segments.iter() {
            s.seg.borrow_mut().reset();
        }
    }

    GetSetInspectUnsignedType!(get_u32, set_u32, inspect_u32, u32);
    GetSetInspectUnsignedType!(get_u16, set_u16, inspect_u16, u16);
}

impl Default for MemoryMap {
    fn default() -> Self {
        Self::new()
    }
}
