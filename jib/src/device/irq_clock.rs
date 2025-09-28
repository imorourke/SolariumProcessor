use crate::{
    cpu::Processor,
    memory::{MemorySegment, MemorySegmentError},
};

use super::{DEVICE_ID_SIZE, DEVICE_MEM_SIZE, DeviceAction, ProcessorDevice};

pub struct InterruptClockDevice {
    clock_interval: u32,
    current_count: u32,
    interrupt: u32,
}

impl InterruptClockDevice {
    pub const DEVICE_ID: u16 = 2;

    pub fn new(interrupt: u32) -> Self {
        Self {
            clock_interval: 0,
            current_count: 0,
            interrupt,
        }
    }
}

impl MemorySegment for InterruptClockDevice {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.inspect(offset)
    }

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Ok(Self::DEVICE_ID.to_be_bytes()[offset as usize])
        } else {
            let offset = offset - DEVICE_ID_SIZE;

            let index = offset / Processor::BYTES_PER_WORD;
            let within = offset % Processor::BYTES_PER_WORD;

            let mem = [self.clock_interval, self.current_count, self.interrupt];

            if (index as usize) < mem.len() {
                let val = mem[index as usize].to_be_bytes();
                Ok(val[within as usize])
            } else {
                Err(MemorySegmentError::InvalidMemoryAccess(offset))
            }
        }
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, data: u8) -> Result<(), MemorySegmentError> {
        if offset >= DEVICE_ID_SIZE {
            let index = (offset - DEVICE_ID_SIZE) / Processor::BYTES_PER_WORD;
            let within = (offset - DEVICE_ID_SIZE) % Processor::BYTES_PER_WORD;

            let mut mem = [self.clock_interval, self.current_count, self.interrupt];

            if (index as usize) < mem.len() {
                let mut val = mem[index as usize].to_be_bytes();

                val[within as usize] = data;
                mem[index as usize] = u32::from_be_bytes(val);

                self.clock_interval = mem[0];
                // self.current_count = mem[1]; // Do not set the current count
                self.interrupt = mem[2];

                Ok(())
            } else {
                Err(MemorySegmentError::InvalidMemoryWrite(offset, data))
            }
        } else {
            Err(MemorySegmentError::InvalidMemoryWrite(offset, data))
        }
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        self.clock_interval = 0;
        self.current_count = 0;
    }

    /// Provides the length of the memory segment
    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }
}

impl ProcessorDevice for InterruptClockDevice {
    fn on_step(&mut self) -> Option<DeviceAction> {
        if self.clock_interval != 0 && self.interrupt != 0 {
            self.current_count = (self.current_count + 1) % self.clock_interval;

            match self.current_count {
                0 => Some(DeviceAction::CallInterrupt(self.interrupt)),
                _ => None,
            }
        } else {
            self.current_count = 0;
            None
        }
    }

    fn device_id(&self) -> u16 {
        Self::DEVICE_ID
    }
}
