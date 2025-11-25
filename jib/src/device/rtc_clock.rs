use chrono::{Datelike, Timelike};

use crate::{
    device::{DEVICE_ID_SIZE, DEVICE_MEM_SIZE, ProcessorDevice},
    memory::{MemorySegment, MemorySegmentError},
};

extern crate std;

#[derive(Default)]
pub struct RtcClockDevice;

impl RtcClockDevice {
    const DEVICE_ID: u16 = 3;
}

impl MemorySegment for RtcClockDevice {
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.inspect(offset)
    }

    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Ok(Self::DEVICE_ID.to_be_bytes()[offset as usize])
        } else {
            let index = offset - DEVICE_ID_SIZE;
            let time = chrono::Utc::now();
            match index {
                0 => Ok((time.year() - 1980) as u8),
                1 => Ok(time.month() as u8),
                2 => Ok(time.day0() as u8),
                3 => Ok(time.hour() as u8),
                4 => Ok(time.minute() as u8),
                5 => Ok(time.second() as u8),
                6 => Ok((time.timestamp_millis() / 10) as u8),
                _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            }
        }
    }

    fn set(&mut self, offset: u32, _val: u8) -> Result<(), MemorySegmentError> {
        Err(MemorySegmentError::ReadOnlyMemory(offset))
    }

    fn reset(&mut self) {
        // Do Nothing
    }

    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }
}

impl ProcessorDevice for RtcClockDevice {
    fn device_id(&self) -> u16 {
        Self::DEVICE_ID
    }
}
