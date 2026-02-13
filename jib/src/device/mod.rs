mod block_device;
mod irq_clock;
#[cfg(feature = "rtcdev")]
mod rtc_clock;
#[cfg(feature = "rtcdev")]
mod rtc_timer;
mod serial_io;

pub use block_device::BlockDevice;
pub use irq_clock::InterruptClockDevice;
pub use rtc_clock::RtcClockDevice;
pub use rtc_timer::RtcTimerDevice;
pub use serial_io::SerialInputOutputDevice;

use crate::memory::{MemorySegment, MemorySegmentError};

pub const DEVICE_MEM_SIZE: u32 = 32;
pub const DEVICE_BLOCK_MEM_SIZE: u32 = 384;
pub const DEVICE_ID_SIZE: u32 = core::mem::size_of::<u16>() as u32;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum DeviceType {
    None,
    SerialIO,
    IrqClock,
    RtcClock,
    RtcTimer,
    BlockDevice,
}

impl DeviceType {
    pub const fn get_device_id(&self) -> u16 {
        match self {
            Self::None => 0,
            Self::SerialIO => 1,
            Self::IrqClock => 2,
            Self::RtcClock => 3,
            Self::RtcTimer => 4,
            Self::BlockDevice => u16::MAX,
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct BlankDevice;

impl MemorySegment for BlankDevice {
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.inspect(offset)
    }

    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if offset < self.len() {
            Ok(0)
        } else {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        }
    }

    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }

    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError> {
        Err(MemorySegmentError::InvalidMemoryWrite(offset, val))
    }

    fn reset(&mut self) {
        // Nothing
    }
}

impl ProcessorDevice for BlankDevice {
    fn device_type(&self) -> DeviceType {
        DeviceType::None
    }
}

pub enum DeviceAction {
    CallInterrupt(u32),
}

pub trait ProcessorDevice: MemorySegment {
    /// Runs any processing required of the device every CPU cycle
    fn on_step(&mut self) -> Option<DeviceAction> {
        None
    }

    /// Returns the device ID
    fn device_type(&self) -> DeviceType;
}
