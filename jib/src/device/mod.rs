mod block_device;
mod irq_clock;
#[cfg(feature = "rtcdev")]
mod rtc_clock;
mod serial_io;

pub use block_device::BlockDevice;
pub use irq_clock::InterruptClockDevice;
pub use rtc_clock::RtcClockDevice;
pub use serial_io::SerialInputOutputDevice;

use crate::memory::MemorySegment;

pub const DEVICE_MEM_SIZE: u32 = 32;
pub const DEVICE_BLOCK_MEM_SIZE: u32 = 384;
pub const DEVICE_ID_SIZE: u32 = core::mem::size_of::<u16>() as u32;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum DeviceType {
    SerialIO,
    IrqClock,
    RtcClock,
    BlockDevice,
}

impl DeviceType {
    pub const fn get_device_id(&self) -> u16 {
        match self {
            Self::SerialIO => 1,
            Self::IrqClock => 2,
            Self::RtcClock => 3,
            Self::BlockDevice => 4,
        }
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
