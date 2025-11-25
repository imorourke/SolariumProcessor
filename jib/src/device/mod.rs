mod irq_clock;
#[cfg(feature = "rtcdev")]
mod rtc_clock;
mod serial_io;

pub use irq_clock::InterruptClockDevice;
pub use rtc_clock::RtcClockDevice;
pub use serial_io::SerialInputOutputDevice;

use crate::memory::MemorySegment;

pub const DEVICE_MEM_SIZE: u32 = 32;
pub const DEVICE_ID_SIZE: u32 = 2;

pub enum DeviceAction {
    CallInterrupt(u32),
}

pub trait ProcessorDevice: MemorySegment {
    /// Runs any processing required of the device every CPU cycle
    fn on_step(&mut self) -> Option<DeviceAction> {
        None
    }

    /// Returns the device ID
    fn device_id(&self) -> u16;
}
