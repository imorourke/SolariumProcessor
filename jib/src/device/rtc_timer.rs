use crate::{
    device::{DEVICE_ID_SIZE, DEVICE_MEM_SIZE, DeviceType, ProcessorDevice},
    memory::{MemorySegment, MemorySegmentError},
};

extern crate std;

#[derive(Default)]
pub struct RtcTimerDevice {
    irq: u8,
    repeats: bool,
    milliseconds: u32,
    init_time: Option<std::time::Instant>,
}

impl RtcTimerDevice {
    pub fn is_triggered(&self) -> bool {
        if self.milliseconds > 0
            && let Some(t) = self.init_time
        {
            std::time::Instant::now() - t
                > std::time::Duration::from_millis(self.milliseconds.into())
        } else {
            false
        }
    }
}

impl MemorySegment for RtcTimerDevice {
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        self.inspect(offset)
    }

    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Ok(self.device_type().get_device_id().to_be_bytes()[offset as usize])
        } else {
            let index = offset - DEVICE_ID_SIZE;
            match index {
                0 => Ok(self.irq),
                1 => Ok(if self.repeats { 1 } else { 0 }),
                x if x >= 2 && x < 6 => Ok(self.milliseconds.to_be_bytes()[x as usize - 2]),
                _ => Ok(0),
            }
        }
    }

    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Err(MemorySegmentError::ReadOnlyMemory(offset))
        } else {
            let index = offset - DEVICE_ID_SIZE;

            match index {
                0 => self.irq = val,
                1 => self.repeats = val != 0,
                x if x >= 2 && x < 6 => {
                    let mut ms_bytes = self.milliseconds.to_be_bytes();
                    ms_bytes[x as usize - 2] = val;
                    self.milliseconds = u32::from_be_bytes(ms_bytes);
                }
                _ => return Err(MemorySegmentError::ReadOnlyMemory(offset)),
            }

            if self.irq != 0 && self.milliseconds != 0 {
                if self.init_time.is_none() {
                    self.init_time = Some(std::time::Instant::now());
                }
            } else {
                self.init_time = None;
            }

            Ok(())
        }
    }

    fn reset(&mut self) {
        self.irq = 0;
        self.repeats = false;
        self.milliseconds = 0;
        self.init_time = None;
    }

    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }
}

impl ProcessorDevice for RtcTimerDevice {
    fn device_type(&self) -> DeviceType {
        DeviceType::RtcTimer
    }

    fn on_step(&mut self) -> Option<super::DeviceAction> {
        if self.is_triggered() {
            if self.repeats {
                self.init_time = Some(std::time::Instant::now());
            } else {
                self.init_time = None;
            }

            if self.irq != 0 {
                Some(super::DeviceAction::CallInterrupt(self.irq as u32))
            } else {
                None
            }
        } else {
            None
        }
    }
}
