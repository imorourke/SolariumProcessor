#[cfg(feature = "std")]
use alloc::string::String;
use alloc::{boxed::Box, vec::Vec};

use crate::{
    device::{DEVICE_BLOCK_MEM_SIZE, DeviceType, ProcessorDevice},
    memory::{MemorySegment, MemorySegmentError},
};

type BlockSyncFunc = dyn Fn(&[u8]) -> bool;

pub struct BlockDevice {
    pub data: Vec<u8>,
    current_offset: u32,
    target_offset: u32,
    parked: bool,
    memory_size: u16,
    on_sync: Option<Box<BlockSyncFunc>>,
}

impl BlockDevice {
    const CONTROL_SIZE: usize = 12;
    const WINDOW_SIZE_MAX: usize = 256;
    const DATA_TOP: usize = BlockDevice::CONTROL_SIZE + BlockDevice::WINDOW_SIZE_MAX;

    pub fn new<T: IntoIterator<Item = u8>>(data: T) -> Self {
        Self {
            data: data.into_iter().collect(),
            current_offset: 0,
            target_offset: 0,
            parked: true,
            memory_size: 0,
            on_sync: None,
        }
    }

    #[cfg(feature = "std")]
    pub fn new_file(file: &std::path::Path) -> Result<Self, String> {
        let data = match std::fs::read(file) {
            Ok(v) => v,
            Err(e) => return Err(std::format!("{e}")),
        };

        let pb = file.to_path_buf();

        Ok(Self {
            data,
            current_offset: 0,
            target_offset: 0,
            parked: true,
            memory_size: 0,
            on_sync: Some(Box::new(move |data| std::fs::write(&pb, data).is_ok())),
        })
    }

    pub fn set_offset(&mut self, target: u32) {
        self.target_offset = target;
        self.current_offset = target;
    }

    pub fn sync(&self) -> bool {
        if let Some(sync_func) = self.on_sync.as_ref() {
            sync_func(&self.data)
        } else {
            false
        }
    }

    pub fn get_data_window(&self) -> &[u8] {
        if (self.target_offset as usize) < self.data.len() {
            let window_bottom = self.target_offset as usize;
            let window_top = (window_bottom + Self::WINDOW_SIZE_MAX).min(self.data.len());
            &self.data[window_bottom..window_top]
        } else {
            &[]
        }
    }

    pub fn get_data_window_mut(&mut self) -> &mut [u8] {
        if (self.target_offset as usize) < self.data.len() {
            let window_bottom = self.target_offset as usize;
            let window_top = (window_bottom + Self::WINDOW_SIZE_MAX).min(self.data.len());
            &mut self.data[window_bottom..window_top]
        } else {
            &mut []
        }
    }

    fn reported_size(&self) -> u16 {
        let window_size = self.get_data_window().len() as u16;
        if self.memory_size != 0 {
            self.memory_size.min(window_size)
        } else {
            window_size
        }
    }
}

impl Drop for BlockDevice {
    fn drop(&mut self) {
        self.sync();
    }
}

impl ProcessorDevice for BlockDevice {
    fn device_type(&self) -> DeviceType {
        DeviceType::BlockDevice
    }
}

// [2] DeviceID
// [1] Parked
// [1] Ready?
// [4] TargetOffset
// [1] MoveToOffset
// [1] WriteData
// [2] MemorySize (max 256)
// [12.. Data]

impl MemorySegment for BlockDevice {
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        let local_offset = offset as usize;

        match local_offset {
            0..2 => Ok(self.device_type().get_device_id().to_be_bytes()[local_offset]),
            2 => Ok(self.parked as u8),
            3 => Ok((self.current_offset == self.target_offset) as u8),
            4..8 => Ok(self.target_offset.to_be_bytes()[local_offset - 4]),
            8 => Ok(0),
            9 => Ok(0),
            10..12 => Ok(self.reported_size().to_be_bytes()[local_offset - 10]),
            Self::CONTROL_SIZE..Self::DATA_TOP => {
                if let Some(v) = self
                    .get_data_window()
                    .get(local_offset - Self::CONTROL_SIZE)
                {
                    Ok(*v)
                } else {
                    Ok(0)
                }
            }
            _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
        }
    }

    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError> {
        let local_offset = offset as usize;

        match local_offset {
            0..2 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            2 => {
                self.parked = val == 0;
                Ok(())
            }
            3 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            4..8 => {
                let mut bytes = self.target_offset.to_be_bytes();
                bytes[local_offset - 4] = val;
                self.target_offset = u32::from_be_bytes(bytes);
                Ok(())
            }
            8 => {
                self.set_offset(self.target_offset);
                Ok(())
            }
            9 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            10..12 => {
                let mut temp_array = self.memory_size.to_be_bytes();
                temp_array[local_offset - 10] = val;
                self.memory_size = u16::from_be_bytes(temp_array);
                Ok(())
            }
            Self::CONTROL_SIZE..Self::DATA_TOP => {
                if let Some(v) = self
                    .get_data_window_mut()
                    .get_mut(local_offset - Self::CONTROL_SIZE)
                {
                    *v = val;
                }
                Ok(())
            }
            _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
        }
    }

    fn len(&self) -> u32 {
        DEVICE_BLOCK_MEM_SIZE
    }

    fn reset(&mut self) {
        self.set_offset(0);
    }
}
