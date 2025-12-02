#[cfg(feature = "std")]
use alloc::string::String;
use alloc::{boxed::Box, vec::Vec};

use crate::{
    device::{DEVICE_BLOCK_MEM_SIZE, DEVICE_ID_SIZE, DeviceType, ProcessorDevice},
    memory::{MemorySegment, MemorySegmentError},
};

pub struct BlockDevice {
    data: Vec<u8>,
    current_offset: u32,
    target_offset: u32,
    parked: bool,
    on_sync: Option<alloc::boxed::Box<dyn Fn(&[u8]) -> bool>>,
}

impl BlockDevice {
    const WINDOW_SIZE_MAX: usize = 192;

    pub fn new<T: IntoIterator<Item = u8>>(data: T) -> Self {
        Self {
            data: data.into_iter().collect(),
            current_offset: 0,
            target_offset: 0,
            parked: true,
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
            on_sync: Some(Box::new(move |data| match std::fs::write(&pb, data) {
                Ok(_) => true,
                Err(_) => false,
            })),
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
// [.. Data]

impl MemorySegment for BlockDevice {
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Ok(self.device_type().get_device_id().to_be_bytes()[offset as usize])
        } else {
            let local_offset = offset - DEVICE_ID_SIZE;
            const DATA_TOP: u32 = DEVICE_BLOCK_MEM_SIZE + 10;

            match local_offset {
                0 => Ok(self.parked as u8),
                1 => Ok((self.current_offset == self.target_offset) as u8),
                2..6 => Ok(self.target_offset.to_be_bytes()[local_offset as usize - 2]),
                6 => Ok(0),
                7 => Ok(0),
                8..10 => {
                    Ok((self.get_data_window().len() as u16).to_be_bytes()
                        [local_offset as usize - 8])
                }
                10..DATA_TOP => {
                    if let Some(v) = self.get_data_window().get(local_offset as usize - 10) {
                        Ok(*v)
                    } else {
                        Ok(0)
                    }
                }
                _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            }
        }
    }

    fn set(&mut self, offset: u32, val: u8) -> Result<(), MemorySegmentError> {
        if offset < DEVICE_ID_SIZE {
            Err(MemorySegmentError::InvalidMemoryAccess(offset))
        } else {
            let local_offset = offset - DEVICE_ID_SIZE;
            const DATA_TOP: u32 = DEVICE_BLOCK_MEM_SIZE + 10;

            match local_offset {
                0 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
                1 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
                2..6 => {
                    let mut bytes = self.target_offset.to_be_bytes();
                    bytes[local_offset as usize - 2] = val;
                    self.target_offset = u32::from_be_bytes(bytes);
                    Ok(())
                }
                6 => {
                    self.set_offset(self.target_offset);
                    Ok(())
                }
                7 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
                8..10 => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
                10..DATA_TOP => {
                    if let Some(v) = self
                        .get_data_window_mut()
                        .get_mut(local_offset as usize - 10)
                    {
                        *v = val;
                    }
                    Ok(())
                }
                _ => Err(MemorySegmentError::InvalidMemoryAccess(offset)),
            }
        }
    }

    fn len(&self) -> u32 {
        DEVICE_BLOCK_MEM_SIZE
    }

    fn reset(&mut self) {
        self.set_offset(0);
    }
}
