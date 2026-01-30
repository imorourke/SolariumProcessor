use alloc::collections::VecDeque;
use core::cell::RefCell;

use super::{DEVICE_ID_SIZE, DEVICE_MEM_SIZE, ProcessorDevice};

use crate::{
    device::{DeviceAction, DeviceType},
    memory::{MemorySegment, MemorySegmentError},
};

/// Provides a memory serial I/O memory-mapped device
pub struct SerialInputOutputDevice {
    /// Provides the base address for the input device
    input_queue: RefCell<VecDeque<u8>>,
    output_queue: VecDeque<u8>,
    buffer_size: usize,
    interrupt_char: u8,
    interrupt_num: u8,
    interrupt_triggered: bool,
}

/// Defines constant values for the memory address offsets
impl SerialInputOutputDevice {
    // Define memory size and offset values
    const OFFSET_INPUT_SIZE: u32 = 2;
    const OFFSET_INPUT_DATA: u32 = 3;
    const OFFSET_OUTPUT_SIZE: u32 = 4;
    const OFFSET_OUTPUT_DATA: u32 = 5;
    const OFFSET_INPUT_RESET_IN: u32 = 6;
    const OFFSET_INPUT_RESET_OUT: u32 = 7;
    const OFFSET_INTERRUPT_NUM: u32 = 8;
    const OFFSET_INTERRUPT_CHAR: u32 = 9;

    /// Constructs a new serial device
    pub fn new(buffer_size: usize) -> SerialInputOutputDevice {
        // Construct the serial device output
        Self {
            input_queue: RefCell::new(VecDeque::new()),
            output_queue: VecDeque::new(),
            buffer_size,
            interrupt_char: 0,
            interrupt_num: 0,
            interrupt_triggered: false,
        }
    }

    /// Determines if there is output in the queue
    pub fn has_output(&self) -> bool {
        !self.output_queue.is_empty()
    }

    /// Determines if there is input in the queue
    pub fn has_input(&self) -> bool {
        return !self.input_queue.borrow().is_empty();
    }

    /// Pushes the input value into the input queue
    pub fn push_input(&mut self, val: u8) -> bool {
        if self.input_queue.borrow().len() < self.buffer_size {
            self.input_queue.borrow_mut().push_back(val);
            if self.interrupt_char != 0 && val == self.interrupt_char {
                self.interrupt_triggered = true;
            }
            true
        } else {
            false
        }
    }

    /// Pops the output value from the output queue and returns
    pub fn pop_output(&mut self) -> Option<u8> {
        self.output_queue.pop_front()
    }

    fn common_get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            n if n < DEVICE_ID_SIZE => {
                Ok(self.device_type().get_device_id().to_be_bytes()[offset as usize])
            }
            Self::OFFSET_INPUT_SIZE => {
                Ok((u8::MAX as usize).min(self.input_queue.borrow().len()) as u8)
            }
            Self::OFFSET_OUTPUT_SIZE => Ok((u8::MAX as usize).min(self.output_queue.len()) as u8),
            Self::OFFSET_OUTPUT_DATA
            | Self::OFFSET_INPUT_RESET_IN
            | Self::OFFSET_INPUT_RESET_OUT => Ok(0),
            Self::OFFSET_INPUT_DATA => match self.input_queue.borrow().front() {
                Some(v) => Ok(*v),
                None => Ok(0),
            },
            Self::OFFSET_INTERRUPT_NUM => Ok(self.interrupt_num),
            Self::OFFSET_INTERRUPT_CHAR => Ok(self.interrupt_char),
            _ => Ok(0),
        }
    }
}

impl MemorySegment for SerialInputOutputDevice {
    /// Provides the word at the requested memory location
    fn get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            Self::OFFSET_INPUT_DATA => match self.input_queue.borrow_mut().pop_front() {
                Some(v) => Ok(v),
                None => Ok(0),
            },
            x => self.common_get(x),
        }
    }

    /// Provides the word at the requested memory location without affecting the device state
    fn inspect(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        self.common_get(offset)
    }

    /// Sets the word at the requested memory location with the given data
    /// Returns true if the value could be set; otherwise returns false
    fn set(&mut self, offset: u32, data: u8) -> Result<(), MemorySegmentError> {
        // Return error if not within the given offset value
        if !self.within(offset) {
            return Err(MemorySegmentError::InvalidMemoryAccess(offset));
        }

        // Extract the offset and match based on the result
        match offset {
            Self::OFFSET_OUTPUT_DATA => {
                if self.output_queue.len() < self.buffer_size {
                    self.output_queue.push_back(data);
                    Ok(())
                } else {
                    Err(MemorySegmentError::InvalidMemoryWrite(offset, data))
                }
            }
            Self::OFFSET_INPUT_RESET_IN => {
                if data != 0 {
                    self.input_queue.borrow_mut().clear();
                }
                Ok(())
            }
            Self::OFFSET_INPUT_RESET_OUT => {
                if data != 0 {
                    self.output_queue.clear();
                }
                Ok(())
            }
            Self::OFFSET_INTERRUPT_NUM => {
                self.interrupt_num = data;
                Ok(())
            }
            Self::OFFSET_INTERRUPT_CHAR => {
                self.interrupt_char = data;
                Ok(())
            }
            _ => Err(MemorySegmentError::InvalidMemoryWrite(offset, data)),
        }
    }

    /// Resets the memory segment
    fn reset(&mut self) {
        self.input_queue.borrow_mut().clear();
        self.output_queue.clear();
    }

    /// Provides the length of the memory segment
    fn len(&self) -> u32 {
        DEVICE_MEM_SIZE
    }
}

impl ProcessorDevice for SerialInputOutputDevice {
    fn device_type(&self) -> DeviceType {
        DeviceType::SerialIO
    }

    fn on_step(&mut self) -> Option<DeviceAction> {
        if self.interrupt_triggered && self.interrupt_num != 0 {
            self.interrupt_triggered = false;
            Some(DeviceAction::CallInterrupt(self.interrupt_num as u32))
        } else {
            None
        }
    }
}
