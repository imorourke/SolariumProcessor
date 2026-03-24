use core::cell::RefCell;

use super::{DEVICE_ID_SIZE, DEVICE_MEM_SIZE, ProcessorDevice};

use crate::{
    device::{DeviceAction, DeviceType, circ_buf::CircularBufferDyn},
    memory::{MemorySegment, MemorySegmentError},
};

/// Implements a simple circular buffer to store data within a known quantity of items
#[derive(Clone, Copy)]
pub struct CircularBuffer<T: Copy + Clone + Default, const S: usize> {
    /// The raw data values
    buffer: [T; S],
    /// The next index to write to
    idx_write: usize,
    /// The next index to read from
    idx_read: usize,
}

impl<T: Default + Clone + Copy, const S: usize> CircularBuffer<T, S> {
    /// Provides the next index for the given size
    const fn next_idx(i: usize) -> usize {
        (i + 1) % S
    }

    /// Pushes an element into the buffer
    pub fn push(&mut self, val: T) {
        let next_write = Self::next_idx(self.idx_write);
        if next_write == self.idx_read {
            self.idx_read = Self::next_idx(self.idx_read);
        }
        self.buffer[self.idx_write] = val;
        self.idx_write = next_write;
    }

    /// Pops an element from the buffer
    pub fn pop(&mut self) -> Option<T> {
        if let Some(val) = self.peek() {
            self.idx_read = Self::next_idx(self.idx_read);
            Some(val)
        } else {
            None
        }
    }

    /// Peeks at the next element to be read from the buffer
    pub fn peek(&self) -> Option<T> {
        if self.idx_read != self.idx_write {
            Some(self.buffer[self.idx_read])
        } else {
            None
        }
    }
}

impl<T: Default + Clone + Copy, const S: usize> Default for CircularBuffer<T, S> {
    fn default() -> Self {
        Self {
            buffer: [T::default(); S],
            idx_write: 0,
            idx_read: 0,
        }
    }
}

/// Provides a memory serial I/O memory-mapped device
pub struct SerialInputOutputDevice {
    /// Provides the base address for the input device
    input_queue: RefCell<CircularBufferDyn<u8>>,
    output_queue: CircularBufferDyn<u8>,
    interrupt_char: u8,
    interrupt_num: u8,
    interrupt_triggered: bool,
}

/// Defines constant values for the memory address offsets
impl SerialInputOutputDevice {
    // Define memory size and offset values
    const OFFSET_INPUT_SIZE: u32 = 2;
    const OFFSET_OUTPUT_SIZE: u32 = 4;
    const OFFSET_INPUT_DATA: u32 = 6;
    const OFFSET_OUTPUT_DATA: u32 = 7;
    const OFFSET_INPUT_RESET_IN: u32 = 8;
    const OFFSET_INPUT_RESET_OUT: u32 = 9;
    const OFFSET_INTERRUPT_NUM: u32 = 10;
    const OFFSET_INTERRUPT_CHAR: u32 = 11;

    /// Constructs a new serial device
    pub fn new(buffer_size: usize) -> SerialInputOutputDevice {
        // Construct the serial device output
        Self {
            input_queue: RefCell::new(CircularBufferDyn::<u8>::new(buffer_size)),
            output_queue: CircularBufferDyn::<u8>::new(buffer_size),
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
        self.input_queue.borrow_mut().push(val);
        if self.interrupt_char != 0 && val == self.interrupt_char && self.interrupt_num != 0 {
            self.interrupt_triggered = true;
        }
        true
    }

    /// Pops the output value from the output queue and returns
    pub fn pop_output(&mut self) -> Option<u8> {
        self.output_queue.pop()
    }

    fn common_get(&self, offset: u32) -> Result<u8, MemorySegmentError> {
        // Use the offset values to determine the action to take
        match offset {
            n if n < DEVICE_ID_SIZE => {
                Ok(self.device_type().get_device_id().to_be_bytes()[offset as usize])
            }
            x if (Self::OFFSET_INPUT_SIZE..Self::OFFSET_INPUT_SIZE + 2).contains(&x) => Ok(
                ((u16::MAX as usize).min(self.input_queue.borrow().len()) as u16).to_be_bytes()
                    [x as usize - Self::OFFSET_INPUT_SIZE as usize],
            ),
            x if (Self::OFFSET_OUTPUT_SIZE..Self::OFFSET_OUTPUT_SIZE + 2).contains(&x) => Ok(
                ((u16::MAX as usize).min(self.output_queue.len()) as u16).to_be_bytes()
                    [x as usize - Self::OFFSET_OUTPUT_SIZE as usize],
            ),
            Self::OFFSET_OUTPUT_DATA
            | Self::OFFSET_INPUT_RESET_IN
            | Self::OFFSET_INPUT_RESET_OUT => Ok(0),
            Self::OFFSET_INPUT_DATA => match self.input_queue.borrow().peek() {
                Some(v) => Ok(v),
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
            Self::OFFSET_INPUT_DATA => match self.input_queue.borrow_mut().pop() {
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
                self.output_queue.push(data);
                Ok(())
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
        if self.interrupt_triggered {
            self.interrupt_triggered = false;
            if self.interrupt_num != 0 {
                Some(DeviceAction::CallInterrupt(self.interrupt_num as u32))
            } else {
                None
            }
        } else {
            None
        }
    }
}
