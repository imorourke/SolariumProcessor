use std::boxed::Box;

/// Implements a simple circular buffer to store data within a known quantity of items
#[derive(Clone)]
pub struct CircularBufferDyn<T: Copy + Clone + Default> {
    /// The raw data values
    buffer: Box<[T]>,
    /// The next index to write to
    idx_write: usize,
    /// The next index to read from
    idx_read: Option<usize>,
}

impl<T: Default + Clone + Copy> CircularBufferDyn<T> {
    /// Creates a new dynamic circular buffer
    pub fn new(size: usize) -> Self {
        assert_ne!(size, 0);
        Self {
            buffer: unsafe { Box::new_zeroed_slice(size).assume_init() },
            idx_write: 0,
            idx_read: None,
        }
    }

    /// Provides the current length of the elements
    pub fn len(&self) -> usize {
        if let Some(mut current) = self.idx_read {
            let mut l = 0;
            while current != self.idx_write {
                current = self.next_idx(current);
                l += 1;
            }
            l
        } else {
            0
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.idx_read.is_none()
    }

    pub fn clear(&mut self) {
        self.idx_read = None;
    }

    /// Provides the next index for the given size
    const fn next_idx(&self, i: usize) -> usize {
        (i + 1) % self.buffer.len()
    }

    /// Pushes an element into the buffer
    pub fn push(&mut self, val: T) {
        let next_write = self.next_idx(self.idx_write);

        if let Some(idx) = self.idx_read
            && self.idx_write == idx
        {
            self.idx_read = Some(self.next_idx(idx));
        } else if self.idx_read.is_none() {
            self.idx_read = Some(self.idx_write);
        }

        self.buffer[self.idx_write] = val;
        self.idx_write = next_write;
    }

    /// Pops an element from the buffer
    pub fn pop(&mut self) -> Option<T> {
        if let Some(val) = self.peek() {
            if let Some(idx) = self.idx_read {
                let nidx = self.next_idx(idx);
                if nidx == self.idx_write {
                    self.idx_read = None;
                } else {
                    self.idx_read = Some(nidx);
                }
            }
            Some(val)
        } else {
            None
        }
    }

    /// Peeks at the next element to be read from the buffer
    pub fn peek(&self) -> Option<T> {
        if let Some(idx) = self.idx_read {
            Some(self.buffer[idx])
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::CircularBufferDyn;

    #[test]
    fn test_dyn_single_value() {
        let mut buf = CircularBufferDyn::<u8>::new(1);
        assert!(buf.pop().is_none());

        buf.push(1);
        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
    }

    #[test]
    fn test_dyn_multi_value2() {
        let mut buf = CircularBufferDyn::<u8>::new(2);
        assert!(buf.pop().is_none());

        buf.push(1);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        buf.push(3);

        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.peek(), Some(3));
        assert_eq!(buf.pop(), Some(3));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
    }

    #[test]
    fn test_dyn_multi_value3() {
        let mut buf = CircularBufferDyn::<u8>::new(3);
        assert!(buf.pop().is_none());

        buf.push(1);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        buf.push(3);
        buf.push(4);

        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.peek(), Some(3));
        assert_eq!(buf.pop(), Some(3));
        assert_eq!(buf.peek(), Some(4));
        assert_eq!(buf.pop(), Some(4));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
    }
}
