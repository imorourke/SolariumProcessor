#![no_std]

extern crate alloc;

use alloc::boxed::Box;
use alloc::vec::Vec;

/// Implements a simple circular buffer to store data within a known quantity of items
#[derive(Debug, Clone)]
pub struct CircularBuffer<T: Copy + Clone + Default, const S: usize> {
    /// The raw data values
    buffer: [T; S],
    /// The next index to write to
    idx_write: usize,
    /// The next index to read from
    idx_read: Option<usize>,
    /// Determines if we have looped around or not
    looped: bool,
}

impl<T: Default + Clone + Copy, const S: usize> CircularBuffer<T, S> {
    /// Determines the current length of the buffer
    pub fn len(&self) -> usize {
        if let Some(mut i) = self.idx_read {
            let mut l = 0;

            if i == self.idx_write {
                if !self.looped {
                    return 1;
                }

                l += 1;
                i = Self::next_idx(i);
            }

            while i != self.idx_write {
                l += 1;
                i = Self::next_idx(i);
            }

            l
        } else {
            0
        }
    }

    /// Obtains the current list of elements
    pub fn elts(&self) -> Vec<T> {
        let mut v = Vec::new();

        if let Some(mut i) = self.idx_read {
            if i == self.idx_write {
                v.push(self.buffer[i]);

                if !self.looped {
                    return v;
                }

                i = Self::next_idx(i);
            }

            while i != self.idx_write {
                v.push(self.buffer[i]);
                i = Self::next_idx(i);
            }
        }

        v
    }

    /// Returns true if the list is empty
    pub const fn is_empty(&self) -> bool {
        self.idx_read.is_none()
    }

    /// Clears all elements from the list
    pub fn clear(&mut self) {
        self.idx_read = None;
        self.looped = false;
        self.idx_write = 0;
        self.buffer.fill(T::default());
    }

    /// Provides the next index for the given size
    const fn next_idx(i: usize) -> usize {
        (i + 1) % S
    }

    /// Pushes an element into the buffer
    pub fn push(&mut self, val: T) {
        let next_write = Self::next_idx(self.idx_write);

        if let Some(idx) = self.idx_read
            && self.idx_write == idx
        {
            self.idx_read = Some(Self::next_idx(idx));
            self.looped = true;
        } else if self.idx_read.is_none() {
            self.idx_read = Some(self.idx_write);
        }

        self.buffer[self.idx_write] = val;
        self.idx_write = next_write;
    }

    /// Peeks at the next element to be read from the buffer
    fn peek_idx(&self) -> Option<(T, usize)> {
        if let Some(idx) = self.idx_read {
            Some((self.buffer[idx], idx))
        } else {
            None
        }
    }

    /// Pops an element from the buffer
    pub fn pop(&mut self) -> Option<T> {
        if let Some((val, idx)) = self.peek_idx() {
            let nidx = Self::next_idx(idx);
            self.looped = false;
            if nidx == self.idx_write {
                self.idx_read = None;
            } else {
                self.idx_read = Some(nidx);
            }
            Some(val)
        } else {
            None
        }
    }

    /// Peeks at the next element to be read from the buffer
    pub fn peek(&self) -> Option<T> {
        self.peek_idx().map(|x| x.0)
    }
}

impl<T: Default + Clone + Copy, const S: usize> Default for CircularBuffer<T, S> {
    fn default() -> Self {
        assert_ne!(S, 0);
        Self {
            buffer: [T::default(); S],
            idx_write: 0,
            idx_read: None,
            looped: false,
        }
    }
}

/// Implements a simple circular buffer to store data within a known quantity of items
#[derive(Clone)]
pub struct CircularBufferDyn<T: Copy + Clone + Default> {
    /// The raw data values
    buffer: Box<[T]>,
    /// The next index to write to
    idx_write: usize,
    /// The next index to read from
    idx_read: Option<usize>,
    /// Whether we have looped around or not
    looped: bool,
}

impl<T: Default + Clone + Copy> CircularBufferDyn<T> {
    /// Creates a new dynamic circular buffer
    pub fn new(size: usize) -> Self {
        assert_ne!(size, 0);
        Self {
            buffer: unsafe { Box::new_zeroed_slice(size).assume_init() },
            idx_write: 0,
            idx_read: None,
            looped: false,
        }
    }

    /// Determines the current length of the buffer
    pub fn len(&self) -> usize {
        if let Some(mut i) = self.idx_read {
            let mut l = 0;

            if i == self.idx_write {
                if !self.looped {
                    return 1;
                }

                l += 1;
                i = self.next_idx(i);
            }

            while i != self.idx_write {
                l += 1;
                i = self.next_idx(i);
            }

            l
        } else {
            0
        }
    }

    /// Obtains the current list of elements
    pub fn elts(&self) -> Vec<T> {
        let mut v = Vec::new();

        if let Some(mut i) = self.idx_read {
            if i == self.idx_write {
                v.push(self.buffer[i]);

                if !self.looped {
                    return v;
                }

                i = self.next_idx(i);
            }

            while i != self.idx_write {
                v.push(self.buffer[i]);
                i = self.next_idx(i);
            }
        }

        v
    }

    /// Returns true if the list is empty
    pub const fn is_empty(&self) -> bool {
        self.idx_read.is_none()
    }

    /// Clears elements in the array
    pub fn clear(&mut self) {
        self.idx_read = None;
        self.idx_write = 0;
        self.looped = false;
        self.buffer.fill(T::default());
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
            self.looped = true;
        } else if self.idx_read.is_none() {
            self.idx_read = Some(self.idx_write);
        }

        self.buffer[self.idx_write] = val;
        self.idx_write = next_write;
    }

    /// Peeks at the next element to be read from the buffer
    fn peek_idx(&self) -> Option<(T, usize)> {
        if let Some(idx) = self.idx_read {
            Some((self.buffer[idx], idx))
        } else {
            None
        }
    }

    /// Pops an element from the buffer
    pub fn pop(&mut self) -> Option<T> {
        if let Some((val, idx)) = self.peek_idx() {
            let nidx = self.next_idx(idx);
            self.looped = false;
            if nidx == self.idx_write {
                self.idx_read = None;
            } else {
                self.idx_read = Some(nidx);
            }
            Some(val)
        } else {
            None
        }
    }

    /// Peeks at the next element to be read from the buffer
    pub fn peek(&self) -> Option<T> {
        self.peek_idx().map(|x| x.0)
    }
}

#[cfg(test)]
mod test {
    use super::{CircularBuffer, CircularBufferDyn};
    use alloc::vec;

    #[test]
    fn test_single_value() {
        let mut buf = CircularBuffer::<u8, 1>::default();
        assert!(buf.pop().is_none());

        buf.push(1);
        assert_eq!(buf.elts(), vec![1]);
        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.elts(), vec![2]);
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        assert_eq!(buf.elts(), vec![]);
    }

    #[test]
    fn test_multi_value2() {
        let mut buf = CircularBuffer::<u8, 2>::default();
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
    fn test_multi_value3() {
        let mut buf = CircularBuffer::<u8, 3>::default();
        assert!(buf.pop().is_none());

        assert_eq!(buf.elts(), vec![]);
        buf.push(1);
        assert_eq!(buf.elts(), vec![1]);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.elts(), vec![1]);
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);

        assert_eq!(buf.elts(), vec![1, 2]);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.elts(), vec![1, 2]);
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.elts(), vec![2]);
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        buf.push(3);
        buf.push(4);

        assert_eq!(buf.elts(), vec![2, 3, 4]);

        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![3, 4]);
        assert_eq!(buf.peek(), Some(3));
        assert_eq!(buf.pop(), Some(3));
        assert_eq!(buf.elts(), vec![4]);
        assert_eq!(buf.peek(), Some(4));
        assert_eq!(buf.pop(), Some(4));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        assert_eq!(buf.elts(), vec![]);
    }

    #[test]
    fn test_dyn_single_value() {
        let mut buf = CircularBufferDyn::<u8>::new(1);
        assert!(buf.pop().is_none());

        buf.push(1);
        assert_eq!(buf.elts(), vec![1]);
        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.elts(), vec![2]);
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        assert_eq!(buf.elts(), vec![]);
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

        assert_eq!(buf.elts(), vec![]);
        buf.push(1);
        assert_eq!(buf.elts(), vec![1]);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.elts(), vec![1]);
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);

        assert_eq!(buf.elts(), vec![1, 2]);

        assert_eq!(buf.peek(), Some(1));
        assert_eq!(buf.elts(), vec![1, 2]);
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.elts(), vec![2]);
        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![]);
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        buf.push(1);
        buf.push(2);
        buf.push(3);
        buf.push(4);

        assert_eq!(buf.elts(), vec![2, 3, 4]);

        assert_eq!(buf.peek(), Some(2));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf.elts(), vec![3, 4]);
        assert_eq!(buf.peek(), Some(3));
        assert_eq!(buf.pop(), Some(3));
        assert_eq!(buf.elts(), vec![4]);
        assert_eq!(buf.peek(), Some(4));
        assert_eq!(buf.pop(), Some(4));
        assert_eq!(buf.peek(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);
        assert_eq!(buf.pop(), None);

        assert_eq!(buf.elts(), vec![]);
    }
}
