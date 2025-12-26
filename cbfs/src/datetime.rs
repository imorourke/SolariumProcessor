use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CbDateTime {
    pub year: u8,
    pub month: u8,
    pub day: u8,
    pub hour: u8,
    pub minute: u8,
    pub second: u8,
}

impl CbDateTime {
    const YEAR_BITS: u32 = 8;
    const MONTH_BITS: u32 = 4;
    const DAY_BITS: u32 = 5;
    const HOUR_BITS: u32 = 5;
    const MINUTE_BITS: u32 = 6;
    const SECOND_BITS: u32 = 6;

    const YEAR_MASK: BitMask = BitMask::new(Self::YEAR_BITS, 0);
    const MONTH_MASK: BitMask = BitMask::new(Self::MONTH_BITS, Self::YEAR_MASK.offset);
    const DAY_MASK: BitMask = BitMask::new(Self::DAY_BITS, Self::MONTH_MASK.offset);
    const HOUR_MASK: BitMask = BitMask::new(Self::HOUR_BITS, Self::DAY_MASK.offset);
    const MINUTE_MASK: BitMask = BitMask::new(Self::MINUTE_BITS, Self::HOUR_MASK.offset);
    const SECOND_MASK: BitMask = BitMask::new(Self::SECOND_BITS, Self::MINUTE_MASK.offset);
}

impl From<u32> for CbDateTime {
    fn from(value: u32) -> Self {
        Self {
            year: Self::YEAR_MASK.get_val(value) as u8,
            month: Self::MONTH_MASK.get_val(value) as u8,
            day: Self::DAY_MASK.get_val(value) as u8,
            hour: Self::HOUR_MASK.get_val(value) as u8,
            minute: Self::MINUTE_MASK.get_val(value) as u8,
            second: Self::SECOND_MASK.get_val(value) as u8,
        }
    }
}

impl From<CbDateTime> for u32 {
    fn from(value: CbDateTime) -> Self {
        let mut word = 0;
        CbDateTime::YEAR_MASK.set_val(&mut word, value.year as u32);
        CbDateTime::MONTH_MASK.set_val(&mut word, value.month as u32);
        CbDateTime::DAY_MASK.set_val(&mut word, value.day as u32);
        CbDateTime::HOUR_MASK.set_val(&mut word, value.hour as u32);
        CbDateTime::MINUTE_MASK.set_val(&mut word, value.minute as u32);
        CbDateTime::SECOND_MASK.set_val(&mut word, value.second as u32);
        word
    }
}

impl Display for CbDateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}/{}/{} {}:{}:{}",
            self.year,
            self.month + 1,
            self.day + 1,
            self.hour,
            self.minute,
            self.second
        )
    }
}

#[derive(Debug, Clone, Copy)]
struct BitMask {
    pub offset: u32,
    pub mask: u32,
}

impl BitMask {
    const fn new(count: u32, offset: u32) -> Self {
        let mut i = 0;
        let mut mask = 0;
        while i < count {
            let bit = offset + i;
            assert!(bit < u32::BITS);
            mask |= 1 << bit;
            i += 1;
        }
        Self { offset, mask }
    }

    pub const fn get_val(&self, word: u32) -> u32 {
        (word & self.mask) >> self.offset
    }

    pub fn set_val(&self, word: &mut u32, val: u32) {
        *word = (*word & !self.mask) | ((val << self.offset) & self.mask);
    }
}
