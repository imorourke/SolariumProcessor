use std::fmt::Display;

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbVolumeHeader {
    version: U32,
    sector_size: U32,
    sector_count: U32,
    volume_name: [u8; 16],
    root_node: U16,
}

impl CbVolumeHeader {
    pub fn volume_byte_size(&self) -> usize {
        self.sector_size.get() as usize * self.sector_count.get() as usize
    }
}

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

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbDirectoryHeader {
    name: [u8; Self::NAME_SIZE],
    attributes: u8,
    reserved: u8,
    next_block: U32,
    modification_time: U32,
    byte_size: U32,
}

impl CbDirectoryHeader {
    pub const NAME_SIZE: usize = 12;

    pub const ATT_DIRECTORY: u8 = 1 << 0;

    pub fn to_name(s: &str) -> Result<[u8; Self::NAME_SIZE], CbFileError> {
        let mut vals = [0; Self::NAME_SIZE];

        if s.len() > vals.len() {
            return Err(CbFileError::InvalidName);
        }

        for (v, c) in vals.iter_mut().zip(s.chars()) {
            *v = c as u8;
        }

        Ok(vals)
    }

    pub fn get_name(&self) -> String {
        self.name
            .iter()
            .take_while(|c| **c != 0)
            .map(|c| *c as char)
            .collect()
    }

    pub fn is_directory(&self) -> bool {
        (self.attributes & Self::ATT_DIRECTORY) != 0
    }
}

#[derive(Debug, Clone)]
pub enum CbFileError {
    FileNotFound(u32),
    InvalidPath(String),
    PathNotFound(String),
    InvalidName,
    NodeNotFile,
    NodeNotDirectory,
    TableFull,
    CriticalError(String),
}

pub struct CbFileSystem {
    header: CbVolumeHeader,
    nodes: Vec<u16>,
    data: Vec<u8>,
}

impl CbFileSystem {
    const CURRENT_VERSION: u32 = 1;
    const NODE_END: u16 = 0x7FFF;
    const NODE_FILE: u16 = 0x8000;

    const fn is_node_end(node: u16) -> bool {
        node & Self::NODE_END == Self::NODE_END
    }

    const fn is_node_file(node: u16) -> bool {
        node & Self::NODE_FILE == Self::NODE_FILE
    }

    const fn is_node_dir(node: u16) -> bool {
        node & Self::NODE_FILE == 0
    }

    const fn is_node_valid(node: u16) -> bool {
        node != 0
    }

    pub fn get_node_for_path(&self, path: &str) -> Option<u16> {
        None
    }

    pub fn get_directory_listing(&self, dir: u16) -> Result<Vec<CbDirectoryHeader>, CbFileError> {
        const DIR_SIZE: usize = std::mem::size_of::<CbDirectoryHeader>();

        if Self::is_node_dir(dir) {
            let data = self.get_node_data(dir)?;
            let mut dirs = Vec::new();
            let mut current_ind = 0;

            loop {
                if current_ind + DIR_SIZE <= data.len() && data[current_ind] != 0 {
                    let dirval = match CbDirectoryHeader::read_from_bytes(
                        &data[current_ind..(current_ind + DIR_SIZE)],
                    ) {
                        Ok(v) => v,
                        Err(e) => return Err(CbFileError::CriticalError(format!("{e}"))),
                    };
                    dirs.push(dirval);
                    current_ind += DIR_SIZE;
                } else {
                    break;
                }
            }

            Ok(dirs)
        } else {
            Err(CbFileError::NodeNotDirectory)
        }
    }

    pub fn get_node_data(&self, node: u16) -> Result<Vec<u8>, CbFileError> {
        let mut data = Vec::new();
        let mut current = node;
        let ssize = self.header.sector_size.get() as usize;
        loop {
            if !Self::is_node_valid(current) {
                return Err(CbFileError::CriticalError(format!(
                    "node {current} not valid for data"
                )));
            }

            let i1 = current as usize * ssize as usize;
            let i2 = i1 + ssize as usize;
            data.extend_from_slice(&self.data[i1..i2]);

            if data.len() > self.header.volume_byte_size() {
                return Err(CbFileError::CriticalError(format!(
                    "loop detected in obtaining file data - got {}",
                    data.len()
                )));
            } else if Self::is_node_end(current) {
                break;
            }

            if let Some(next) = self.nodes.get(current as usize) {
                current = *next;
            } else {
                return Err(CbFileError::CriticalError(format!(
                    "node {current} extends past table"
                )));
            }
        }

        Ok(data)
    }

    pub fn set_file_data(&self, file: u16, data: &[u8]) -> Result<(), CbFileError> {
        todo!()
    }

    pub fn mkdirn(&self, parent: u16, name: &str) -> Result<u16, CbFileError> {
        todo!()
    }

    pub fn mkfilen(&self, parent: u16, data: &[u8]) -> Result<u16, CbFileError> {
        todo!()
    }

    pub fn rmfilen(&self, node: u16) -> Result<(), CbFileError> {
        todo!()
    }

    pub fn rmdirn(&self, node: u16) -> Result<(), CbFileError> {
        todo!()
    }

    fn use_new_sector(&mut self, existing_sector: u16) -> Result<u16, CbFileError> {
        if let Some(next) = self
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, x)| **x == 0)
            .map(|(i, _)| i as u16)
            .next()
        {
            self.nodes[existing_sector as usize] = next;
            self.nodes[next as usize] = 0xFFFF;
            Ok(next as u16)
        } else {
            Err(CbFileError::TableFull)
        }
    }
}

impl Default for CbFileSystem {
    fn default() -> Self {
        const DEFAULT_NAME: &str = "root";

        let mut name = [b'\0'; 16];
        for (v, c) in name.iter_mut().zip(DEFAULT_NAME.chars()) {
            *v = c as u8;
        }

        let header = CbVolumeHeader {
            version: Self::CURRENT_VERSION.into(),
            volume_name: name,
            sector_count: 1024.into(),
            sector_size: 256.into(),
            root_node: 0.into(),
        };
        let sector_count = header.sector_count.get();
        let sector_size = header.sector_size.get();

        let mut data = vec![0u8; sector_count as usize * sector_size as usize];
        header.write_to(data.as_mut_slice()).unwrap();

        let table = vec![0u16; sector_count as usize];

        Self {
            data,
            nodes: table,
            header,
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_file_nodes() {
        panic!()
    }
}
