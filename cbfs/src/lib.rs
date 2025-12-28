mod datetime;
mod entries;
mod names;

use std::{
    fmt::Debug,
    fs::OpenOptions,
    io::{Seek, SeekFrom, Write},
    path::Path,
};

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

pub use crate::{
    entries::{CbEntryHeader, CbEntryType},
    names::{StringArrayError, string_to_array},
};

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbVolumeHeader {
    pub version: U16,
    pub sector_size: U16,
    pub sector_count: U16,
    pub volume_name: [u8; 16],
    pub root_sector: U16,
    reserved: [u8; 40],
}

impl CbVolumeHeader {
    const CURRENT_VERSION: u16 = 1;
    const NODE_TABLE_ENTRY_SIZE: usize = std::mem::size_of::<u16>();

    pub fn new(sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let min_sector_size = std::mem::size_of::<Self>().min(std::mem::size_of::<CbEntryHeader>());

        if (sector_size as usize) < min_sector_size {
            return Err(CbfsError::SectorSizeTooSmall(sector_size));
        } else if sector_count >= CbFileSystem::NODE_END {
            return Err(CbfsError::InvalidSectorOption);
        }

        let mut header = Self {
            version: Self::CURRENT_VERSION.into(),
            sector_size: sector_size.into(),
            sector_count: sector_count.into(),
            root_sector: 0.into(),
            volume_name: string_to_array("")?,
            reserved: [0; _],
        };

        let table_size = header.sector_count.get() * Self::NODE_TABLE_ENTRY_SIZE as u16;
        header.root_sector = U16::new(table_size.div_ceil(header.sector_size.get()) + 1);

        if header.root_sector.get() >= sector_count {
            return Err(CbfsError::InvalidSectorOption);
        }

        Ok(header)
    }

    pub fn set_name(&mut self, name: &str) -> Result<(), CbfsError> {
        self.volume_name = string_to_array(name)?;
        Ok(())
    }

    pub fn volume_byte_size(&self) -> u64 {
        self.sector_size.get() as u64 * self.sector_count.get() as u64
    }

    pub fn data_sector_size(&self) -> u64 {
        (self.sector_count.get() as u64 - self.root_sector.get() as u64)
            * self.sector_size.get() as u64
    }
}

#[derive(Debug)]
pub enum CbfsError {
    InvalidNode(u16),
    InvalidPath(String),
    DuplicateName(String),
    PathNotFound(String),
    UnknownEntryId(u8),
    InvalidName,
    NodeNotFile,
    NodeNotDirectory,
    TableFull,
    CriticalError(String),
    InvalidSectorOption,
    SectorSizeTooSmall(u16),
    IoError(std::io::Error),
}

impl From<StringArrayError> for CbfsError {
    fn from(value: StringArrayError) -> Self {
        match value {
            StringArrayError::InvalidName => Self::InvalidName,
        }
    }
}

impl From<std::io::Error> for CbfsError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}

#[derive(Debug, Clone)]
pub struct CbFileSystem {
    pub header: CbVolumeHeader,
    nodes: Vec<u16>,
    data: Vec<u8>,
}

impl CbFileSystem {
    const NODE_END: u16 = 0xFFFF;

    pub fn new(name: &str, sector_size: u16, sector_count: u16) -> Result<Self, CbfsError> {
        let mut header = CbVolumeHeader::new(sector_size, sector_count)?;
        header.set_name(name)?;
        let sector_count = header.sector_count.get();

        let data = vec![0u8; header.data_sector_size() as usize];
        let mut nodes = vec![0u16; sector_count as usize];

        for i in 0..=header.root_sector.get() as usize {
            nodes[i] = Self::NODE_END;
        }

        let root_entry = CbEntryHeader {
            attributes: 0,
            entry_type: CbEntryType::Directory as u8,
            modification_time: U32::new(0),
            name: string_to_array("")?,
            parent: U16::new(0),
            payload_size: U32::new(0),
        };

        let mut fs = Self {
            data,
            nodes,
            header,
        };

        fs.set_node_data_header(fs.header.root_sector.get(), root_entry, &[])?;

        Ok(fs)
    }

    pub fn open(path: &Path) -> Result<Self, CbfsError> {
        let all_data = std::fs::read(path).unwrap();

        let hdr =
            CbVolumeHeader::read_from_bytes(&all_data[..std::mem::size_of::<CbVolumeHeader>()])
                .unwrap();

        let sect_size = hdr.sector_size.get() as usize;
        let sect_count = hdr.sector_count.get() as usize;

        let node_table = all_data
            [sect_size..(sect_size + CbVolumeHeader::NODE_TABLE_ENTRY_SIZE * sect_count)]
            .chunks(CbVolumeHeader::NODE_TABLE_ENTRY_SIZE)
            .map(|x| U16::read_from_bytes(x).unwrap().get())
            .collect::<Vec<_>>();

        let node_data = all_data[(sect_size * hdr.root_sector.get() as usize)..].to_vec();

        assert_eq!(sect_count, node_table.len());
        assert_eq!(
            node_data.len(),
            sect_size * (sect_count - hdr.root_sector.get() as usize)
        );

        Ok(Self {
            header: hdr,
            nodes: node_table,
            data: node_data,
        })
    }

    const fn is_node_end(node: u16) -> bool {
        node & Self::NODE_END == Self::NODE_END
    }

    pub const fn get_node_valid(&self, node: u16) -> Result<u16, CbfsError> {
        if node >= self.header.root_sector.get() && node < self.header.sector_count.get() {
            Ok(node)
        } else {
            Err(CbfsError::InvalidNode(node))
        }
    }

    fn get_data_start_idx(&self, node: u16) -> Result<usize, CbfsError> {
        let idx = (self.get_node_valid(node)? - self.header.root_sector.get()) as usize;
        Ok(idx * self.header.sector_size.get() as usize)
    }

    fn get_node_type(&self, node: u16) -> Result<CbEntryType, CbfsError> {
        match self.data.get(self.get_data_start_idx(node)?).copied() {
            Some(val) => Ok(CbEntryType::from(val)),
            None => Err(CbfsError::InvalidNode(node)),
        }
    }

    fn is_node_directory(&self, node: u16) -> Result<bool, CbfsError> {
        Ok(self.get_node_type(node)? == CbEntryType::Directory)
    }

    pub fn get_node_for_path(&self, path: &str) -> Result<u16, CbfsError> {
        let mut current = self.header.root_sector.get() as usize;
        let parts = path
            .split('/')
            .filter(|x| !x.is_empty())
            .collect::<Vec<_>>();

        'outer: for (i, p) in parts.iter().enumerate() {
            for d in self.get_directory_listing(current as u16)? {
                let entry = self.get_node_header(d)?;
                if &entry.get_name() == p {
                    if i + 1 >= parts.len() || entry.get_entry_type() == CbEntryType::Directory {
                        current = d as usize;
                        continue 'outer;
                    } else {
                        return Err(CbfsError::NodeNotDirectory);
                    }
                }
            }

            return Err(CbfsError::PathNotFound(path.into()));
        }

        Ok(current as u16)
    }

    pub fn get_node_header(&self, node: u16) -> Result<CbEntryHeader, CbfsError> {
        let idx = self.get_data_start_idx(node)?;
        let id = self.data[idx];
        Ok(match CbEntryType::from(id) {
            CbEntryType::File | CbEntryType::Directory => CbEntryHeader::read_from_bytes(
                &self.data[idx..(idx + std::mem::size_of::<CbEntryHeader>())],
            )
            .unwrap(),
            _ => return Err(CbfsError::UnknownEntryId(id)),
        })
    }

    pub fn set_node_header(&mut self, node: u16, hdr: &CbEntryHeader) -> Result<(), CbfsError> {
        let idx = self.get_data_start_idx(node)?;
        for (dst, src) in self.data[idx..(idx + std::mem::size_of::<CbEntryHeader>())]
            .iter_mut()
            .zip(hdr.as_bytes())
        {
            *dst = *src;
        }
        Ok(())
    }

    pub fn get_directory_listing(&self, dir: u16) -> Result<Vec<u16>, CbfsError> {
        if !self.is_node_directory(dir)? {
            return Err(CbfsError::NodeNotDirectory);
        }

        let (hdr, data) = self.get_node_data(dir)?;
        let mut dirs = Vec::new();

        for i in (0..hdr.get_payload_size()).step_by(CbVolumeHeader::NODE_TABLE_ENTRY_SIZE) {
            let node_val =
                U16::read_from_bytes(&data[i..(i + CbVolumeHeader::NODE_TABLE_ENTRY_SIZE)])
                    .unwrap()
                    .get();
            if node_val != 0 {
                dirs.push(node_val);
            }
        }

        Ok(dirs)
    }

    pub fn get_node_data_raw(&self, mut node: u16) -> Result<Vec<u8>, CbfsError> {
        let mut data = Vec::new();
        if self.nodes[self.get_node_valid(node)? as usize] == 0 {
            return Err(CbfsError::NodeNotFile);
        }

        while node != Self::NODE_END {
            assert_ne!(node, 0);
            let idx = self.get_data_start_idx(node)?;
            data.extend(&self.data[idx..(idx + self.header.sector_size.get() as usize)]);
            node = self.nodes[node as usize];
        }

        Ok(data)
    }

    pub fn get_node_data(&self, node: u16) -> Result<(CbEntryHeader, Vec<u8>), CbfsError> {
        let raw_data = self.get_node_data_raw(node)?;
        let hdr = CbEntryHeader::read_from_bytes(&raw_data[..std::mem::size_of::<CbEntryHeader>()])
            .unwrap();
        let hdr_size = hdr.get_header_size();

        Ok((hdr, raw_data[hdr_size..hdr.get_total_size()].to_vec()))
    }

    pub fn set_node_byte_size(&mut self, node: u16, size: u32) -> Result<(), CbfsError> {
        let (hdr, mut data) = self.get_node_data(node)?;
        if hdr.get_entry_type() == CbEntryType::File {
            data.resize(size as usize, 0);
            self.set_node_data_header(node, hdr, &data)
        } else {
            Err(CbfsError::NodeNotFile)
        }
    }

    pub fn num_sectors_for_node(&self, node: u16) -> usize {
        if node == 0 {
            return 0;
        }

        let mut count = 1;
        let mut current = node;
        while current != Self::NODE_END {
            count += 1;
            current = self.nodes[current as usize];
            assert_ne!(0, current);
        }

        count
    }

    fn get_next_free_node(&self) -> Result<u16, CbfsError> {
        for (i, x) in self.nodes.iter().enumerate() {
            if *x == 0 {
                return Ok(i as u16);
            }
        }

        Err(CbfsError::TableFull)
    }

    pub fn set_num_sectors(&mut self, mut node: u16, count: u16) -> Result<(), CbfsError> {
        // Check that there is enough space left in the table
        let num_free = self.nodes.iter().copied().filter(|x| *x == 0).count();

        let num_current = self.num_sectors_for_node(node);
        let num_required = self.required_sectors_for_size(count as usize);
        let num_new = num_required as i64 - num_current as i64;

        if num_new > 0 && num_new > num_free as i64 {
            return Err(CbfsError::TableFull);
        }

        // Set the required nodes
        let mut current_count = 0;

        while !Self::is_node_end(node) {
            let mut next = self.nodes[node as usize];

            if current_count < count {
                current_count += 1;
                if current_count == count {
                    self.nodes[node as usize] = Self::NODE_END;
                } else if next == Self::NODE_END {
                    next = self.get_next_free_node()?;
                    self.nodes[node as usize] = next;
                    self.nodes[next as usize] = Self::NODE_END;
                }
            } else {
                self.nodes[node as usize] = 0;
            }

            node = next;
        }

        Ok(())
    }

    pub fn get_sectors_for_entry(&self, node: u16) -> Result<Vec<u16>, CbfsError> {
        let mut current = self.get_node_valid(node)?;
        let mut vals = Vec::new();
        while !Self::is_node_end(current) {
            vals.push(current);
            current = self.nodes[current as usize];
        }
        Ok(vals)
    }

    pub fn required_sectors_for_size(&self, size: usize) -> usize {
        let sector_size = self.header.sector_size.get() as usize;
        size.div_ceil(sector_size)
    }

    pub fn set_node_data(&mut self, node: u16, data: &[u8]) -> Result<(), CbfsError> {
        let required_sectors = self.required_sectors_for_size(data.len());
        self.set_num_sectors(node, required_sectors as u16)?;

        let file_nodes = self.get_sectors_for_entry(node)?;

        assert_eq!(file_nodes.len(), required_sectors);
        let sec_size = self.header.sector_size.get() as usize;

        for (d, idx) in data.chunks(sec_size).zip(file_nodes) {
            let data_idx = self.get_data_start_idx(idx)?;

            for (dst, src) in self.data[data_idx..(data_idx + sec_size)]
                .iter_mut()
                .zip(d.iter())
            {
                *dst = *src;
            }
        }

        Ok(())
    }

    pub fn set_node_data_header(
        &mut self,
        node: u16,
        mut header: CbEntryHeader,
        data: &[u8],
    ) -> Result<(), CbfsError> {
        header.payload_size = U32::new(data.len() as u32);
        let mut new_data = header.as_bytes().to_vec();
        new_data.extend(data);
        self.set_node_data(node, &new_data)
    }

    pub fn mkentryn(
        &mut self,
        parent: u16,
        name: &str,
        entry_type: CbEntryType,
        data: &[u8],
    ) -> Result<u16, CbfsError> {
        if self.is_node_directory(parent)? {
            for d in self.get_directory_listing(parent)? {
                if self.get_node_header(d)?.get_name() == name {
                    return Err(CbfsError::DuplicateName(name.into()));
                }
            }

            let new_node = self.get_next_free_node()?;
            self.nodes[new_node as usize] = Self::NODE_END;

            let new_hdr = CbEntryHeader {
                attributes: 0,
                entry_type: entry_type as u8,
                modification_time: U32::new(0),
                name: string_to_array(name)?,
                parent: U16::new(parent),
                payload_size: U32::new(data.len() as u32),
            };

            self.add_node_to_directory(parent, new_node)?;
            self.set_node_data_header(new_node, new_hdr, data)?;

            Ok(new_node)
        } else {
            Err(CbfsError::NodeNotDirectory)
        }
    }

    pub fn rmnode(&mut self, node: u16) -> Result<(), CbfsError> {
        if self.is_node_directory(node)? {
            for n in self.get_directory_listing(node)? {
                self.rmnode(n)?;
            }
        }

        let hdr = self.get_node_header(node)?;
        let pnode = hdr.parent.get();
        if pnode != 0 {
            self.remove_node_from_directory(pnode, node)?;
        }

        self.set_num_sectors(node, 0)?;

        Ok(())
    }

    fn remove_node_from_directory(&mut self, node: u16, target_node: u16) -> Result<(), CbfsError> {
        let (hdr, mut data) = self.get_node_data(node)?;
        assert_eq!(hdr.get_payload_size(), data.len());

        for d in data
            .chunks_mut(CbVolumeHeader::NODE_TABLE_ENTRY_SIZE)
            .map(|x| U16::mut_from_bytes(x).unwrap())
        {
            if d.get() == target_node {
                *d = U16::new(0);
            }
        }

        self.set_node_data_header(node, hdr, &data)?;
        Ok(())
    }

    fn add_node_to_directory(&mut self, node: u16, new_node: u16) -> Result<(), CbfsError> {
        let (mut hdr, mut data) = self.get_node_data(node)?;
        assert_eq!(hdr.get_payload_size(), data.len());
        let mut found = false;

        for i in (0..data.len()).step_by(CbVolumeHeader::NODE_TABLE_ENTRY_SIZE) {
            let val = U16::from_bytes([data[i], data[i + 1]]).get();
            if val == 0 {
                let node_data = U16::new(new_node).to_bytes();
                data[i] = node_data[0];
                data[i + 1] = node_data[1];
                found = true;
                break;
            }
        }

        if !found {
            hdr.set_payload_size(hdr.get_payload_size() + CbVolumeHeader::NODE_TABLE_ENTRY_SIZE);
            data.extend_from_slice(U16::new(new_node).as_bytes());
        }
        self.set_node_data_header(node, hdr, &data)?;

        Ok(())
    }

    pub fn moven(&mut self, node: u16, new_parent: u16) -> Result<(), CbfsError> {
        if !self.is_node_directory(new_parent)? {
            return Err(CbfsError::NodeNotDirectory);
        } else if node == self.header.root_sector.get() {
            return Err(CbfsError::InvalidNode(node));
        }

        let mut hdr = self.get_node_header(node)?;

        let pnode = hdr.parent.get();
        hdr.parent = new_parent.into();
        self.remove_node_from_directory(pnode, node)?;
        self.add_node_to_directory(new_parent, node)?;
        self.set_node_header(node, &hdr)?;

        Ok(())
    }

    pub fn write_to_file(&self, file: &Path) -> Result<(), CbfsError> {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file)?;

        let sect_size = self.header.sector_size.get() as u64;

        f.set_len(self.header.volume_byte_size())?;
        f.seek(SeekFrom::Start(0))?;
        f.write_all(self.header.as_bytes())?;
        f.seek(SeekFrom::Start(sect_size))?;

        for n in self.nodes.iter().copied() {
            f.write_all(&U16::new(n).to_bytes()).unwrap();
        }

        assert_eq!(self.data.len() as u64, self.header.data_sector_size());

        f.seek(SeekFrom::Start(
            sect_size * (self.header.root_sector.get() as u64),
        ))?;
        f.write_all(&self.data).unwrap();

        assert_eq!(f.stream_position()?, self.header.volume_byte_size());

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::{CbEntryType, CbFileSystem};

    #[test]
    fn test_file_nodes() {
        // Do Nothing
    }

    #[test]
    fn test_directory_create() {
        let mut fs = CbFileSystem::new("test", 1024, 512).unwrap();
        let dir_abc = fs
            .mkentryn(
                fs.header.root_sector.get(),
                "abc",
                CbEntryType::Directory,
                &[],
            )
            .unwrap();
        let dir_defg = fs
            .mkentryn(
                fs.header.root_sector.get(),
                "defg",
                CbEntryType::Directory,
                &[],
            )
            .unwrap();

        let file_a_data_in = "Hello, world!\n".bytes().collect::<Vec<_>>();

        let file_a = fs
            .mkentryn(
                fs.header.root_sector.get(),
                "a.txt",
                CbEntryType::File,
                &file_a_data_in,
            )
            .unwrap();

        let file_b_data_in = "Hello, ABC!\n".bytes().collect::<Vec<_>>();
        let file_b = fs
            .mkentryn(dir_abc, "a.txt", CbEntryType::File, &file_b_data_in)
            .unwrap();

        println!("File A = {file_a}");
        println!("File B = {file_b}");

        let (file_a_hdr, file_a_data) = fs.get_node_data(file_a).unwrap();
        assert_eq!(file_a_hdr.payload_size.get() as usize, file_a_data.len());
        assert_eq!(file_a_hdr.payload_size.get() as usize, file_a_data_in.len());
        assert_eq!(file_a_data, file_a_data_in);

        let (file_b_hdr, file_b_data) = fs.get_node_data(file_b).unwrap();
        assert_eq!(file_b_hdr.payload_size.get() as usize, file_b_data.len());
        assert_eq!(file_b_hdr.payload_size.get() as usize, file_b_data_in.len());
        assert_eq!(file_b_data, file_b_data_in);

        assert_eq!(fs.get_directory_listing(dir_defg).unwrap().len(), 0);
    }
}
