use std::{
    fmt::Debug,
    fs::OpenOptions,
    io::{Read, Write},
    path::Path,
};

use zerocopy::{
    FromBytes, Immutable, IntoBytes, KnownLayout,
    big_endian::{U16, U32},
};

use crate::{CbError, CbFileSystem, CbVolumeHeader};

pub struct CbContainer {
    pub header: CbContainerHeader,
    pub filesystem: CbFileSystem,
}

impl CbContainer {
    pub fn new(header: CbContainerHeader, filesystem: CbFileSystem) -> Self {
        Self { header, filesystem }
    }

    /// Opens a filesystem disk image and loads into memory
    pub fn open(path: &Path) -> Result<Self, CbError> {
        let mut f = std::fs::File::open(path)?;

        let mut file_header_bytes = [0u8; std::mem::size_of::<CbContainerHeader>()];
        f.read_exact(&mut file_header_bytes)?;
        let file_header = CbContainerHeader::read_from_bytes(&file_header_bytes).unwrap();

        file_header.check_data()?;

        let mut raw_data = Vec::new();

        if file_header.is_compressed() {
            #[cfg(feature = "gzip")]
            {
                let size = match flate2::read::GzDecoder::new(f).read_to_end(&mut raw_data) {
                    Ok(s) => s,
                    Err(e) => {
                        return Err(CbError::ContainerError(format!(
                            "gzip decompresion error - {e}"
                        )));
                    }
                };
                if raw_data.len() != size {
                    return Err(CbError::ContainerError(format!(
                        "unexpected size read - {} vs {}",
                        raw_data.len(),
                        size
                    )));
                }
            }
            #[cfg(not(feature = "gzip"))]
            {
                return Err(CbError::ContainerError(
                    "gzip feature not enabled".to_string(),
                ));
            }
        } else {
            let size = f.read_to_end(&mut raw_data).unwrap();
            if raw_data.len() != size {
                return Err(CbError::ContainerError(format!(
                    "unexpected size read - {} vs {}",
                    raw_data.len(),
                    size
                )));
            }
        };

        let fs_data = if file_header.is_sparse() {
            const S_U16: usize = std::mem::size_of::<u16>();

            let header_size = U16::read_from_bytes(&raw_data[0..S_U16]).unwrap().get() as usize;
            let entry_start = header_size + S_U16;
            const HEADER_SIZE: usize = std::mem::size_of::<CbVolumeHeader>();
            let mut header_data = [0u8; HEADER_SIZE];
            raw_data[S_U16..header_size + S_U16]
                .write_to(&mut header_data[0..header_size.min(HEADER_SIZE)])
                .unwrap();

            let header = CbVolumeHeader::read_from_bytes(&header_data).unwrap(); // TODO

            let sector_size: usize = header.sector_size.get() as usize;
            let sector_count: usize = header.sector_count.get() as usize;

            let mut new_data = vec![0u8; sector_count * sector_size];
            header.write_to(&mut new_data[0..HEADER_SIZE]).unwrap(); // TODO

            let entry_end = entry_start + (sector_count * S_U16);
            let entry_data = &raw_data[entry_start..entry_end];
            entry_data
                .write_to(&mut new_data[sector_size..sector_size + entry_data.len()])
                .unwrap();

            let num_sparse = U16::from_bytes([raw_data[entry_end], raw_data[entry_end + 1]]).get();

            let mut current_index = entry_end + S_U16;
            let mut current_num = 0;
            while current_num < num_sparse {
                let current_sector =
                    U16::from_bytes([raw_data[current_index], raw_data[current_index + 1]]).get();
                let sector_data = &raw_data[current_index + S_U16
                    ..current_index + S_U16 + header.sector_size.get() as usize];

                let sector_start = current_sector as usize * sector_size;
                let sector_end = sector_start + sector_size;

                sector_data
                    .write_to(&mut new_data[sector_start..sector_end])
                    .unwrap();
                current_index += sector_size + S_U16;
                current_num += 1;
            }

            if current_num != num_sparse || current_index != raw_data.len() {
                return Err(CbError::ContainerError(format!(
                    "found {current_num} sparse, expected {num_sparse} at file end"
                )));
            }

            new_data
        } else {
            raw_data
        };

        let fs = CbFileSystem::from_bytes(&fs_data)?;

        Ok(Self {
            header: file_header,
            filesystem: fs,
        })
    }

    /// Saves the current in-memory filesystem to the provided file
    pub fn write_fs_to_file(&self, file: &Path) -> Result<(), CbError> {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file)?;

        f.write_all(self.header.as_bytes())?;

        fn write_inner<T: Write>(mut f: T, fs: &CbFileSystem, sparse: bool) -> Result<(), CbError> {
            if sparse {
                f.write_all(U16::new(std::mem::size_of::<CbVolumeHeader>() as u16).as_bytes())?;
                f.write_all(fs.header.as_bytes())?;

                for n in fs.entries.iter().copied() {
                    f.write_all(&U16::new(n).to_bytes())?;
                }

                let num_sparse_sectors = fs
                    .entries
                    .iter()
                    .skip(fs.header.root_sector.get() as usize)
                    .filter(|x| **x != 0)
                    .count() as u16;
                f.write_all(&U16::new(num_sparse_sectors).to_bytes())?;

                for (i, n) in fs.entries.iter().enumerate() {
                    if *n != 0 && i >= fs.header.root_sector.get() as usize {
                        f.write_all(&U16::new(i as u16).to_bytes())?;
                        f.write_all(fs.get_sector_data(i as u16)?)?;
                    }
                }
            } else {
                f.write_all(&fs.as_bytes()?)?;
            }

            Ok(())
        }

        if self.header.is_compressed() {
            #[cfg(feature = "gzip")]
            {
                write_inner(
                    flate2::write::GzEncoder::new(f, flate2::Compression::best()),
                    &self.filesystem,
                    self.header.is_sparse(),
                )
            }
            #[cfg(not(feature = "gzip"))]
            {
                Err(CbError::ContainerError(
                    "gzip feature not enabled".to_string(),
                ))
            }
        } else {
            write_inner(f, &self.filesystem, self.header.is_sparse())
        }
    }
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable)]
pub struct CbContainerHeader {
    magic_number: U32,
    version: U16,
    flags: U16,
}

impl CbContainerHeader {
    const MAGIC_NUMBER: u32 = 0xA80E83BC;
    const VERSION: u16 = 1;
    const FLAG_SPARSE: u16 = 1 << 0;
    const FLAG_COMPRESSED: u16 = 1 << 1;

    pub fn new(sparse: bool, compressed: bool) -> Self {
        let mut v = Self {
            magic_number: U32::new(Self::MAGIC_NUMBER),
            version: U16::new(Self::VERSION),
            flags: U16::new(0),
        };
        v.set_flag(sparse, Self::FLAG_SPARSE);
        v.set_flag(compressed, Self::FLAG_COMPRESSED);
        v
    }

    pub fn check_data(&self) -> Result<(), CbError> {
        if self.magic_number.get() != Self::MAGIC_NUMBER {
            Err(CbError::UnknownError(format!(
                "unknown magic number {}",
                self.magic_number.get()
            )))
        } else if self.version.get() != Self::VERSION {
            Err(CbError::UnknownError(format!(
                "unknown version number {} != expected {}",
                self.version.get(),
                Self::VERSION
            )))
        } else {
            Ok(())
        }
    }

    const fn get_flag(&self, flag: u16) -> bool {
        (self.flags.get() & flag) == flag
    }

    fn set_flag(&mut self, is_set: bool, flag: u16) {
        self.flags.set(if is_set {
            self.flags.get() | flag
        } else {
            self.flags.get() & !flag
        })
    }

    pub fn is_sparse(&self) -> bool {
        self.get_flag(Self::FLAG_SPARSE)
    }

    pub fn set_sparse(&mut self, val: bool) {
        self.set_flag(val, Self::FLAG_SPARSE);
    }

    pub fn is_compressed(&self) -> bool {
        self.get_flag(Self::FLAG_COMPRESSED)
    }

    pub fn set_compressed(&mut self, val: bool) {
        self.set_flag(val, Self::FLAG_COMPRESSED);
    }
}
