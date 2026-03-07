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

#[derive(Debug, Clone)]
pub struct CbContainer {
    pub header: CbContainerHeader,
    pub filesystem: CbFileSystem,
}

impl CbContainer {
    /// Creates a new container from input values
    pub fn new(header: CbContainerHeader, filesystem: CbFileSystem) -> Self {
        Self { header, filesystem }
    }

    /// Reads a container from a given writer
    pub fn read_container<T: Read>(f: &mut T) -> Result<Self, CbError> {
        let mut file_header_bytes = [0u8; std::mem::size_of::<CbContainerHeader>()];
        f.read_exact(&mut file_header_bytes)?;
        let file_header = match CbContainerHeader::read_from_bytes(&file_header_bytes) {
            Ok(x) => x,
            Err(e) => {
                return Err(CbError::ContainerError(format!(
                    "unable to read container header: {e}"
                )));
            }
        };

        file_header.check_data()?;

        fn read_u16<T: Read>(f: &mut T) -> Result<u16, CbError> {
            let mut tmp = U16::new(0);
            f.read_exact(tmp.as_mut_bytes())?;
            Ok(tmp.get())
        }

        fn read_inner<T: Read>(f: &mut T, sparse: bool) -> Result<CbFileSystem, CbError> {
            const HEADER_SIZE: usize = std::mem::size_of::<CbVolumeHeader>();

            let fs_data = if sparse {
                const S_U16: usize = std::mem::size_of::<u16>();
                let header_size = read_u16(f)? as usize;

                if header_size > HEADER_SIZE {
                    return Err(CbError::ContainerError(format!(
                        "found header size {header_size} >= max {HEADER_SIZE}"
                    )));
                }

                let mut header_data = [0u8; HEADER_SIZE];
                f.read_exact(&mut header_data[..header_size])?;

                let header = match CbVolumeHeader::read_from_bytes(&header_data) {
                    Ok(h) => h,
                    Err(e) => {
                        return Err(CbError::ContainerError(format!(
                            "unable to read sparse volume header: {e}"
                        )));
                    }
                };

                let sector_size: usize = header.sector_size.get() as usize;
                let sector_count: usize = header.sector_count.get() as usize;

                let mut new_data = vec![0u8; sector_count * sector_size];
                match header.write_to(&mut new_data[0..HEADER_SIZE]) {
                    Ok(_) => (),
                    Err(e) => {
                        return Err(CbError::ContainerError(format!(
                            "unable to write to inner header as sparse: {e}"
                        )));
                    }
                }

                f.read_exact(&mut new_data[sector_size..sector_size + S_U16 * sector_count])?;

                let num_sparse = read_u16(f)?;
                for _ in 0..num_sparse {
                    let sector = read_u16(f)? as usize;
                    let sector_base = sector * sector_size;
                    f.read_exact(&mut new_data[sector_base..sector_base + sector_size])?;
                }

                new_data
            } else {
                let mut header_data = [0u8; HEADER_SIZE];
                f.read_exact(&mut header_data)?;
                let header = match CbVolumeHeader::read_from_bytes(&header_data) {
                    Ok(h) => h,
                    Err(e) => {
                        return Err(CbError::ContainerError(format!(
                            "unable to read header: {e}"
                        )));
                    }
                };

                let mut new_data = vec![0; header.volume_byte_size() as usize];
                header_data.write_to_prefix(&mut new_data)?;

                f.read_exact(&mut new_data[HEADER_SIZE..])?;

                new_data
            };

            CbFileSystem::from_bytes(&fs_data)
        }

        let fs = if file_header.is_compressed() {
            #[cfg(feature = "gzip")]
            {
                let mut gf = flate2::read::GzDecoder::new(f);
                read_inner(&mut gf, file_header.is_sparse())?
            }
            #[cfg(not(feature = "gzip"))]
            {
                return Err(CbError::ContainerError(
                    "gzip feature not enabled".to_string(),
                ));
            }
        } else {
            read_inner(f, file_header.is_sparse())?
        };

        Ok(Self {
            header: file_header,
            filesystem: fs,
        })
    }

    /// Writes a container to a given writer
    pub fn write_container<T: Write>(&self, f: &mut T) -> Result<(), CbError> {
        f.write_all(self.header.as_bytes())?;

        fn write_inner<T: Write>(mut f: T, fs: &CbFileSystem, sparse: bool) -> Result<(), CbError> {
            if sparse {
                f.write_all(U16::new(std::mem::size_of::<CbVolumeHeader>() as u16).as_bytes())?;
                f.write_all(fs.header.as_bytes())?;

                for n in fs.entries.iter().copied() {
                    f.write_all(&U16::new(n).to_bytes())?;
                }

                assert_eq!(fs.header.sector_count.get() as usize, fs.entries.len());
                let sectors_to_write: Vec<_> = fs
                    .entries
                    .iter()
                    .enumerate()
                    .skip(fs.header.root_sector.get() as usize)
                    .filter(|(_, x)| **x != 0)
                    .map(|(i, _)| i as u16)
                    .collect();

                f.write_all(&U16::new(sectors_to_write.len() as u16).to_bytes())?;

                for s in sectors_to_write {
                    f.write_all(&U16::new(s).to_bytes())?;
                    f.write_all(fs.get_sector_data(s)?)?;
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

    /// Opens a filesystem disk image and loads into memory
    pub fn open(path: &Path) -> Result<Self, CbError> {
        let mut f = std::fs::File::open(path)?;
        Self::read_container(&mut f)
    }

    /// Saves the current in-memory filesystem to the provided file
    pub fn save(&self, file: &Path) -> Result<(), CbError> {
        let mut f = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(file)?;
        self.write_container(&mut f)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum CbContainerOptions {
    #[default]
    None,
    Sparse,
    Compressed,
    SparseCompressed,
}

impl CbContainerOptions {
    pub fn from_flags(sparse: bool, compressed: bool) -> Self {
        let flags = (sparse, compressed);
        match flags {
            (false, false) => CbContainerOptions::None,
            (true, false) => CbContainerOptions::Sparse,
            (false, true) => CbContainerOptions::Compressed,
            (true, true) => CbContainerOptions::SparseCompressed,
        }
    }

    pub fn is_sparse(&self) -> bool {
        matches!(self, Self::Sparse | Self::SparseCompressed)
    }

    pub fn is_compressed(&self) -> bool {
        matches!(self, Self::Compressed | Self::SparseCompressed)
    }
}

#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Eq, PartialEq)]
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

    pub fn new(options: CbContainerOptions) -> Self {
        let mut v = Self {
            magic_number: U32::new(Self::MAGIC_NUMBER),
            version: U16::new(Self::VERSION),
            flags: U16::new(0),
        };
        v.set_flag(options.is_sparse(), Self::FLAG_SPARSE);
        v.set_flag(options.is_compressed(), Self::FLAG_COMPRESSED);
        v
    }

    pub fn get_options(&self) -> CbContainerOptions {
        CbContainerOptions::from_flags(self.is_sparse(), self.is_compressed())
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

impl Default for CbContainerHeader {
    fn default() -> Self {
        Self {
            magic_number: U32::new(Self::MAGIC_NUMBER),
            version: U16::new(Self::VERSION),
            flags: U16::new(0),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{CbContainer, CbContainerHeader, CbContainerOptions, CbEntryType, CbFileSystem};

    fn generate_test_filesystem() -> CbFileSystem {
        let mut fs = CbFileSystem::new("test", 1024, 512).unwrap();
        fs.randomize_sectors(true);
        let dir_abc = fs
            .create_entry(
                fs.header.root_sector.get(),
                "abc",
                CbEntryType::Directory,
                &[],
            )
            .unwrap();
        fs.create_entry(
            fs.header.root_sector.get(),
            "defg",
            CbEntryType::Directory,
            &[],
        )
        .unwrap();

        let file_a_data_in = "Hello, world!\n".bytes().collect::<Vec<_>>();

        fs.create_entry(
            fs.header.root_sector.get(),
            "a.txt",
            CbEntryType::File,
            &file_a_data_in,
        )
        .unwrap();

        let file_b_data_in = "Hello, ABC!\n".bytes().collect::<Vec<_>>();
        fs.create_entry(dir_abc, "a.txt", CbEntryType::File, &file_b_data_in)
            .unwrap();

        fs
    }

    #[test]
    fn container_options() {
        const INPUT_VALUES: &[(CbContainerOptions, bool, bool)] = &[
            (CbContainerOptions::None, false, false),
            (CbContainerOptions::Sparse, true, false),
            (CbContainerOptions::Compressed, false, true),
            (CbContainerOptions::SparseCompressed, true, true),
        ];

        for (opt, sparse, compressed) in INPUT_VALUES {
            let hdr = CbContainerHeader::new(*opt);
            assert_eq!(opt.is_sparse(), *sparse);
            assert_eq!(opt.is_compressed(), *compressed);
            assert_eq!(hdr.is_sparse(), opt.is_sparse());
            assert_eq!(hdr.is_compressed(), opt.is_compressed());
            assert_eq!(hdr.get_options(), *opt);
        }
    }

    #[test]
    fn rw_raw() {
        let cont = CbContainer::new(
            CbContainerHeader::new(CbContainerOptions::None),
            generate_test_filesystem(),
        );

        let mut data_buf = Vec::new();
        cont.write_container(&mut data_buf).unwrap();

        let cont_new = CbContainer::read_container(&mut data_buf.as_slice()).unwrap();

        assert_eq!(cont.header, cont_new.header);
    }

    #[test]
    fn rw_sparse() {
        let cont = CbContainer::new(
            CbContainerHeader::new(CbContainerOptions::Sparse),
            generate_test_filesystem(),
        );

        let mut data_buf = Vec::new();
        cont.write_container(&mut data_buf).unwrap();

        let cont_new = CbContainer::read_container(&mut data_buf.as_slice()).unwrap();

        assert_eq!(cont.header, cont_new.header);
        assert!(cont.header.check_data().is_ok());
        assert!(cont_new.header.check_data().is_ok());

        let kfs = &cont_new.filesystem;

        for k in kfs.base_entries.iter() {
            let hdr = kfs.entry_header(*k).unwrap();
            if hdr.get_entry_type() == CbEntryType::Directory {
                kfs.directory_listing(*k).unwrap();
            }
            kfs.entry_data(*k).unwrap();
        }
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn rw_compressed() {
        let cont = CbContainer::new(
            CbContainerHeader::new(CbContainerOptions::Compressed),
            generate_test_filesystem(),
        );

        let mut data_buf = Vec::new();
        cont.write_container(&mut data_buf).unwrap();

        let cont_new = CbContainer::read_container(&mut data_buf.as_slice()).unwrap();

        assert_eq!(cont.header, cont_new.header);
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn raw_sparse_compressed() {
        let cont = CbContainer::new(
            CbContainerHeader::new(CbContainerOptions::SparseCompressed),
            generate_test_filesystem(),
        );

        let mut data_buf = Vec::new();
        cont.write_container(&mut data_buf).unwrap();

        let cont_new = CbContainer::read_container(&mut data_buf.as_slice()).unwrap();

        assert_eq!(cont.header, cont_new.header);
        assert_eq!(cont.filesystem.entries, cont_new.filesystem.entries);
        assert_eq!(cont.filesystem, cont_new.filesystem);
    }
}
