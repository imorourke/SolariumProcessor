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

use crate::{FileSystem, FileSystemError, VolumeHeader};

/// Reads a container from a given writer
pub fn read_container<T: Read>(
    f: &mut T,
) -> Result<(ContainerHeader, FileSystem), FileSystemError> {
    let mut file_header_bytes = [0u8; std::mem::size_of::<ContainerHeader>()];
    f.read_exact(&mut file_header_bytes)?;
    let file_header = match ContainerHeader::read_from_bytes(&file_header_bytes) {
        Ok(x) => x,
        Err(e) => {
            return Err(FileSystemError::ContainerError(format!(
                "unable to read container header: {e}"
            )));
        }
    };

    file_header.check_data()?;

    fn read_u16<T: Read>(f: &mut T) -> Result<u16, FileSystemError> {
        let mut tmp = U16::new(0);
        f.read_exact(tmp.as_mut_bytes())?;
        Ok(tmp.get())
    }

    fn read_inner<T: Read>(f: &mut T, sparse: bool) -> Result<FileSystem, FileSystemError> {
        const HEADER_SIZE: usize = std::mem::size_of::<VolumeHeader>();

        if sparse {
            const S_U16: usize = std::mem::size_of::<u16>();
            let header_size = read_u16(f)? as usize;

            if header_size > HEADER_SIZE {
                return Err(FileSystemError::ContainerError(format!(
                    "found header size {header_size} >= max {HEADER_SIZE}"
                )));
            }

            let mut header_data = [0u8; HEADER_SIZE];
            f.read_exact(&mut header_data[..header_size])?;

            let header = match VolumeHeader::read_from_bytes(&header_data) {
                Ok(h) => h,
                Err(e) => {
                    return Err(FileSystemError::ContainerError(format!(
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
                    return Err(FileSystemError::ContainerError(format!(
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

            FileSystem::read_bytes(&mut new_data.as_slice())
        } else {
            FileSystem::read_bytes(f)
        }
    }

    let fs = if file_header.get_options().compressed {
        #[cfg(feature = "gzip")]
        {
            let mut gf = flate2::read::GzDecoder::new(f);
            read_inner(&mut gf, file_header.get_options().sparse)?
        }
        #[cfg(not(feature = "gzip"))]
        {
            return Err(FileSystemError::ContainerError(
                "gzip feature not enabled".to_string(),
            ));
        }
    } else {
        read_inner(f, file_header.get_options().sparse)?
    };

    Ok((file_header, fs))
}

/// Writes a container to a given writer
pub fn write_container<T: Write>(
    header: &ContainerHeader,
    fs: &FileSystem,
    f: &mut T,
) -> Result<(), FileSystemError> {
    f.write_all(header.as_bytes())?;

    fn write_inner<T: Write>(
        mut f: T,
        fs: &FileSystem,
        sparse: bool,
    ) -> Result<(), FileSystemError> {
        if sparse {
            f.write_all(U16::new(std::mem::size_of::<VolumeHeader>() as u16).as_bytes())?;
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
            fs.write_bytes::<T>(&mut f)?;
        }

        Ok(())
    }

    if header.get_options().compressed {
        #[cfg(feature = "gzip")]
        {
            write_inner(
                flate2::write::GzEncoder::new(f, flate2::Compression::best()),
                fs,
                header.get_options().sparse,
            )
        }
        #[cfg(not(feature = "gzip"))]
        {
            Err(FileSystemError::ContainerError(
                "gzip feature not enabled".to_string(),
            ))
        }
    } else {
        write_inner(f, fs, header.get_options().sparse)
    }
}

/// Opens a filesystem disk image and loads into memory
pub fn open_container(path: &Path) -> Result<(ContainerHeader, FileSystem), FileSystemError> {
    let mut f = std::fs::File::open(path)?;
    read_container(&mut f)
}

/// Saves the current in-memory filesystem to the provided file
pub fn save_container(
    header: &ContainerHeader,
    filesystem: &FileSystem,
    file: &Path,
) -> Result<(), FileSystemError> {
    let mut f = OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(file)?;
    write_container(header, filesystem, &mut f)
}

#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, KnownLayout, Immutable, FromBytes, IntoBytes,
)]
pub struct CbContainerFlags(U16);

impl CbContainerFlags {
    /// Provides the flag for a sparse file
    const FLAG_SPARSE: u16 = 1 << 0;

    /// Provides the flag for a compressed file
    const FLAG_COMPRESSED: u16 = 1 << 1;
}

impl Default for CbContainerFlags {
    fn default() -> Self {
        CbContainerOptions::default().into()
    }
}

/// Provides options for handling the contianer saving/loading options
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CbContainerOptions {
    /// Determines if the container saves the disk image in sparse mode
    pub sparse: bool,
    /// Determines if the container will be saved in a compressed mode. This requires the `gzip` feature.
    pub compressed: bool,
}

impl From<CbContainerOptions> for CbContainerFlags {
    fn from(value: CbContainerOptions) -> Self {
        let flag_sparse = if value.sparse { Self::FLAG_SPARSE } else { 0 };
        let flag_compressed = if value.compressed {
            Self::FLAG_COMPRESSED
        } else {
            0
        };
        Self((flag_sparse | flag_compressed).into())
    }
}

impl From<CbContainerFlags> for CbContainerOptions {
    fn from(value: CbContainerFlags) -> Self {
        let flags = value.0.get();
        Self {
            sparse: (flags & CbContainerFlags::FLAG_SPARSE) != 0,
            compressed: (flags & CbContainerFlags::FLAG_COMPRESSED) != 0,
        }
    }
}

/// Provides the header structure for the filesystem container
#[repr(C)]
#[repr(packed)]
#[derive(Debug, Clone, Copy, FromBytes, IntoBytes, KnownLayout, Immutable, Eq, PartialEq)]
pub struct ContainerHeader {
    /// Defines a magic number to use as an indicator for the filesystem type
    magic_number: U32,
    /// Provides the version number for the filesystem container
    version: U16,
    /// Defines flags for how to read and process the encoded filesystem
    flags: CbContainerFlags,
}

impl ContainerHeader {
    /// Provides the expected magic number to read as part of the filesystem
    const MAGIC_NUMBER: u32 = 0xA80E83BC;
    /// Defines the current version of the container
    const VERSION: u16 = 1;

    /// Creates a new filesystem header with the provided container options
    pub fn new(options: CbContainerOptions) -> Self {
        Self {
            magic_number: U32::new(Self::MAGIC_NUMBER),
            version: U16::new(Self::VERSION),
            flags: options.into(),
        }
    }

    /// Contains the options for the filesystem header from the flags
    pub fn get_options(&self) -> CbContainerOptions {
        self.flags.into()
    }

    /// Sets the options for the current container
    pub fn set_options(&mut self, options: CbContainerOptions) {
        self.flags = options.into();
    }

    /// Checks the data integrity to make sure that the filesystem is valid and can be read/written
    pub fn check_data(&self) -> Result<(), FileSystemError> {
        if self.magic_number.get() != Self::MAGIC_NUMBER {
            Err(FileSystemError::UnknownError(format!(
                "unknown magic number {}",
                self.magic_number.get()
            )))
        } else if self.version.get() != Self::VERSION {
            Err(FileSystemError::UnknownError(format!(
                "unknown version number {} != expected {}",
                self.version.get(),
                Self::VERSION
            )))
        } else {
            Ok(())
        }
    }
}

impl Default for ContainerHeader {
    fn default() -> Self {
        Self {
            magic_number: U32::new(Self::MAGIC_NUMBER),
            version: U16::new(Self::VERSION),
            flags: CbContainerFlags::default(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        CbContainerOptions, ContainerHeader, EntryType, FileSystem,
        container::{read_container, write_container},
    };

    fn generate_test_filesystem() -> FileSystem {
        let mut fs = FileSystem::new("test", 1024, 512).unwrap();
        fs.randomize_sectors(true);

        let dir_abc = fs
            .create_entry(
                fs.header.root_sector.get(),
                "abc",
                EntryType::Directory,
                &[],
            )
            .unwrap();
        fs.create_entry(
            fs.header.root_sector.get(),
            "defg",
            EntryType::Directory,
            &[],
        )
        .unwrap();

        let file_a_data_in = "Hello, world!\n".bytes().collect::<Vec<_>>();

        fs.create_entry(
            fs.header.root_sector.get(),
            "a.txt",
            EntryType::File,
            &file_a_data_in,
        )
        .unwrap();

        let file_b_data_in = "Hello, ABC!\n".bytes().collect::<Vec<_>>();
        fs.create_entry(dir_abc, "a.txt", EntryType::File, &file_b_data_in)
            .unwrap();

        fs
    }

    fn generate_test_filesystem_empty() -> FileSystem {
        let mut fs = FileSystem::new("test", 1024, 1234).unwrap();
        fs.randomize_sectors(true);
        fs
    }

    #[test]
    fn container_options() {
        assert_eq!(CbContainerOptions::default().sparse, false);
        assert_eq!(CbContainerOptions::default().compressed, false);
        const INPUT_VALUES: &[CbContainerOptions] = &[
            CbContainerOptions {
                sparse: false,
                compressed: false,
            },
            CbContainerOptions {
                sparse: true,
                compressed: false,
            },
            CbContainerOptions {
                sparse: false,
                compressed: true,
            },
            CbContainerOptions {
                sparse: true,
                compressed: true,
            },
        ];

        for opt in INPUT_VALUES {
            let hdr = ContainerHeader::new(*opt);
            assert_eq!(hdr.get_options(), *opt);
        }
    }

    fn test_filesystem_val(header: ContainerHeader, filesystem: FileSystem) {
        let mut data_buf = Vec::new();
        write_container(&header, &filesystem, &mut data_buf).unwrap();

        let (header_new, fs_new) = read_container(&mut data_buf.as_slice()).unwrap();

        assert_eq!(header, header_new);
        assert!(header.check_data().is_ok());
        assert!(header_new.check_data().is_ok());

        assert_eq!(filesystem.entries, fs_new.entries);
        assert_eq!(filesystem, fs_new);

        for k in fs_new.base_entries.iter() {
            let hdr = fs_new.entry_header(*k).unwrap();
            if hdr.get_entry_type() == EntryType::Directory {
                fs_new.directory_listing(*k).unwrap();
            }
            fs_new.entry_data(*k).unwrap();
        }
    }

    fn test_filesystem(option: CbContainerOptions) {
        let hdr = ContainerHeader::new(option);
        test_filesystem_val(hdr, generate_test_filesystem());
        test_filesystem_val(hdr, generate_test_filesystem_empty());
    }

    #[test]
    fn rw_raw() {
        test_filesystem(CbContainerOptions {
            sparse: false,
            compressed: false,
        });
    }

    #[test]
    fn rw_sparse() {
        test_filesystem(CbContainerOptions {
            sparse: true,
            compressed: false,
        });
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn rw_compressed() {
        test_filesystem(CbContainerOptions {
            sparse: false,
            compressed: true,
        });
    }

    #[cfg(feature = "gzip")]
    #[test]
    fn raw_sparse_compressed() {
        test_filesystem(CbContainerOptions {
            sparse: true,
            compressed: true,
        });
    }
}
