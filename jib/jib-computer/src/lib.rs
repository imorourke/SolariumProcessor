use cbfs_lib::{EntryType, FileSystem, FileSystemError, VolumeHeader};
use cblang::{
    CodeGenerationOptions, CompilerError, CompilingState, PreprocessorError, ProgramType,
    TokenError, VirtualFilesystem,
};
use circ_buff::CircularBuffer;
use core::{cell::RefCell, fmt::Display};
use jib_asm::{AssemblerError, AssemblerErrorLoc, AssemblerOutput};
#[cfg(not(target_arch = "wasm32"))]
use jib_cpu::device::RtcTimerDevice;
use jib_cpu::{
    cpu::{Instruction, Processor, ProcessorError, RegisterManager, ResetType},
    device::{
        BlankDevice, BlockDevice, DEVICE_MEM_SIZE, InterruptClockDevice, ProcessorDevice,
        RtcClockDevice, SerialInputOutputDevice,
    },
    memory::{MemorySegment, ReadOnlySegment, ReadWriteSegment},
    text::{CharacterError, byte_to_character, character_to_byte},
};
use std::{
    format,
    path::{Component, Path},
    rc::Rc,
    vec::Vec,
};

pub struct JibComputer {
    running: bool,
    running_requested: bool,
    bootloader: bool,
    cpu: Processor,
    dev_serial_io: Rc<RefCell<SerialInputOutputDevice>>,
    #[cfg(not(target_arch = "wasm32"))]
    dev_rtc_timer: Rc<RefCell<RtcTimerDevice>>,
    inst_history: CircularBuffer<(u32, Instruction), 10>,
    hard_drive: Rc<RefCell<BlockDevice>>,
    #[cfg(test)]
    step_count: u128,
}

impl JibComputer {
    const INIT_MEMORY_SIZE: u32 = 0x40000000;
    pub const BOOTLOADER_START: u32 = 0xFFFF0000;
    const DEVICE_START_ADDR: u32 = 0xFFFFA000;
    const DEVICE_HD_START_ADDR: u32 = 0xFFFFB000;
    const DEVICE_COUNT: usize =
        ((Self::DEVICE_HD_START_ADDR - Self::DEVICE_START_ADDR) / DEVICE_MEM_SIZE) as usize;
    pub const THREAD_LOOP_MS: u64 = 50;

    pub fn new() -> Result<Self, ComputerError> {
        let mut s = Self {
            running: false,
            running_requested: false,
            bootloader: false,
            cpu: Processor::default(),
            dev_serial_io: Rc::new(RefCell::new(SerialInputOutputDevice::new(2048))),
            #[cfg(not(target_arch = "wasm32"))]
            dev_rtc_timer: Rc::new(RefCell::new(RtcTimerDevice::default())),
            inst_history: Default::default(),
            hard_drive: Self::create_block_device(&Self::create_hard_drive()?)?,
            #[cfg(test)]
            step_count: 0,
        };

        s.reset(None)?;
        Ok(s)
    }

    fn create_hard_drive() -> Result<FileSystem, ComputerError> {
        // Read the kernel/app files
        static CODE_APP: &str = include_str!("../../../cbos/app.cb");
        static CODE_OS: &str = include_str!("../../../cbos/os.cb");

        // Compile OS into a file
        let kernel_compiled = Self::compile_kernel_code(CODE_OS, None, false)?;
        let kernel_data = kernel_compiled.get_assembler()?.bytes;

        // Obtain the default interface value
        let mut interface_data = Vec::new();
        {
            const KNOWN_PREFIXES: &[&str] = &["k_", "K_", "std_", "irq_", "mem_"];

            let mut writer = std::io::BufWriter::new(&mut interface_data);
            kernel_compiled
                .get_exported_interface()?
                .filter(|name| KNOWN_PREFIXES.iter().any(|prefix| name.starts_with(prefix)))
                .write_interface(&mut writer)?;
        }

        const CBOS_INTF_GUARD: &str = "CBOS_DEFS";
        let interface_str = match String::from_utf8(interface_data) {
            Ok(x) => format!("#ifndef {CBOS_INTF_GUARD}\n#define {CBOS_INTF_GUARD}\n\n{x}\n#endif // {CBOS_INTF_GUARD}"),
            Err(_) => return Err(ComputerError::Utf8Error),
        };

        // Compile an example program
        let app_data = Self::compile_app_code(CODE_APP, &interface_str)?;

        let mut fs = FileSystem::new("cbos", 256, 4096)?;
        fs.create_entry(fs.root_sector(), "boot.bin", EntryType::File, &kernel_data)?;
        let home_dir = fs.create_entry(fs.root_sector(), "home", EntryType::Directory, &[])?;

        let build_date: &'static str = env!("BUILD_DATE");
        fs.create_entry(
            home_dir,
            "version",
            EntryType::File,
            format!("CB/OS\nBuild Date\n{}\n", build_date).as_bytes(),
        )?;
        fs.create_entry(
            home_dir,
            "hello.exe",
            EntryType::File,
            &app_data.get_assembler()?.bytes,
        )?;
        fs.create_entry(
            home_dir,
            "script.run",
            EntryType::File,
            b"date\nmem\n\npwd\ncat hello.txt\ncat hello.txt",
        )?;

        let src = fs.create_entry(fs.root_sector(), "src", EntryType::Directory, &[])?;

        fs.create_entry(src, "os.cb", EntryType::File, CODE_OS.as_bytes())?;
        fs.create_entry(src, "cbos_defs.cb", EntryType::File, interface_str.as_bytes())?;
        fs.create_entry(src, "app_hello.cb", EntryType::File, CODE_APP.as_bytes())?;

        for (path, code) in cblang::DEFAULT_FILES.iter() {
            let mut current_dir = src;
            let path_val = Path::new(path);

            for p in path_val.parent().unwrap().components() {
                if let Component::Normal(os_name) = &p
                    && let Some(name) = os_name.to_str()
                {
                    current_dir = if let Some(existing) = fs
                        .directory_listing(current_dir)?
                        .iter()
                        .find(|x| x.get_name() == name)
                    {
                        existing.base_block.get()
                    } else {
                        fs.create_entry(current_dir, name, EntryType::Directory, &[])?
                    };
                } else {
                    panic!("unsupported name value");
                }
            }

            if let Some(name) = path_val.file_name().and_then(|x| x.to_str())
                && name.len() < VolumeHeader::VOLUME_NAME_SIZE
            {
                fs.create_entry(current_dir, name, EntryType::File, code.as_bytes())?;
            }
        }

        Ok(fs)
    }

    fn create_block_device(fs: &FileSystem) -> Result<Rc<RefCell<BlockDevice>>, ComputerError> {
        Ok(Rc::new(RefCell::new(BlockDevice::new(fs.get_fs_bytes()?))))
    }

    pub fn get_inst_history(&self) -> Vec<(u32, Instruction)> {
        self.inst_history.elts()
    }

    pub fn get_register_state(&self) -> RegisterManager {
        self.cpu.get_register_state()
    }

    pub fn memory_inspect_u8(&self, addr: u32) -> Result<u8, ComputerError> {
        Ok(self.cpu.memory_inspect(addr)?)
    }

    pub fn memory_inspect_u32(&self, addr: u32) -> Result<u32, ComputerError> {
        Ok(self.cpu.memory_inspect_u32(addr)?)
    }

    pub fn step_cpu(
        &mut self,
        breakpoint: Option<u32>,
        auto_break: bool,
    ) -> Result<bool, ComputerError> {
        let pc = self.cpu.get_current_pc().unwrap_or(0);
        if let Ok(inst) = self.cpu.get_current_inst() {
            self.inst_history.push((pc, inst));
        }

        let (debug_stop, cancel_run_request) =
            if auto_break && let Ok(op) = self.cpu.get_current_op() {
                (
                    op == Processor::OP_DEBUG_BREAK || op == Processor::OP_HALT,
                    op == Processor::OP_DEBUG_BREAK,
                )
            } else if let Some(brk) = breakpoint {
                (brk == pc, true)
            } else {
                (false, true)
            };

        if debug_stop {
            self.running = false;
            self.running_requested = self.running_requested && !cancel_run_request;
            return Ok(false);
        }

        self.cpu.step()?;

        #[cfg(test)]
        {
            self.step_count += 1;
        }

        Ok(true)
    }

    pub fn compile_app_code(
        code: &str,
        os_interface: &str,
    ) -> Result<CompilingState, ComputerError> {
        let mut fs = VirtualFilesystem::default();
        fs.add_file(Path::new("main.cb"), code)?;
        fs.add_file(Path::new("cbos_defs.cb"), os_interface)?;

        let preprocessed =
            cblang::preprocess_code_with_fs(Path::new("main.cb"), fs, [].into_iter())?;

        let tokens = preprocessed.tokenize()?;

        let options = CodeGenerationOptions {
            prog_type: ProgramType::Application,
            trim_code: true,
            ..Default::default()
        };

        Ok(cblang::compile(tokens, options)?)
    }

    pub fn compile_kernel_code(
        code: &str,
        start_offset: Option<u32>,
        trim_code: bool,
    ) -> Result<CompilingState, ComputerError> {
        let preprocessed =
            cblang::preprocess_code_as_file(code, Path::new("input.cb"), [].into_iter())?;

        let tokens = preprocessed.tokenize()?;

        let options = CodeGenerationOptions {
            prog_type: ProgramType::Kernel {
                stack_loc_init: Some(ProgramType::DEFAULT_STACK_LOC),
                base_location: start_offset.unwrap_or(ProgramType::DEFAULT_START_OFFSET),
            },
            trim_code,
            ..Default::default()
        };

        Ok(cblang::compile(tokens, options)?)
    }

    pub fn soft_reset(&mut self) -> Result<(), ComputerError> {
        self.cpu.reset(ResetType::Soft)?;
        Ok(())
    }

    pub fn reset(&mut self, input_code: Option<(u32, &[u8])>) -> Result<(), ComputerError> {
        const INIT_RO_LEN: u32 = Processor::BASE_HW_INT_ADDR;

        self.cpu = Processor::default();
        self.dev_serial_io.borrow_mut().reset();
        self.hard_drive = Self::create_block_device(&Self::create_hard_drive()?)?;

        self.inst_history.clear();

        #[cfg(test)]
        {
            self.step_count = 0;
        }

        let mut reset_vec_data: Vec<u8> = vec![0; INIT_RO_LEN as usize];
        let start_loc = if self.bootloader {
            Self::BOOTLOADER_START
        } else {
            ProgramType::DEFAULT_START_OFFSET
        };

        for (i, x) in start_loc.to_be_bytes().iter().enumerate() {
            reset_vec_data[i] = *x;
            reset_vec_data[i + std::mem::size_of::<u32>()] = *x;
        }

        assert!(reset_vec_data.len() == INIT_RO_LEN as usize);

        self.cpu.memory_add_segment(
            0,
            Rc::new(RefCell::new(ReadOnlySegment::new(reset_vec_data))),
        )?;

        self.cpu.memory_add_segment(
            INIT_RO_LEN,
            Rc::new(RefCell::new(ReadWriteSegment::new(
                (Self::INIT_MEMORY_SIZE - INIT_RO_LEN) as usize,
            ))),
        )?;

        self.cpu.memory_add_segment(
            Self::BOOTLOADER_START,
            Rc::new(RefCell::new(ReadWriteSegment::new(
                (Self::DEVICE_START_ADDR - Self::BOOTLOADER_START) as usize,
            ))),
        )?;

        let blank_dev = Rc::new(RefCell::new(BlankDevice));
        let devices: [Rc<RefCell<dyn ProcessorDevice>>; _] = [
            self.dev_serial_io.clone(),
            Rc::new(RefCell::new(InterruptClockDevice::default())),
            Rc::new(RefCell::new(RtcClockDevice)),
            #[cfg(not(target_arch = "wasm32"))]
            self.dev_rtc_timer.clone(),
        ];

        for i in 0..Self::DEVICE_COUNT {
            let dev_loc = Self::DEVICE_START_ADDR + (i as u32) * DEVICE_MEM_SIZE;

            let dev = if let Some(d) = devices.get(i) {
                self.cpu.device_add(d.clone())?;
                d.clone()
            } else {
                blank_dev.clone()
            };

            self.cpu.memory_add_segment(dev_loc, dev)?;
        }

        self.cpu.device_add(self.hard_drive.clone())?;
        self.cpu
            .memory_add_segment(Self::DEVICE_HD_START_ADDR, self.hard_drive.clone())?;

        self.cpu.reset(ResetType::Hard)?;

        // Compile and setup bootloader
        for (i, x) in Self::compile_kernel_code(
            include_str!("../../../cbos/bootloader.cb"),
            Some(Self::BOOTLOADER_START),
            true,
        )?
        .get_assembler()?
        .bytes
        .iter()
        .enumerate()
        {
            self.cpu.memory_set(Self::BOOTLOADER_START + i as u32, *x)?;
        }

        if !self.bootloader
            && let Some((start, code)) = input_code
        {
            self.cpu.memory_set_range(start, code)?;
        }

        if self.running_requested {
            self.running = true;
        }

        Ok(())
    }

    pub fn step_devices(&mut self) -> Result<bool, ComputerError> {
        Ok(if self.cpu.step_devices()? {
            self.running = true;
            true
        } else {
            false
        })
    }

    pub fn use_bootloader(&mut self, value: bool) -> Result<(), ComputerError> {
        self.bootloader = value;
        Ok(())
    }

    pub fn using_bootloader(&self) -> bool {
        self.bootloader
    }

    pub fn trigger_irq(&mut self, irq: u32) -> Result<bool, ComputerError> {
        if self.cpu.trigger_hardware_interrupt(irq)? {
            if self.running_requested && !self.running && self.cpu.step_devices()? {
                self.running = true;
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    pub fn set_running_request(&mut self, running_requested: bool) -> bool {
        if running_requested != self.running_requested {
            self.running = running_requested;
            self.running_requested = running_requested;
            true
        } else {
            false
        }
    }

    pub fn get_running(&self) -> bool {
        self.running
    }

    pub fn get_running_requested(&self) -> bool {
        self.running_requested
    }

    pub fn set_code(&mut self, asm: AssemblerOutput) -> Result<(), ComputerError> {
        self.reset(Some((asm.start_address, &asm.bytes)))
    }

    pub fn set_serial_input(&mut self, s: &str) -> Result<bool, ComputerError> {
        for c in s.chars().chain(['\n']) {
            let cv = character_to_byte(c)?;
            if !self.dev_serial_io.borrow_mut().push_input(cv) {
                return Ok(false);
            } else if self.running_requested && !self.running && self.cpu.step_devices()? {
                self.running = true;
            }
        }
        Ok(true)
    }

    pub fn get_serial_output(&mut self) -> Result<Vec<char>, ComputerError> {
        let mut char_vec = Vec::new();
        while let Some(w) = self.dev_serial_io.borrow_mut().pop_output() {
            let c = byte_to_character(w)?;
            char_vec.push(c);
        }
        Ok(char_vec)
    }

    pub fn get_serial_output_unknown(&mut self) -> Vec<char> {
        let mut char_vec = Vec::new();
        while let Some(w) = self.dev_serial_io.borrow_mut().pop_output() {
            char_vec.push(byte_to_character(w).unwrap_or('?'));
        }
        char_vec
    }

    pub fn get_disk_data(&self) -> Result<Vec<u8>, ComputerError> {
        Ok(self.hard_drive.borrow().data.clone())
    }
}

#[derive(Debug, Clone)]
pub enum ComputerError {
    ProcessorError(ProcessorError),
    AssemblerError(AssemblerError),
    AssemblerErrorLoc(AssemblerErrorLoc),
    DiskError(FileSystemError),
    TokenError(TokenError),
    PreprocessorError(PreprocessorError),
    CharacterError(CharacterError),
    FilesystemError(cblang::FilesystemError),
    Utf8Error,
    IoError,
}

impl Display for ComputerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PreprocessorError(e) => write!(f, "preprocessor => {e}"),
            Self::AssemblerError(e) => write!(f, "assembler => {e}"),
            Self::AssemblerErrorLoc(e) => write!(f, "assembler => {e}"),
            Self::DiskError(e) => write!(f, "disk => {e}"),
            Self::TokenError(e) => write!(f, "token => {e}"),
            Self::ProcessorError(e) => write!(f, "processor => {e}"),
            Self::CharacterError(e) => write!(f, "character => {e}"),
            Self::FilesystemError(e) => write!(f, "filesystem => {e}"),
            Self::Utf8Error => write!(f, "utf8 error"),
            Self::IoError => write!(f, "io error"),
        }
    }
}

impl From<CompilerError> for ComputerError {
    fn from(value: CompilerError) -> Self {
        match value {
            CompilerError::AssemblerError(v) => Self::AssemblerErrorLoc(v),
            CompilerError::TokenError(v) => Self::TokenError(v),
        }
    }
}

impl From<ProcessorError> for ComputerError {
    fn from(value: ProcessorError) -> Self {
        Self::ProcessorError(value)
    }
}

impl From<AssemblerError> for ComputerError {
    fn from(value: AssemblerError) -> Self {
        Self::AssemblerError(value)
    }
}

impl From<AssemblerErrorLoc> for ComputerError {
    fn from(value: AssemblerErrorLoc) -> Self {
        Self::AssemblerErrorLoc(value)
    }
}

impl From<TokenError> for ComputerError {
    fn from(value: TokenError) -> Self {
        Self::TokenError(value)
    }
}

impl From<PreprocessorError> for ComputerError {
    fn from(value: PreprocessorError) -> Self {
        Self::PreprocessorError(value)
    }
}

impl From<FileSystemError> for ComputerError {
    fn from(value: FileSystemError) -> Self {
        Self::DiskError(value)
    }
}

impl From<CharacterError> for ComputerError {
    fn from(value: CharacterError) -> Self {
        Self::CharacterError(value)
    }
}

impl From<cblang::FilesystemError> for ComputerError {
    fn from(value: cblang::FilesystemError) -> Self {
        Self::FilesystemError(value)
    }
}

impl From<std::io::Error> for ComputerError {
    fn from(_: std::io::Error) -> Self {
        Self::IoError
    }
}

#[cfg(test)]
mod test {
    use super::JibComputer;
    use jib_cpu::cpu::Processor;

    fn run_cpu_serial_out_test(in_code: &str, expected_out: &str) {
        let asm = JibComputer::compile_kernel_code(in_code, None, true)
            .unwrap()
            .get_assembler()
            .unwrap();

        let mut cpu = JibComputer::new().unwrap();
        cpu.set_code(asm).unwrap();

        let mut serial_output = Vec::new();
        let mut iter_count = 0;

        while cpu.cpu.get_current_op().unwrap() != Processor::OP_HALT {
            cpu.step_cpu(None, false).unwrap();
            while let Some(c) = cpu.dev_serial_io.borrow_mut().pop_output() {
                serial_output.push(c);
            }
            iter_count += 1;
            assert!(iter_count < 40000);
        }

        println!("Step Count: {}", cpu.step_count);
        println!("{}", str::from_utf8(&serial_output).unwrap());

        assert_eq!(str::from_utf8(&serial_output).unwrap(), expected_out);
    }

    #[test]
    fn test_malloc() {
        const EXPECTED: &str = "A\n\
                @16777216, 10\n\
                @16777238, 12\n\
                @16777262, 30\n\
                @16777304, 45\n\
                B\n\
                @16777238, 12\n\
                @16777262, 30\n\
                @16777304, 45\n\
                C\n\
                @16777216, 5\n\
                @16777238, 12\n\
                @16777262, 30\n\
                @16777304, 45\n\
                D\n\
                No Heap Allocations\n\
                E\n\
                @16777216, 33\n\
                F\n\
                No Heap Allocations\n\
                Heap Test Pass\n";

        run_cpu_serial_out_test(
            include_str!("../../../cbuoy/cblang/tests/test_kmalloc.cb"),
            EXPECTED,
        );
    }

    #[test]
    fn test_struct_ptr() {
        static EXPECTED: &str = "Hello, world!\n\
            13\n\
            720\n\
            13\n\
            13\n\
            13\n\
            13\n\
            0\n\
            1\n\
            1234\n\
            Hello, world!\n\
            7\n\
            7\n";
        run_cpu_serial_out_test(
            include_str!("../../../cbuoy/cblang/tests/test_struct_ptr.cb"),
            EXPECTED,
        );
    }
}
