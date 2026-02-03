use crate::messages::{ThreadToUi, UiToThread};
use jib::{
    cpu::{Processor, ProcessorError},
    device::{
        BlankDevice, BlockDevice, DEVICE_MEM_SIZE, InterruptClockDevice, ProcessorDevice,
        RtcClockDevice, SerialInputOutputDevice,
    },
    memory::{MemorySegment, ReadOnlySegment, ReadWriteSegment},
};
use jib_asm::InstructionList;
use std::{
    cell::RefCell,
    rc::Rc,
    sync::mpsc::{Receiver, RecvError, Sender, TryRecvError},
};

struct CircularBuffer<T, const S: usize> {
    history: [Option<T>; S],
    index: usize,
    last: Option<T>,
}

impl<T: Clone + PartialEq + Eq, const S: usize> CircularBuffer<T, S> {
    pub fn reset(&mut self) {
        self.history.fill(None);
        self.index = 0;
        self.last = None;
    }

    pub fn push(&mut self, val: T) {
        let new_last = Some(val);
        if new_last != self.last {
            self.history[self.index] = new_last.clone();
            self.index = (self.index + 1) % self.history.len();
            self.last = new_last;
        }
    }

    pub fn list(&self) -> Vec<T> {
        let mut vals = Vec::new();

        for i in 0..self.history.len() {
            let iv = (self.index + i) % self.history.len();

            if let Some(t) = self.history[iv].as_ref() {
                vals.push(t.clone());
            }
        }

        vals
    }
}

impl<T: Clone + PartialEq + Eq, const S: usize> Default for CircularBuffer<T, S> {
    fn default() -> Self {
        const {
            assert!(S > 0);
        }

        Self {
            history: [const { None }; S],
            index: 0,
            last: None,
        }
    }
}

pub struct CpuState {
    running: bool,
    running_requested: bool,
    running_prev: bool,
    multiplier: f64,
    #[cfg(not(target_arch = "wasm32"))]
    run_thread: bool,
    cpu: Processor,
    dev_serial_io: Rc<RefCell<SerialInputOutputDevice>>,
    last_code: Vec<u8>,
    inst_history: CircularBuffer<String, 10>,
    inst_map: InstructionList,
    breakpoint: Option<u32>,
    step_count: u128,
    rx: Receiver<UiToThread>,
    tx: Sender<ThreadToUi>,
    hard_drive: Rc<RefCell<BlockDevice>>,
}

impl CpuState {
    const INIT_MEMORY_SIZE: u32 = 0x40000000;
    const DEVICE_START_ADDR: u32 = 0xFFFFA000;
    const DEVICE_HD_START_ADDR: u32 = 0xFFFFB000;
    const DEVICE_COUNT: usize =
        ((Self::DEVICE_HD_START_ADDR - Self::DEVICE_START_ADDR) / DEVICE_MEM_SIZE) as usize;
    pub const THREAD_LOOP_MS: u64 = 50;
    const MSGS_PER_LOOP: u64 = 1000;

    pub fn new(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) -> Result<Self, ProcessorError> {
        let mut s = Self {
            #[cfg(not(target_arch = "wasm32"))]
            run_thread: true,
            running: false,
            running_requested: false,
            running_prev: false,
            multiplier: 1.0,
            cpu: Processor::default(),
            dev_serial_io: Rc::new(RefCell::new(SerialInputOutputDevice::new(2048))),
            last_code: Vec::new(),
            inst_history: Default::default(),
            inst_map: InstructionList::default(),
            breakpoint: None,
            step_count: 0,
            rx,
            tx,
            hard_drive: Self::create_hard_drive(),
        };

        s.reset()?;
        Ok(s)
    }

    fn create_hard_drive() -> Rc<RefCell<BlockDevice>> {
        let mut fs = cbfs_lib::CbFileSystem::new("root", 256, 4096).unwrap();
        fs.create_entry(
            fs.header.root_sector.get(),
            "hello.txt",
            cbfs_lib::CbEntryType::File,
            b"Hello, world!",
        )
        .unwrap();
        let root_dir = fs
            .create_entry(
                fs.header.root_sector.get(),
                "root",
                cbfs_lib::CbEntryType::Directory,
                &[],
            )
            .unwrap();
        fs.create_entry(root_dir, "version", cbfs_lib::CbEntryType::File, b"CB/OS")
            .unwrap();
        let src = fs
            .create_entry(root_dir, "src", cbfs_lib::CbEntryType::Directory, &[])
            .unwrap();

        for (path, code) in cbuoy::DEFAULT_FILES.iter().cloned() {
            let name = path.split('/').last().unwrap();
            if name.len() < cbfs_lib::CbDirectoryEntry::NAME_SIZE {
                fs.create_entry(src, name, cbfs_lib::CbEntryType::File, code.as_bytes())
                    .unwrap();
            }
        }

        Rc::new(RefCell::new(BlockDevice::new(fs.as_bytes().unwrap())))
    }

    fn step_cpu(&mut self, enable_breakpoints: bool) -> Result<(), ThreadToUi> {
        let mut inst_details = "??".to_string();

        let pc = self.cpu.get_current_pc().unwrap_or(0);
        if let Ok(inst) = self.cpu.get_current_inst()
            && let Some(disp_val) = self.inst_map.get_display_inst(inst)
        {
            inst_details = disp_val;
        }

        inst_details = format!("0x{pc:08x} = {inst_details}");
        self.inst_history.push(inst_details.clone());

        if let Some(brk) = self.breakpoint
            && brk == pc
            && enable_breakpoints
        {
            self.running = false;

            let msg = format!(
                "Breaking at 0x{brk:08x}\n{}",
                self.inst_history
                    .list()
                    .into_iter()
                    .map(|s| format!("    {s}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
            return Err(ThreadToUi::LogMessage(msg));
        }

        let debug_stop = if let Ok(op) = self.cpu.get_current_op() {
            op == Processor::OP_DEBUG_BREAK || op == Processor::OP_HALT
        } else {
            false
        };

        self.step_count += 1;
        let res = self.cpu.step();

        if let Err(e) = res {
            let msg = format!(
                "{}\n{}",
                e,
                self.inst_history
                    .list()
                    .into_iter()
                    .map(|s| format!("    {s}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            );

            Err(ThreadToUi::LogMessage(msg))
        } else {
            if debug_stop {
                self.running = false;
            }

            Ok(())
        }
    }

    fn reset(&mut self) -> Result<(), ProcessorError> {
        const INIT_RO_LEN: u32 = Processor::BASE_HW_INT_ADDR;

        self.cpu = Processor::default();
        self.dev_serial_io.borrow_mut().reset();

        self.inst_history.reset();
        self.step_count = 0;

        let reset_vec_data: Vec<u8> = (0..INIT_RO_LEN)
            .map(|i| {
                let is = i as usize;
                if is < self.last_code.len() {
                    self.last_code[is]
                } else {
                    0
                }
            })
            .collect();
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

        let blank_dev = Rc::new(RefCell::new(BlankDevice));
        let devices: [Rc<RefCell<dyn ProcessorDevice>>; _] = [
            self.dev_serial_io.clone(),
            Rc::new(RefCell::new(InterruptClockDevice::default())),
            Rc::new(RefCell::new(RtcClockDevice)),
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

        self.cpu.reset(jib::cpu::ResetType::Hard)?;

        if self.last_code.len() > INIT_RO_LEN as usize {
            self.cpu
                .memory_set_range(INIT_RO_LEN, &self.last_code[INIT_RO_LEN as usize..])?;
        }
        Ok(())
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        fn inner_handler(
            state: &mut CpuState,
            msg: UiToThread,
        ) -> Result<Option<ThreadToUi>, ProcessorError> {
            match msg {
                UiToThread::CpuStep => {
                    if let Err(e) = state.step_cpu(false) {
                        return Ok(Some(e));
                    }
                }
                UiToThread::CpuRun(set) => {
                    state.running = set;
                    state.running_requested = set;
                }
                #[cfg(not(target_arch = "wasm32"))]
                UiToThread::Exit => state.run_thread = false,
                UiToThread::CpuReset => {
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::CpuIrq(irq) => {
                    if !state.cpu.trigger_hardware_interrupt(irq as u32)? {
                        return Ok(Some(ThreadToUi::LogMessage(format!(
                            "irq {irq} not triggered"
                        ))));
                    }

                    if state.running_requested && !state.running && state.cpu.step_devices()? {
                        state.running = true;
                    }
                }
                UiToThread::SetMultiplier(m) => {
                    state.multiplier = m;
                }
                UiToThread::SetCode(data) => {
                    let code = if data.start_address != 0 {
                        let mut asm_vals = vec![0; data.start_address as usize];
                        asm_vals.extend(data.bytes);
                        asm_vals
                    } else {
                        data.bytes
                    };

                    state.running = false;
                    state.last_code = code;
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::SerialInput(s) => {
                    for c in s.chars().chain(['\n']) {
                        match jib::text::character_to_byte(c) {
                            Ok(word) => {
                                if !state.dev_serial_io.borrow_mut().push_input(word) {
                                    return Ok(Some(ThreadToUi::LogMessage(
                                        "device serial input buffer full".to_string(),
                                    )));
                                } else if state.running_requested
                                    && !state.running
                                    && state.cpu.step_devices()?
                                {
                                    state.running = true;
                                }
                            }
                            Err(e) => return Ok(Some(ThreadToUi::LogMessage(e.to_string()))),
                        }
                    }
                }
                UiToThread::RequestMemory(base, size) => {
                    // Send memory if needed
                    let mut resp_memory = Vec::new();
                    for i in 0..size {
                        resp_memory.push(state.cpu.memory_inspect(base + i).unwrap_or_default());
                    }
                    state
                        .tx
                        .send(ThreadToUi::ResponseMemory(base, resp_memory))
                        .unwrap();
                }
                UiToThread::DiskReset => {
                    state.hard_drive = CpuState::create_hard_drive();
                    state.reset().unwrap();
                }
                UiToThread::DiskSave => {
                    let fs =
                        cbfs_lib::CbFileSystem::read_from_bytes(&state.hard_drive.borrow().data)
                            .unwrap();
                    fs.write_fs_to_file(std::path::Path::new("hd.cbfs"), false, false)
                        .unwrap();
                }
            }

            Ok(None)
        }

        match inner_handler(self, msg) {
            Ok(resp) => resp,
            Err(e) => Some(ThreadToUi::LogMessage(format!("error: {e}"))),
        }
    }

    pub fn process_messages(&mut self, blocking: bool) -> Result<(), ()> {
        if self.running {
            for _ in 0..Self::MSGS_PER_LOOP {
                let resp = match self.rx.try_recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(TryRecvError::Disconnected) => return Err(()),
                    Err(TryRecvError::Empty) => break,
                };

                if let Some(r) = &resp {
                    self.tx
                        .send(r.clone())
                        .expect("Unable to send response to main thread!");
                }
            }
        } else {
            let resp = if blocking {
                match self.rx.recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(RecvError) => return Err(()),
                }
            } else {
                match self.rx.try_recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(TryRecvError::Empty) => return Ok(()),
                    Err(TryRecvError::Disconnected) => return Err(()),
                }
            };

            if let Some(r) = &resp {
                self.tx
                    .send(r.clone())
                    .expect("Unable to send response to main thread!");
            }
        }

        // Step if required
        if self.running {
            let step_repeat_count = self.multiplier as i64;

            for _ in 0..step_repeat_count {
                if let Err(msg) = self.step_cpu(true) {
                    self.running = false;
                    self.tx.send(msg).unwrap();
                    break;
                }

                if !self.running {
                    break;
                }
            }
        }

        // Send Registers
        self.tx
            .send(ThreadToUi::RegisterState(Box::new(
                self.cpu.get_register_state(),
            )))
            .unwrap();

        let pc = self
            .cpu
            .get_register_state()
            .get(jib::cpu::Register::ProgramCounter)
            .unwrap_or(0);
        let mem = self.cpu.memory_inspect_u32(pc).unwrap_or(0);

        self.tx
            .send(ThreadToUi::ProgramCounterValue(pc, mem))
            .unwrap();

        // Send CPU state
        if self.running_prev != self.running {
            self.running_prev = self.running;
            self.tx.send(ThreadToUi::CpuRunning(self.running)).unwrap();
        }

        // Check for serial output
        let mut char_vec = Vec::new();
        while let Some(w) = self.dev_serial_io.borrow_mut().pop_output() {
            let c = match jib::text::byte_to_character(w) {
                Ok(v) => v,
                Err(e) => {
                    self.tx
                        .send(ThreadToUi::LogMessage(format!("{e}")))
                        .unwrap();
                    '?'
                }
            };

            char_vec.push(c);
        }

        if !char_vec.is_empty() {
            self.tx
                .send(ThreadToUi::SerialOutput(
                    char_vec.into_iter().collect::<String>(),
                ))
                .unwrap();
        }

        Ok(())
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = match CpuState::new(rx, tx.clone()) {
        Ok(cpu) => cpu,
        Err(e) => {
            eprintln!("Error with this: {:?}", e);
            tx.send(ThreadToUi::ThreadExit).unwrap();
            return;
        }
    };

    while state.run_thread {
        if state.process_messages(true).is_err() {
            break;
        }

        if state.running {
            std::thread::sleep(std::time::Duration::from_millis(CpuState::THREAD_LOOP_MS));
        }
    }

    tx.send(ThreadToUi::ThreadExit).unwrap();
}

#[cfg(test)]
mod test {
    use crate::cpu_thread::CpuState;
    use cbuoy::CodeGenerationOptions;
    use jib::cpu::Processor;
    use std::path::Path;

    fn run_cpu_serial_out_test(in_code: &str, expected_out: &str) {
        let tokens = cbuoy::preprocess_code_as_file(in_code, Path::new("test.cb"), [].into_iter())
            .unwrap()
            .tokenize()
            .unwrap();

        let tokens = cbuoy::parse(
            tokens,
            CodeGenerationOptions {
                debug_locations: true,
                ..Default::default()
            },
        )
        .and_then(|x| x.get_assembler())
        .unwrap();
        let asm = jib_asm::assemble_tokens(tokens).unwrap();

        let (tx_thread, _rx_thread) = std::sync::mpsc::channel();
        let (_tx_ui, rx_ui) = std::sync::mpsc::channel();

        let mut cpu = CpuState::new(rx_ui, tx_thread).unwrap();
        cpu.handle_msg(crate::messages::UiToThread::SetCode(asm));

        let mut serial_output = Vec::new();
        let mut iter_count = 0;

        while cpu.cpu.get_current_op().unwrap() != Processor::OP_HALT {
            match cpu.step_cpu(false) {
                Err(crate::messages::ThreadToUi::LogMessage(msg)) => {
                    panic!("unable to step CPU\n{msg}")
                }
                Err(err) => panic!("unable to step cpu - {err:?}"),
                _ => (),
            };
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
            @65536, 10\n\
            @65558, 12\n\
            @65582, 30\n\
            @65624, 45\n\
            B\n\
            @65558, 12\n\
            @65582, 30\n\
            @65624, 45\n\
            C\n\
            @65536, 5\n\
            @65558, 12\n\
            @65582, 30\n\
            @65624, 45\n\
            D\n\
            No Heap Allocations\n\
            E\n\
            @65536, 33\n\
            F\n\
            No Heap Allocations\n\
            Heap Test Pass\n";

        run_cpu_serial_out_test(include_str!("../../cbuoy/tests/test_kmalloc.cb"), EXPECTED);
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
            include_str!("../../cbuoy/tests/test_struct_ptr.cb"),
            EXPECTED,
        );
    }
}
