use crate::messages::{ThreadToUi, UiToThread};
use jib::{
    cpu::{Processor, ProcessorError},
    device::{InterruptClockDevice, SerialInputOutputDevice},
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

struct CpuState {
    running: bool,
    running_prev: bool,
    multiplier: f64,
    run_thread: bool,
    memory_request: (u32, u32),
    cpu: Processor,
    dev_serial_io: Rc<RefCell<SerialInputOutputDevice>>,
    last_code: Vec<u8>,
    inst_history: CircularBuffer<String, 10>,
    inst_map: InstructionList,
    breakpoint: Option<u32>,
    step_count: u128,
    rx: Receiver<UiToThread>,
    tx: Sender<ThreadToUi>,
}

impl CpuState {
    const DEVICE_START_IND: u32 = 0xA000;
    const THREAD_LOOP_MS: u64 = 50;
    const MSGS_PER_LOOP: u64 = 1000;

    fn new(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) -> Result<Self, ProcessorError> {
        let mut s = Self {
            run_thread: true,
            running: false,
            running_prev: false,
            multiplier: 1.0,
            cpu: Processor::default(),
            dev_serial_io: Rc::new(RefCell::new(SerialInputOutputDevice::new(2048))),
            last_code: Vec::new(),
            memory_request: (0, 0),
            inst_history: Default::default(),
            inst_map: InstructionList::default(),
            breakpoint: None,
            step_count: 0,
            rx,
            tx,
        };

        s.reset()?;
        Ok(s)
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
                (Self::DEVICE_START_IND - INIT_RO_LEN) as usize,
            ))),
        )?;
        self.cpu
            .memory_add_segment(Self::DEVICE_START_IND, self.dev_serial_io.clone())?;

        self.cpu.device_add(self.dev_serial_io.clone())?;

        let dev_timer = Rc::new(RefCell::new(InterruptClockDevice::new(0)));

        self.cpu.device_add(dev_timer.clone())?;
        self.cpu.memory_add_segment(
            Self::DEVICE_START_IND + self.dev_serial_io.borrow().len(),
            dev_timer,
        )?;

        self.cpu.reset(jib::cpu::ResetType::Hard)?;

        for (i, val) in self.last_code.iter().enumerate() {
            if i < INIT_RO_LEN as usize {
                continue;
            }

            self.cpu.memory_set(i as u32, *val)?;
        }

        Ok(())
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        fn inner_handler(
            state: &mut CpuState,
            msg: UiToThread,
        ) -> Result<Option<ThreadToUi>, ProcessorError> {
            match msg {
                UiToThread::SetBreakpoint(brk) => {
                    state.breakpoint = if brk == 0 { None } else { Some(brk) };
                    let msg = if brk == 0 {
                        "Disabling Breakpoint".into()
                    } else {
                        format!("Setting Breakpoint to 0x{brk:08x}")
                    };
                    return Ok(Some(ThreadToUi::LogMessage(msg)));
                }
                UiToThread::CpuStep => {
                    if let Err(e) = state.step_cpu(false) {
                        return Ok(Some(e));
                    }
                }
                UiToThread::CpuStart => state.running = true,
                UiToThread::CpuStop => state.running = false,
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
                    for c in s.chars().chain(['\n'; 1]) {
                        match jib::text::character_to_byte(c) {
                            Ok(word) => {
                                if !state.dev_serial_io.borrow_mut().push_input(word) {
                                    return Ok(Some(ThreadToUi::LogMessage(
                                        "device serial input buffer full".to_string(),
                                    )));
                                }
                            }
                            Err(e) => return Ok(Some(ThreadToUi::LogMessage(e.to_string()))),
                        }
                    }
                }
                UiToThread::RequestMemory(base, size) => state.memory_request = (base, size),
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

        // Send memory if needed
        let (base, size) = self.memory_request;
        let mut resp_memory = Vec::new();
        for i in 0..size {
            resp_memory.push(self.cpu.memory_inspect(base + i).unwrap_or_default());
        }
        self.tx
            .send(ThreadToUi::ResponseMemory(base, resp_memory))
            .unwrap();

        // Send CPU state
        if self.running_prev != self.running {
            self.running_prev = self.running;
            self.tx.send(ThreadToUi::CpuRunning(self.running)).unwrap();
        }

        Ok(())
    }
}

pub fn cpu_thread(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) {
    let mut state = CpuState::new(rx, tx.clone()).unwrap();

    while state.run_thread {
        if state.process_messages(true).is_err() {
            break;
        }

        // Final sleep
        if state.running {
            std::thread::sleep(std::time::Duration::from_millis(CpuState::THREAD_LOOP_MS));
        }
    }

    let _ = tx.send(ThreadToUi::ThreadExit);
}

#[cfg(test)]
mod test {
    use cbuoy::CodeGenerationOptions;
    use jib::cpu::Processor;

    use crate::{EXAMPLE_CB_TEST_KMALLOC, EXAMPLE_CB_TEST_STRUCT_PTR, cpu_thread::CpuState};

    fn run_cpu_serial_out_test(in_code: &str, expected_out: &str) {
        let tokens = cbuoy::parse_str(
            in_code,
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
            assert!(iter_count < 20000);
        }

        println!("Step Count: {}", cpu.step_count);
        println!("{}", str::from_utf8(&serial_output).unwrap());

        assert_eq!(str::from_utf8(&serial_output).unwrap(), expected_out);
    }

    #[test]
    fn test_malloc() {
        const EXPECTED: &str = "A\n\
            @36864, 10\n\
            @36886, 12\n\
            @36910, 30\n\
            @36952, 45\n\
            B\n\
            @36886, 12\n\
            @36910, 30\n\
            @36952, 45\n\
            C\n\
            @36864, 5\n\
            @36886, 12\n\
            @36910, 30\n\
            @36952, 45\n\
            D\n\
            No Heap Allocations\n\
            E\n\
            @36864, 33\n\
            F\n\
            No Heap Allocations\n\
            Heap Test Pass\n";

        run_cpu_serial_out_test(EXAMPLE_CB_TEST_KMALLOC, EXPECTED);
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
        run_cpu_serial_out_test(EXAMPLE_CB_TEST_STRUCT_PTR, EXPECTED);
    }
}
