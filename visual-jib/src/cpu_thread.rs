use crate::messages::{ThreadToUi, UiToThread};
use jib_asm::InstructionList;
use jib_computer::{ComputerError, JibComputer};
use std::sync::mpsc::{Receiver, RecvError, Sender, TryRecvError};

pub struct CpuState {
    running_prev: bool,
    running: bool,
    multiplier: f64,
    #[cfg(not(target_arch = "wasm32"))]
    run_thread: bool,
    last_code: Option<(u32, Vec<u8>)>,
    computer: JibComputer,
    breakpoint: Option<u32>,
    inst_map: InstructionList,
    rx: Receiver<UiToThread>,
    tx: Sender<ThreadToUi>,
}

impl CpuState {
    pub const THREAD_LOOP_MS: u64 = 25;

    pub fn new(rx: Receiver<UiToThread>, tx: Sender<ThreadToUi>) -> Result<Self, ComputerError> {
        let s = Self {
            running_prev: false,
            running: false,
            multiplier: 1.0,
            #[cfg(not(target_arch = "wasm32"))]
            run_thread: true,
            last_code: None,
            computer: JibComputer::new()?,
            inst_map: InstructionList::default(),
            breakpoint: None,
            rx,
            tx,
        };

        s.tx.send(ThreadToUi::BootloaderState(s.computer.using_bootloader()))
            .unwrap();

        Ok(s)
    }

    fn get_inst_history(&self) -> Vec<String> {
        self.computer
            .get_inst_history()
            .into_iter()
            .map(|(pc, inst)| {
                format!(
                    "0x{pc:08x} = {}",
                    self.inst_map
                        .get_display_inst(inst)
                        .unwrap_or("??".to_string())
                )
            })
            .collect::<Vec<_>>()
    }

    fn reset(&mut self) -> Result<(), ComputerError> {
        self.computer
            .reset(self.last_code.as_ref().map(|x| (x.0, x.1.as_slice())))?;
        Ok(())
    }

    fn handle_msg(&mut self, msg: UiToThread) -> Option<ThreadToUi> {
        fn inner_handler(
            state: &mut CpuState,
            msg: UiToThread,
        ) -> Result<Option<ThreadToUi>, ComputerError> {
            match msg {
                UiToThread::UseBootloader(bootloader) => {
                    state.computer.use_bootloader(bootloader)?;
                    state.reset()?;
                    state
                        .tx
                        .send(ThreadToUi::BootloaderState(
                            state.computer.using_bootloader(),
                        ))
                        .unwrap();
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::CpuStep => {
                    if let Err(e) = state.computer.step_cpu(false) {
                        return Ok(Some(ThreadToUi::LogMessage(e.to_string())));
                    }
                }
                UiToThread::CpuRun(set) => {
                    state.computer.set_running_request(set);
                }
                #[cfg(not(target_arch = "wasm32"))]
                UiToThread::Exit => state.run_thread = false,
                UiToThread::CpuReset => {
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::CpuIrq(irq) => {
                    if !state.computer.trigger_irq(irq as u32)? {
                        return Ok(Some(ThreadToUi::LogMessage(format!(
                            "irq {irq} not triggered"
                        ))));
                    }
                }
                UiToThread::SetMultiplier(m) => {
                    state.multiplier = m;
                }
                UiToThread::SetCode(data) => {
                    state.last_code = Some((data.start_address, data.bytes));
                    state.reset()?;
                    return Ok(Some(ThreadToUi::ProcessorReset));
                }
                UiToThread::SerialInput(s) => match state.computer.set_serial_input(&s) {
                    Ok(true) => (),
                    Ok(false) => {
                        return Ok(Some(ThreadToUi::LogMessage(
                            "device serial input buffer full".to_string(),
                        )));
                    }
                    Err(e) => return Ok(Some(ThreadToUi::LogMessage(e.to_string()))),
                },
                UiToThread::RequestMemory(base, size) => {
                    // Send memory if needed
                    let mut resp_memory = Vec::new();
                    for i in 0..size {
                        resp_memory.push(
                            state
                                .computer
                                .cpu
                                .memory_inspect(base + i)
                                .unwrap_or_default(),
                        );
                    }
                    state
                        .tx
                        .send(ThreadToUi::ResponseMemory(base, resp_memory))
                        .unwrap();
                }
                UiToThread::DiskReset => {
                    state.reset()?;
                }
                #[cfg(not(target_arch = "wasm32"))]
                UiToThread::DiskSave => {
                    use cbfs_lib::{ContainerHeader, save_container};

                    save_container(
                        &ContainerHeader::default(),
                        &cbfs_lib::FileSystem::read_bytes(
                            &mut state.computer.get_disk_data()?.as_slice(),
                        )?,
                        std::path::Path::new("hd.cbfs"),
                    )
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

    pub fn process_messages(&mut self, blocking: bool) -> Result<(), ComputerError> {
        const MSGS_PERLOOP: usize = 1000;

        if self.computer.get_running() {
            for _ in 0..MSGS_PERLOOP {
                let resp = match self.rx.try_recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(TryRecvError::Disconnected) => panic!("disconnected!"),
                    Err(TryRecvError::Empty) => break,
                };

                if let Some(r) = &resp {
                    self.tx
                        .send(r.clone())
                        .expect("Unable to send response to main thread!");
                }
            }
        } else {
            let resp = if blocking && !self.computer.get_running() {
                match self.rx.recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(RecvError) => panic!("receive error!"),
                }
            } else {
                if self.running && self.computer.cpu.step_devices().unwrap() {
                    // TODO - HERE!
                    self.running = true;
                }

                match self.rx.try_recv() {
                    Ok(msg) => self.handle_msg(msg),
                    Err(TryRecvError::Empty) => return Ok(()),
                    Err(TryRecvError::Disconnected) => panic!("disconnected!"),
                }
            };

            if let Some(r) = &resp {
                self.tx
                    .send(r.clone())
                    .expect("Unable to send response to main thread!");
            }
        }

        if self.running {
            let step_repeat_count = 2i64.pow(self.multiplier as u32);

            for _ in 0..step_repeat_count {
                if let Err(msg) = self.computer.step_cpu(true) {
                    self.running = false;
                    self.tx
                        .send(ThreadToUi::LogMessage(msg.to_string()))
                        .unwrap();
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
                self.computer.cpu.get_register_state(),
            )))
            .unwrap();

        let pc = self
            .computer
            .cpu
            .get_register_state()
            .get(jib::cpu::Register::ProgramCounter)
            .unwrap_or(0);
        let mem = self.computer.cpu.memory_inspect_u32(pc).unwrap_or(0);

        self.tx
            .send(ThreadToUi::ProgramCounterValue(pc, mem))
            .unwrap();

        // Send CPU state
        if self.running_prev != self.running {
            self.running_prev = self.running;
            self.tx.send(ThreadToUi::CpuRunning(self.running)).unwrap();
        }

        // Check for serial output
        let char_vec = self.computer.get_serial_output()?;
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
