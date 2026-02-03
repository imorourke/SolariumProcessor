use crate::cpu_thread::CpuState;
use crate::messages::{ThreadToUi, UiToThread};
use cbuoy::CodeGenerationOptions;
use eframe::egui::{
    self, CentralPanel, Context, Grid, Id, MenuBar, ScrollArea, Slider, TextBuffer, TextEdit,
};
use jib::cpu::RegisterManager;
use jib_asm::{AssemblerOutput, InstructionList};
use std::{path::PathBuf, sync::LazyLock, time::Duration};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct ProgramCounterView {
    pc: u32,
    val: u32,
}

impl ProgramCounterView {
    fn get_instruction_string(&self) -> String {
        static INSTRUCTIONS: LazyLock<InstructionList> = LazyLock::new(InstructionList::default);

        if let Some(disp_val) = INSTRUCTIONS.get_display_inst(self.val.into()) {
            disp_val
        } else {
            "??".into()
        }
    }
}

#[derive(Debug, Clone)]
struct MemoryViewWindow {
    base: u32,
    base_str: String,
    values: [Option<u8>; Self::SIZE],
    memory_id: Id,
    shown: bool,
}

impl MemoryViewWindow {
    const COLUMNS: usize = 8;
    const ROWS: usize = 6;
    const SIZE: usize = MemoryViewWindow::COLUMNS * MemoryViewWindow::ROWS;

    fn new(id: usize) -> Self {
        Self {
            base: 0,
            base_str: "0".into(),
            values: [None; Self::SIZE],
            memory_id: Id::from(format!("mem{id}")),
            shown: true,
        }
    }

    fn draw(&mut self, ctx: &Context) {
        let mut opened = self.shown;
        egui::Window::new("Memory")
            .open(&mut opened)
            .resizable(false)
            .id(self.memory_id)
            .show(ctx, |ui| {
                ui.label("Base Address");
                if ui
                    .text_edit_singleline(&mut self.base_str)
                    .on_hover_text_at_pointer("The base address for the memory view")
                    .lost_focus()
                {
                    if let Some(rest) = self.base_str.strip_prefix("0x")
                        && let Ok(num) = u32::from_str_radix(rest, 16)
                    {
                        self.base = num;
                    } else if let Ok(num) = self.base_str.parse::<u32>() {
                        self.base = num;
                    }

                    self.base_str = format!("{:#x}", self.base);
                }
                Grid::new("memory_view")
                    .striped(true)
                    .num_columns(Self::COLUMNS)
                    .show(ui, |ui| {
                        for (i, v) in self.values.iter().enumerate() {
                            if i % MemoryViewWindow::COLUMNS == 0 {
                                if i > 0 {
                                    ui.end_row();
                                }

                                ui.label(format!("{:#010x}", self.base + i as u32));
                            }

                            if let Some(b) = v {
                                ui.label(format!("{b:02x}"));
                            } else {
                                ui.label("??");
                            }
                        }
                    });
            });
        self.shown = opened;
    }
}

pub struct VisualJib {
    log_serial: String,
    log_text: String,
    text_serial_input: String,
    cpu_run_requested: bool,
    #[cfg(not(target_arch = "wasm32"))]
    cpu_thread: Option<std::thread::JoinHandle<()>>,
    #[cfg(target_arch = "wasm32")]
    cpu_state: CpuState,
    tx_ui: std::sync::mpsc::Sender<UiToThread>,
    tx_thread: std::sync::mpsc::Sender<ThreadToUi>,
    rx_ui: std::sync::mpsc::Receiver<ThreadToUi>,
    tx_window: std::sync::mpsc::Sender<CodeWindowAction>,
    rx_window: std::sync::mpsc::Receiver<CodeWindowAction>,
    registers: RegisterManager,
    program_counter: ProgramCounterView,
    current_cpu_speed: i32,
    last_cpu_speed: i32,
    code_windows: Vec<CodeWindow>,
    code_window_id: usize,
    memory_windows: Vec<MemoryViewWindow>,
    memory_window_id: usize,
}

impl Default for VisualJib {
    fn default() -> Self {
        let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
        let (tx_thread, rx_ui) = std::sync::mpsc::channel::<ThreadToUi>();
        let tx_thread_local = tx_thread.clone();
        let (tx_window, rx_window) = std::sync::mpsc::channel::<CodeWindowAction>();

        CodeWindow::new(
            0,
            tx_ui.clone(),
            tx_thread.clone(),
            tx_window.clone(),
            include_str!("../../cbuoy/components/os.cb").to_string(),
            CodeWindowType::Cbuoy,
            None,
        )
        .compile_cbuoy();

        tx_ui.send(UiToThread::CpuRun(true)).unwrap();

        Self {
            cpu_run_requested: false,
            #[cfg(not(target_arch = "wasm32"))]
            cpu_thread: Some(std::thread::spawn(move || {
                crate::cpu_thread::cpu_thread(rx_thread, tx_thread)
            })),
            log_serial: String::default(),
            log_text: String::default(),
            text_serial_input: String::default(),
            #[cfg(target_arch = "wasm32")]
            cpu_state: CpuState::new(rx_thread, tx_thread).unwrap(),
            tx_ui,
            rx_ui,
            tx_thread: tx_thread_local,
            tx_window,
            rx_window,
            registers: RegisterManager::default(),
            program_counter: ProgramCounterView::default(),
            current_cpu_speed: 100,
            last_cpu_speed: 0,
            code_windows: Vec::new(),
            code_window_id: 0,
            memory_windows: Vec::new(),
            memory_window_id: 0,
        }
    }
}

impl VisualJib {
    const SPEED_MIN: i32 = 1;
    const SPEED_MAX: i32 = 200;

    #[cfg(not(target_arch = "wasm32"))]
    pub fn name() -> &'static str {
        "V/Jib"
    }

    fn read_cpu_responses(&mut self) {
        while let Ok(msg) = self.rx_ui.try_recv() {
            match msg {
                ThreadToUi::ProcessorReset => {
                    self.log_serial.clear();
                }
                ThreadToUi::RegisterState(regs) => {
                    self.registers = *regs;
                }
                ThreadToUi::ProgramCounterValue(pc, val) => {
                    self.program_counter.pc = pc;
                    self.program_counter.val = val;
                }
                ThreadToUi::LogMessage(msg) => {
                    if !self.log_text.is_empty() {
                        self.log_text = format!("{}\n{}", self.log_text, msg);
                    } else {
                        self.log_text = msg;
                    }
                }
                ThreadToUi::SerialOutput(msg) => {
                    self.log_serial = format!("{}{}", self.log_serial, msg);
                }
                ThreadToUi::ResponseMemory(base, vals) => {
                    for mem in self.memory_windows.iter_mut() {
                        if mem.base == base {
                            for (i, v) in mem.values.iter_mut().enumerate() {
                                if i < vals.len() {
                                    *v = Some(vals[i]);
                                } else {
                                    *v = None;
                                }
                            }
                        }
                    }
                }
                ThreadToUi::CpuRunning(running) => self.cpu_run_requested = running,
                #[cfg(not(target_arch = "wasm32"))]
                ThreadToUi::ThreadExit => std::process::exit(1),
            };
        }

        for mem in self.memory_windows.iter() {
            self.tx_ui
                .send(UiToThread::RequestMemory(mem.base, mem.values.len() as u32))
                .unwrap();
        }

        #[cfg(target_arch = "wasm32")]
        self.cpu_state.process_messages(false).unwrap();
    }

    fn update_interval(&self) -> Option<Duration> {
        Some(Duration::from_millis(if self.cpu_run_requested {
            CpuState::THREAD_LOOP_MS
        } else {
            5000
        }))
    }

    fn open_code_window(
        &mut self,
        code_type: CodeWindowType,
        code: String,
        filepath: Option<&'static str>,
    ) {
        self.code_windows.push(CodeWindow::new(
            self.code_window_id,
            self.tx_ui.clone(),
            self.tx_thread.clone(),
            self.tx_window.clone(),
            code,
            code_type,
            filepath,
        ));
        self.code_window_id += 1;
    }

    fn open_memory_window(&mut self) {
        self.memory_windows
            .push(MemoryViewWindow::new(self.memory_window_id));
        self.memory_window_id += 1;
    }
}

impl eframe::App for VisualJib {
    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        #[cfg(not(target_arch = "wasm32"))]
        if self.tx_ui.send(UiToThread::Exit).is_ok() {
            self.cpu_thread.take().map(|x| x.join());
        } else {
            std::process::exit(1);
        }
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.read_cpu_responses();

        // Remove old windows
        {
            let mut i = 0;
            while i < self.code_windows.len() {
                if !self.code_windows[i].shown {
                    self.code_windows.swap_remove(i);
                } else {
                    i += 1;
                }
            }
        }

        {
            let mut i = 0;
            while i < self.memory_windows.len() {
                if !self.memory_windows[i].shown {
                    self.memory_windows.swap_remove(i);
                } else {
                    i += 1;
                }
            }
        }

        while let Ok(msg) = self.rx_window.try_recv() {
            match msg {
                CodeWindowAction::NewAssemblyWindow(code, filename) => {
                    self.open_code_window(CodeWindowType::Assembly, code, filename);
                }
            }
        }

        CentralPanel::default().show(ctx, |ui| {
            MenuBar::new().ui(ui, |ui| {
                #[cfg(not(target_arch = "wasm32"))]
                ui.menu_button("File", |ui| {
                    if ui.button("Quit").clicked() {
                        ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });

                ui.menu_button("C/Buoy", |ui| {
                    if ui.button("New").clicked() {
                        self.open_code_window(CodeWindowType::Cbuoy, String::new(), None);
                    }

                    if ui.button("CB/OS").clicked() {
                        self.open_code_window(
                            CodeWindowType::Cbuoy,
                            include_str!("../../cbuoy/components/os.cb").to_string(),
                            Some("os.cb"),
                        );
                    }

                    ui.menu_button("Examples", |ui| {
                        static CB_CODES: &[(&str, &str, &str)] = &[
                            (
                                "Default",
                                include_str!("../../cbuoy/examples/default.cb"),
                                "default.cb",
                            ),
                            (
                                "Threading",
                                include_str!("../../cbuoy/examples/threading.cb"),
                                "threading.cb",
                            ),
                            (
                                "kmalloc",
                                include_str!("../../cbuoy/tests/test_kmalloc.cb"),
                                "test/kmalloc.cb",
                            ),
                            (
                                "Structures",
                                include_str!("../../cbuoy/tests/test_struct_ptr.cb"),
                                "test/structures.cb",
                            ),
                        ];

                        for (name, code, path) in CB_CODES.iter().cloned() {
                            if ui.button(name).clicked() {
                                self.open_code_window(
                                    CodeWindowType::Cbuoy,
                                    code.into(),
                                    Some(path),
                                );
                            }
                        }
                    });
                    ui.menu_button("Components", |ui| {
                        for (path, code) in cbuoy::DEFAULT_FILES.iter().cloned() {
                            let name = path.split('/').next_back().unwrap_or(path);
                            if ui.button(name).clicked() {
                                self.open_code_window(
                                    CodeWindowType::Cbuoy,
                                    code.into(),
                                    Some(path),
                                );
                            }
                        }
                    });
                });

                ui.menu_button("J/ASM", |ui| {
                    if ui.button("New").clicked() {
                        self.open_code_window(CodeWindowType::Assembly, String::new(), None);
                    }

                    ui.menu_button("Examples", |ui| {
                        static ASM_CODES: &[(&str, &str, &str)] = &[
                            (
                                "Hello World",
                                include_str!("../../jib-asm/examples/hello_world.jsm"),
                                "hello_world.jsm",
                            ),
                            (
                                "Thread Test",
                                include_str!("../../jib-asm/examples/thread_test.jsm"),
                                "thread_test.jsm",
                            ),
                            (
                                "Serial Echo",
                                include_str!("../../jib-asm/examples/serial_echo.jsm"),
                                "serial_echo.jsm",
                            ),
                            (
                                "Infinite Counter",
                                include_str!("../../jib-asm/examples/infinite_counter.jsm"),
                                "infinite_counter.jsm",
                            ),
                        ];

                        for (name, code, filename) in ASM_CODES.iter().cloned() {
                            if ui.button(name).clicked() {
                                self.open_code_window(
                                    CodeWindowType::Assembly,
                                    code.into(),
                                    Some(filename),
                                );
                            }
                        }
                    });
                });

                ui.menu_button("Devices", |ui| {
                    if ui.button("Memory View").clicked() {
                        self.open_memory_window();
                    }

                    ui.menu_button("IRQ", |ui| {
                        for i in 0..16 {
                            if ui.button(format!("IRQ{}", i)).clicked() {
                                self.tx_ui.send(UiToThread::CpuIrq(i)).unwrap();
                            }
                        }
                    });

                    if ui.button("Reset Disk").clicked() {
                        self.tx_ui.send(UiToThread::DiskReset).unwrap();
                    }

                    #[cfg(not(target_arch = "wasm32"))]
                    if ui.button("Save Disk").clicked() {
                        self.tx_ui.send(UiToThread::DiskSave).unwrap();
                    }
                });
            });

            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.heading("Commands");
                    Grid::new("cpu_commands").show(ui, |ui| {
                        if ui.button("Step").clicked() {
                            self.tx_ui.send(UiToThread::CpuStep).unwrap();
                        }

                        if ui.button("Start").clicked() {
                            self.tx_ui.send(UiToThread::CpuRun(true)).unwrap();
                        }

                        if ui.button("Stop").clicked() {
                            self.tx_ui.send(UiToThread::CpuRun(false)).unwrap();
                        }

                        if ui.button("Reset").clicked() {
                            self.tx_ui.send(UiToThread::CpuReset).unwrap();
                        }
                    });

                    ui.label("Speed Multiplier");
                    ui.add(
                        Slider::new(
                            &mut self.current_cpu_speed,
                            VisualJib::SPEED_MIN..=VisualJib::SPEED_MAX,
                        )
                        .show_value(true),
                    );

                    if self.current_cpu_speed != self.last_cpu_speed {
                        self.last_cpu_speed = self.current_cpu_speed;
                        self.tx_ui
                            .send(UiToThread::SetMultiplier(self.current_cpu_speed as f64))
                            .unwrap();
                    }

                    ui.heading("CPU Registers");
                    const NUM_COLS: usize = 2;
                    Grid::new("cpu_registers")
                        .striped(true)
                        .num_columns(NUM_COLS)
                        .show(ui, |ui| {
                            const NUM_ROWS: usize = RegisterManager::REGISTER_COUNT / NUM_COLS;

                            for i in 0..NUM_ROWS {
                                ui.label(format!("R{:02}: {:08x}", i, self.registers.registers[i]));
                                ui.label(format!(
                                    "R{:02}: {:08x}",
                                    i + NUM_ROWS,
                                    self.registers.registers[i + NUM_ROWS]
                                ));
                                ui.end_row();
                            }
                        });

                    ui.heading("Program Log");

                    ScrollArea::both().stick_to_bottom(true).show(ui, |ui| {
                        TextEdit::multiline(&mut self.log_text)
                            .code_editor()
                            .cursor_at_end(true)
                            .interactive(false)
                            .clip_text(false)
                            .show(ui);
                    });
                });

                ui.vertical(|ui| {
                    ui.heading("Program Counter");
                    ui.label(format!(
                        "PC[0x{:08x}] = 0x{:08x}",
                        self.program_counter.pc, self.program_counter.val
                    ));
                    ui.label(format!(
                        "Inst: {}",
                        self.program_counter.get_instruction_string()
                    ));

                    ui.heading("Serial Input");
                    const RETURN_KEY: egui::Key = egui::Key::Enter;
                    const RETURN_SHORTCUT: egui::KeyboardShortcut =
                        egui::KeyboardShortcut::new(egui::Modifiers::NONE, RETURN_KEY);

                    let serial_txt = TextEdit::singleline(&mut self.text_serial_input)
                        .desired_width(ui.available_width())
                        .return_key(Some(RETURN_SHORTCUT))
                        .code_editor()
                        .show(ui)
                        .response;

                    if serial_txt.lost_focus() && ctx.input(|x| x.key_pressed(RETURN_KEY)) {
                        self.tx_ui
                            .send(UiToThread::SerialInput(self.text_serial_input.take()))
                            .unwrap();
                        serial_txt.request_focus();
                    }

                    ui.heading("Serial Log");
                    if ui.button("Clear").clicked() {
                        self.log_serial = String::new();
                    }
                    ScrollArea::vertical()
                        .id_salt("serial_log")
                        .stick_to_bottom(true)
                        .stick_to_right(true)
                        .show(ui, |ui| {
                            TextEdit::multiline(&mut self.log_serial)
                                .interactive(false)
                                .code_editor()
                                .desired_width(ui.available_width())
                                .show(ui);
                        });
                });

                for w in self.code_windows.iter_mut() {
                    w.draw(ctx)
                }

                for m in self.memory_windows.iter_mut() {
                    m.draw(ctx);
                }
            });
        });

        if let Some(int) = self.update_interval() {
            ctx.request_repaint_after(int);
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum CodeWindowType {
    Assembly,
    Cbuoy,
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum CodeWindowAction {
    NewAssemblyWindow(String, Option<&'static str>),
}

struct CodeWindow {
    tx_ui: std::sync::mpsc::Sender<UiToThread>,
    tx_thread: std::sync::mpsc::Sender<ThreadToUi>,
    tx_window: std::sync::mpsc::Sender<CodeWindowAction>,
    code: String,
    shown: bool,
    id: String,
    compiled: CodeWindowType,
    filename: Option<&'static str>,
}

impl CodeWindow {
    const CB_NAME: &str = "CB";
    const ASM_NAME: &str = "ASM";

    fn new(
        id: usize,
        tx_ui: std::sync::mpsc::Sender<UiToThread>,
        tx_thread: std::sync::mpsc::Sender<ThreadToUi>,
        tx_window: std::sync::mpsc::Sender<CodeWindowAction>,
        code: String,
        code_type: CodeWindowType,
        filename: Option<&'static str>,
    ) -> Self {
        Self {
            tx_ui,
            tx_thread,
            tx_window,
            code,
            shown: true,
            id: format!(
                "{} {id}{}",
                match code_type {
                    CodeWindowType::Assembly => "ASM",
                    CodeWindowType::Cbuoy => "C/Buoy",
                },
                if let Some(t) = &filename {
                    format!(" ({t})")
                } else {
                    String::new()
                }
            ),
            compiled: code_type,
            filename,
        }
    }

    fn compile_asm(&self) {
        let asm = &self.code;
        match jib_asm::assemble_text(asm.as_str()) {
            Ok(v) => {
                self.tx_ui.send(UiToThread::SetCode(v)).unwrap();
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!(
                        "{} Successful",
                        Self::ASM_NAME
                    )))
                    .unwrap();
            }
            Err(e) => self
                .tx_thread
                .send(ThreadToUi::LogMessage(format!("{}: {e}", Self::ASM_NAME)))
                .unwrap(),
        }
    }

    fn compile_cbuoy_to_asm(&self) -> Option<AssemblerOutput> {
        let preprocessed = match cbuoy::preprocess_code_as_file(
            &self.code,
            &if let Some(f) = &self.filename {
                PathBuf::from(f)
            } else {
                PathBuf::from("code.cb")
            },
            [].into_iter(),
        ) {
            Ok(val) => val,
            Err(err) => {
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!(
                        "{} Preprocessor: {err}",
                        Self::CB_NAME
                    )))
                    .unwrap();
                return None;
            }
        };

        let tokens = match preprocessed.tokenize() {
            Ok(val) => val,
            Err(e) => {
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!(
                        "{}: Tokenize error {e}",
                        Self::CB_NAME
                    )))
                    .unwrap();
                return None;
            }
        };

        let options = CodeGenerationOptions::default();

        match cbuoy::parse(tokens, options).and_then(|x| x.get_assembler()) {
            Ok(tokens) => match jib_asm::assemble_tokens(tokens) {
                Ok(asm) => Some(asm),
                Err(err) => {
                    self.tx_thread
                        .send(ThreadToUi::LogMessage(format!("{}: {err}", Self::CB_NAME)))
                        .unwrap();
                    None
                }
            },
            Err(err) => {
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!("{}: {err}", Self::CB_NAME)))
                    .unwrap();

                if let Some(t) = &err.token {
                    for l in preprocessed.get_lines().iter() {
                        if l.loc.line == t.get_loc().line
                            && Some(&l.loc.file) == t.get_loc().file.as_ref()
                        {
                            let line = &l.text;

                            let mut err_msg = format!("{} >> {line}\n", l.loc);
                            err_msg += &format!("{}    ", l.loc);
                            for _ in 0..t.get_loc().column {
                                err_msg += " ";
                            }
                            for _ in 0..t.get_value().len() {
                                err_msg += "^";
                            }
                            self.tx_thread
                                .send(ThreadToUi::LogMessage(err_msg))
                                .unwrap();
                            break;
                        }
                    }
                }

                None
            }
        }
    }

    fn compile_cbuoy(&self) {
        if let Some(asm_out) = self.compile_cbuoy_to_asm() {
            self.tx_ui.send(UiToThread::SetCode(asm_out)).unwrap();
            self.tx_thread
                .send(ThreadToUi::LogMessage(format!(
                    "{}: Compile Successful",
                    Self::CB_NAME
                )))
                .unwrap();
        }
    }

    fn draw(&mut self, ctx: &Context) {
        let mut opened = self.shown;

        egui::Window::new(self.id.as_str())
            .resizable(true)
            .open(&mut opened)
            .show(ctx, |ui| {
                if ui
                    .button(match self.compiled {
                        CodeWindowType::Cbuoy => "Compile",
                        CodeWindowType::Assembly => "Assemble",
                    })
                    .clicked()
                {
                    match self.compiled {
                        CodeWindowType::Assembly => self.compile_asm(),
                        CodeWindowType::Cbuoy => self.compile_cbuoy(),
                    };
                }

                if self.compiled == CodeWindowType::Cbuoy
                    && ui.button("Show Assembly").clicked()
                    && let Some(asm_out) = self.compile_cbuoy_to_asm()
                {
                    let asm = format!(
                        "{}\n{}",
                        asm_out.assembly_lines.join("\n"),
                        asm_out.assembly_debug.join("\n")
                    );

                    self.tx_window
                        .send(CodeWindowAction::NewAssemblyWindow(asm, self.filename))
                        .unwrap();
                }

                ScrollArea::vertical().auto_shrink(false).show(ui, |ui| {
                    let text = &mut self.code;
                    ui.add_sized(
                        ui.available_size(),
                        TextEdit::multiline(text)
                            .code_editor()
                            .cursor_at_end(false)
                            .desired_width(ui.available_width())
                            .clip_text(false),
                    );
                });
            });

        self.shown = opened;
    }
}
