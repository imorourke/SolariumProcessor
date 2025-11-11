use crate::{
    examples::{
        EXAMPLE_CB_DEFAULT, EXAMPLE_CB_OS, EXAMPLE_CB_TEST_KMALLOC, EXAMPLE_CB_TEST_STRUCT_PTR,
        EXAMPLE_CB_THREADING,
    },
    messages::{ThreadToUi, UiToThread},
};
use cbuoy::CodeGenerationOptions;
use eframe::egui::{
    self, CentralPanel, Context, Grid, Id, MenuBar, ScrollArea, Slider, TextBuffer, TextEdit,
};
use jib::cpu::RegisterManager;
use jib_asm::{AssemblerOutput, InstructionList};
use std::{sync::LazyLock, thread::JoinHandle, time::Duration};

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
                    } else {
                        self.base_str = format!("{}", self.base);
                    }
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
    text_debug_location: String,
    cpu_run_requested: bool,
    cpu_thread: Option<JoinHandle<()>>,
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

impl VisualJib {
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
                ThreadToUi::ThreadExit => std::process::exit(1),
            };
        }

        for mem in self.memory_windows.iter() {
            self.tx_ui
                .send(UiToThread::RequestMemory(mem.base, mem.values.len() as u32))
                .unwrap();
        }
    }

    fn update_interval(&self) -> Option<Duration> {
        Some(Duration::from_millis(if self.cpu_run_requested {
            10
        } else {
            100
        }))
    }

    fn open_code_window(&mut self, code_type: CodeWindowType, code: String) {
        self.code_windows.push(CodeWindow::new(
            self.code_window_id,
            self.tx_ui.clone(),
            self.tx_thread.clone(),
            self.tx_window.clone(),
            code,
            code_type,
        ));
        self.code_window_id += 1;
    }

    fn open_memory_window(&mut self) {
        self.memory_windows
            .push(MemoryViewWindow::new(self.memory_window_id));
        self.memory_window_id += 1;
    }
}

impl Default for VisualJib {
    fn default() -> Self {
        let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
        let (tx_thread, rx_ui) = std::sync::mpsc::channel::<ThreadToUi>();
        let tx_thread_local = tx_thread.clone();

        let t = std::thread::spawn(move || crate::cpu_thread::cpu_thread(rx_thread, tx_thread));

        let (tx_window, rx_window) = std::sync::mpsc::channel::<CodeWindowAction>();

        Self {
            cpu_run_requested: false,
            cpu_thread: Some(t),
            log_serial: String::default(),
            log_text: String::default(),
            text_serial_input: String::default(),
            text_debug_location: String::default(),
            tx_ui,
            rx_ui,
            tx_thread: tx_thread_local,
            tx_window,
            rx_window,
            registers: RegisterManager::default(),
            program_counter: ProgramCounterView::default(),
            current_cpu_speed: 0,
            last_cpu_speed: 0,
            code_windows: Vec::new(),
            code_window_id: 0,
            memory_windows: Vec::new(),
            memory_window_id: 0,
        }
    }
}

impl eframe::App for VisualJib {
    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
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
                CodeWindowAction::NewAssemblyWindow(code) => {
                    self.open_code_window(CodeWindowType::Assembly, code);
                }
            }
        }

        CentralPanel::default().show(ctx, |ui| {
            MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Quit").clicked() {
                        ui.ctx().send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });

                ui.menu_button("C/Buoy", |ui| {
                    if ui.button("New").clicked() {
                        self.open_code_window(CodeWindowType::Cbuoy, String::new());
                    }

                    ui.menu_button("Examples", |ui| {
                        static CB_CODES: &[(&str, &str)] = &[
                            ("Default", EXAMPLE_CB_DEFAULT),
                            ("OS", EXAMPLE_CB_OS),
                            ("Threading", EXAMPLE_CB_THREADING),
                            ("kmalloc", EXAMPLE_CB_TEST_KMALLOC),
                            ("Structures", EXAMPLE_CB_TEST_STRUCT_PTR),
                        ];

                        for (name, code) in CB_CODES.iter().cloned() {
                            if ui.button(name).clicked() {
                                self.open_code_window(CodeWindowType::Cbuoy, code.into());
                            }
                        }
                    });
                });

                ui.menu_button("J/ASM", |ui| {
                    if ui.button("New").clicked() {
                        self.open_code_window(CodeWindowType::Assembly, String::new());
                    }

                    ui.menu_button("Examples", |ui| {
                        static ASM_CODES: &[(&str, &str)] = &[
                            ("Empty", ""),
                            (
                                "Hello World",
                                include_str!("../../jib-asm/examples/hello_world.jsm"),
                            ),
                            (
                                "Thread Test",
                                include_str!("../../jib-asm/examples/thread_test.jsm"),
                            ),
                            (
                                "Serial Echo",
                                include_str!("../../jib-asm/examples/serial_echo.jsm"),
                            ),
                            (
                                "Infinite Counter",
                                include_str!("../../jib-asm/examples/infinite_counter.jsm"),
                            ),
                        ];

                        for (name, code) in ASM_CODES.iter().cloned() {
                            if ui.button(name).clicked() {
                                self.open_code_window(CodeWindowType::Assembly, code.into());
                            }
                        }
                    });
                });

                ui.menu_button("Memory", |ui| {
                    if ui.button("New View").clicked() {
                        self.open_memory_window();
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
                            self.tx_ui.send(UiToThread::CpuStart).unwrap();
                        }

                        if ui.button("Stop").clicked() {
                            self.tx_ui.send(UiToThread::CpuStop).unwrap();
                        }

                        ui.end_row();

                        if ui.button("Reset").clicked() {
                            self.tx_ui.send(UiToThread::CpuReset).unwrap();
                        }

                        if ui.button("IRQ1").clicked() {
                            self.tx_ui.send(UiToThread::CpuIrq(1)).unwrap();
                        }
                    });

                    ui.label("Speed Multiplier");
                    ui.add(
                        Slider::new(&mut self.current_cpu_speed, 1..=100)
                            .step_by(5.0)
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

                    ScrollArea::vertical().stick_to_bottom(true).show(ui, |ui| {
                        TextEdit::multiline(&mut self.log_text)
                            .cursor_at_end(true)
                            .interactive(false)
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

                    ui.heading("Breakpoint Location");
                    if ui
                        .text_edit_singleline(&mut self.text_debug_location)
                        .lost_focus()
                    {
                        let (radix, s) =
                            if let Some(rest) = self.text_debug_location.strip_prefix("0x") {
                                (16, rest)
                            } else {
                                (10, self.text_debug_location.as_str())
                            };

                        let loc = if let Ok(v) = u32::from_str_radix(s, radix) {
                            v
                        } else {
                            self.tx_thread
                                .send(ThreadToUi::LogMessage(format!(
                                    "unable to convert '{}' to program location",
                                    self.text_debug_location
                                )))
                                .unwrap();
                            0
                        };

                        self.tx_ui.send(UiToThread::SetBreakpoint(loc)).unwrap();
                    }

                    ui.heading("Serial Input");
                    TextEdit::singleline(&mut self.text_serial_input)
                        .return_key(None)
                        .show(ui);
                    if ui.button("Submit").clicked() {
                        self.tx_ui
                            .send(UiToThread::SerialInput(self.text_serial_input.take()))
                            .unwrap();
                    }

                    ui.heading("Serial Log");
                    ScrollArea::vertical()
                        .id_salt("serial_log")
                        .stick_to_bottom(true)
                        .show(ui, |ui| {
                            TextEdit::multiline(&mut self.log_serial)
                                .interactive(false)
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
    NewAssemblyWindow(String),
}

struct CodeWindow {
    tx_ui: std::sync::mpsc::Sender<UiToThread>,
    tx_thread: std::sync::mpsc::Sender<ThreadToUi>,
    tx_window: std::sync::mpsc::Sender<CodeWindowAction>,
    code: String,
    shown: bool,
    id: String,
    compiled: CodeWindowType,
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
    ) -> Self {
        Self {
            tx_ui,
            tx_thread,
            tx_window,
            code,
            shown: true,
            id: format!(
                "{} {id}",
                match code_type {
                    CodeWindowType::Assembly => "ASM",
                    CodeWindowType::Cbuoy => "C/Buoy",
                }
            ),
            compiled: code_type,
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
        let options = CodeGenerationOptions::default();

        match cbuoy::parse_str(&self.code, options).and_then(|x| x.get_assembler()) {
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
                    let line_num = t.get_loc().line;
                    let display_num = line_num + 1;
                    let line = self.code.lines().nth(line_num).unwrap();
                    self.tx_thread
                        .send(ThreadToUi::LogMessage(format!("> {display_num} >> {line}")))
                        .unwrap();

                    let mut err_msg = format!("> {display_num}    ");
                    for _ in 0..t.get_loc().column {
                        err_msg += " ";
                    }
                    for _ in 0..t.get_value().len() {
                        err_msg += "^";
                    }

                    self.tx_thread
                        .send(ThreadToUi::LogMessage(err_msg))
                        .unwrap();
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
                        .send(CodeWindowAction::NewAssemblyWindow(asm))
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
