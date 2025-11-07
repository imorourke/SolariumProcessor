use cbuoy::CodeGenerationOptions;
use eframe::egui::{
    self, CentralPanel, Grid, Layout, ScrollArea, Slider, TextBuffer, TextEdit, Vec2,
};
use jib::cpu::RegisterManager;
use jib_asm::InstructionList;
use std::{sync::LazyLock, thread::JoinHandle, time::Duration};
use visual_jib_lib::{
    EXAMPLE_CB_THREADING,
    messages::{ThreadToUi, UiToThread},
};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
enum CodeSelection {
    Asm,
    #[default]
    CBuoy,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct ProgramCounterView {
    pc: u32,
    val: u32,
}

impl ProgramCounterView {
    fn get_instruction_string(&self) -> String {
        static INSTRUCTIONS: LazyLock<InstructionList> =
            LazyLock::new(|| InstructionList::default());

        if let Some(disp_val) = INSTRUCTIONS.get_display_inst(self.val.into()) {
            disp_val
        } else {
            "??".into()
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct MemoryView {
    base: u32,
    values: [Option<u8>; Self::SIZE],
}

impl MemoryView {
    const COLUMNS: usize = 8;
    const ROWS: usize = 6;
    const SIZE: usize = MemoryView::COLUMNS * MemoryView::ROWS;
}

impl Default for MemoryView {
    fn default() -> Self {
        Self {
            base: 0,
            values: [None; Self::SIZE],
        }
    }
}

pub struct VisualJib {
    code_cbuoy: String,
    code_asm: String,
    code_selection: CodeSelection,
    log_serial: String,
    log_text: String,
    text_serial_input: String,
    text_debug_location: String,
    cpu_run_requested: bool,
    cpu_thread: Option<JoinHandle<()>>,
    tx_ui: std::sync::mpsc::Sender<UiToThread>,
    tx_thread: std::sync::mpsc::Sender<ThreadToUi>,
    rx_ui: std::sync::mpsc::Receiver<ThreadToUi>,
    registers: RegisterManager,
    memory_view: MemoryView,
    program_counter: ProgramCounterView,
    current_cpu_speed: i32,
    last_cpu_speed: i32,
}

impl VisualJib {
    pub fn name() -> &'static str {
        "V/Jib"
    }

    fn compile_asm(&self) {
        static ASM_NAME: &str = "ASM";
        let asm = &self.code_asm;
        match jib_asm::assemble_text(asm.as_str()) {
            Ok(v) => {
                self.tx_ui.send(UiToThread::SetCode(v)).unwrap();
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!("{ASM_NAME} Successful")))
                    .unwrap();
            }
            Err(e) => self
                .tx_thread
                .send(ThreadToUi::LogMessage(format!("{ASM_NAME}: {e}")))
                .unwrap(),
        }
    }

    fn compile_cbuoy(&mut self) {
        let options = CodeGenerationOptions::default();
        static CB_NAME: &str = "CB";

        match cbuoy::parse_str(&self.code_cbuoy, options).and_then(|x| x.get_assembler()) {
            Ok(tokens) => match jib_asm::assemble_tokens(tokens) {
                Ok(asm_out) => {
                    let asm = format!(
                        "{}\n{}",
                        asm_out.assembly_lines.join("\n"),
                        asm_out.assembly_debug.join("\n")
                    );

                    self.tx_ui.send(UiToThread::SetCode(asm_out)).unwrap();
                    self.tx_thread
                        .send(ThreadToUi::LogMessage(format!(
                            "{CB_NAME}: Compile Successful"
                        )))
                        .unwrap();

                    self.code_asm = asm;
                }
                Err(err) => {
                    self.tx_thread
                        .send(ThreadToUi::LogMessage(format!("{CB_NAME}: {err}")))
                        .unwrap();
                }
            },
            Err(err) => {
                self.tx_thread
                    .send(ThreadToUi::LogMessage(format!("{CB_NAME}: {err}")))
                    .unwrap();

                if let Some(t) = &err.token {
                    let line_num = t.get_loc().line;
                    let display_num = line_num + 1;
                    let line = self.code_cbuoy.lines().nth(line_num).unwrap();
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
            }
        }
    }

    fn step_cpu(&mut self) {
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
                    if self.log_text.len() > 0 {
                        self.log_text = format!("{}\n{}", self.log_text, msg);
                    } else {
                        self.log_text = msg;
                    }
                }
                ThreadToUi::SerialOutput(msg) => {
                    self.log_serial = format!("{}{}", self.log_serial, msg);
                }
                ThreadToUi::ResponseMemory(base, vals) => {
                    self.memory_view.base = base;
                    for (i, v) in self.memory_view.values.iter_mut().enumerate() {
                        if i < vals.len() {
                            *v = Some(vals[i]);
                        } else {
                            *v = None;
                        }
                    }
                }
                ThreadToUi::CpuRunning(running) => self.cpu_run_requested = running,
                ThreadToUi::ThreadExit => std::process::exit(1),
            };
        }
    }

    fn update_interval(&self) -> Option<Duration> {
        Some(Duration::from_millis(if self.cpu_run_requested {
            10
        } else {
            100
        }))
    }
}

impl Default for VisualJib {
    fn default() -> Self {
        let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
        let (tx_thread, rx_ui) = std::sync::mpsc::channel::<ThreadToUi>();
        let tx_thread_local = tx_thread.clone();

        let t = std::thread::spawn(move || {
            visual_jib_lib::cpu_thread::cpu_thread(rx_thread, tx_thread)
        });

        let memory_view = MemoryView::default();

        tx_ui
            .send(UiToThread::RequestMemory(
                memory_view.base,
                MemoryView::SIZE as u32,
            ))
            .unwrap();

        Self {
            code_cbuoy: EXAMPLE_CB_THREADING.into(),
            code_asm: include_str!("../../jib-asm/examples/thread_test.jsm").into(),
            code_selection: CodeSelection::CBuoy,
            cpu_run_requested: false,
            cpu_thread: Some(t),
            log_serial: String::default(),
            log_text: String::default(),
            text_serial_input: String::default(),
            text_debug_location: String::default(),
            tx_ui,
            rx_ui,
            tx_thread: tx_thread_local,
            registers: RegisterManager::default(),
            memory_view,
            program_counter: ProgramCounterView::default(),
            current_cpu_speed: 0,
            last_cpu_speed: 0,
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
        self.step_cpu();

        CentralPanel::default().show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.vertical(|ui| {
                    ui.heading("CPU Registers");
                    Grid::new("cpu_registers").show(ui, |ui| {
                        for (i, r) in self.registers.registers.iter().enumerate() {
                            if i > 0 && i % 2 == 0 {
                                ui.end_row();
                            }
                            ui.label(format!("R{i:02}: {r:08x}"));
                        }
                        ui.end_row();
                    });

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
                });

                ui.vertical(|ui| {
                    ui.heading("Memory");
                    Grid::new("memory_view")
                        .spacing(Vec2 { x: 0.0, y: 0.0 })
                        .show(ui, |ui| {
                            for (i, v) in self.memory_view.values.iter().enumerate() {
                                if i % MemoryView::COLUMNS == 0 {
                                    if i > 0 {
                                        ui.end_row();
                                    }

                                    ui.label(format!("L{:08x}", self.memory_view.base + i as u32));
                                }

                                if let Some(b) = v {
                                    ui.label(format!("{b:02x}"));
                                } else {
                                    ui.label("??");
                                }
                            }
                        });
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

                ui.with_layout(Layout::top_down_justified(egui::Align::Center), |ui| {
                    //ui.vertical(|ui| {
                    ui.heading("Code Entry");
                    ui.horizontal(|ui| {
                        if ui.button("Assembly").clicked() {
                            self.code_selection = CodeSelection::Asm;
                        }

                        if ui.button("C/Buoy").clicked() {
                            self.code_selection = CodeSelection::CBuoy;
                        }
                    });

                    ScrollArea::both()
                        .auto_shrink(false)
                        .stick_to_bottom(true)
                        .stick_to_right(true)
                        .show(ui, |ui| {
                            let widget = if self.code_selection == CodeSelection::Asm {
                                TextEdit::multiline(&mut self.code_asm)
                            } else if self.code_selection == CodeSelection::CBuoy {
                                TextEdit::multiline(&mut self.code_cbuoy)
                            } else {
                                panic!()
                            }
                            .code_editor();

                            if false {
                                ui.add_sized(
                                    Vec2 {
                                        x: 200.0,
                                        y: ui.available_height(),
                                    },
                                    widget,
                                );
                            } else {
                                ui.add(widget);
                            }
                        });

                    if ui.button("Compile").clicked() {
                        if self.code_selection == CodeSelection::Asm {
                            self.compile_asm();
                        } else if self.code_selection == CodeSelection::CBuoy {
                            self.compile_cbuoy();
                        }
                    }
                });
            });
        });

        if let Some(int) = self.update_interval() {
            ctx.request_repaint_after(int);
        }
    }
}
