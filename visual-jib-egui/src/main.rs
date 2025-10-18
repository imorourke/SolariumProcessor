use std::{io::Cursor, thread::JoinHandle};

use eframe::egui::{self, IconData, Visuals};
use visual_jib_lib::{
    EXAMPLE_CB_THREADING,
    messages::{ThreadToUi, UiToThread},
};

struct VisualJib {
    code_cbuoy: String,
    code_asm: String,
    log_serial: String,
    log_text: String,
    cpu_thread: Option<JoinHandle<()>>,
    tx_ui: std::sync::mpsc::Sender<UiToThread>,
    rx_ui: std::sync::mpsc::Receiver<ThreadToUi>,
}

impl VisualJib {
    fn name() -> &'static str {
        "V/Jib"
    }
}

impl Default for VisualJib {
    fn default() -> Self {
        let (tx_ui, rx_thread) = std::sync::mpsc::channel::<UiToThread>();
        let (tx_thread, rx_ui) = std::sync::mpsc::channel::<ThreadToUi>();

        let t = std::thread::spawn(move || {
            visual_jib_lib::cpu_thread::cpu_thread(rx_thread, tx_thread)
        });

        Self {
            code_cbuoy: EXAMPLE_CB_THREADING.into(),
            code_asm: include_str!("../../jib-asm/examples/thread_test.jsm").into(),
            cpu_thread: Some(t),
            tx_ui,
            rx_ui,
            log_serial: String::default(),
            log_text: String::default(),
        }
    }
}

impl eframe::App for VisualJib {
    fn on_exit(&mut self, _gl: Option<&eframe::glow::Context>) {
        self.tx_ui.send(UiToThread::Exit).unwrap();
        self.cpu_thread.take().map(|x| x.join());
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.set_visuals(Visuals::light());

        while let Ok(msg) = self.rx_ui.recv_timeout(std::time::Duration::from_millis(0)) {
            match msg {
                ThreadToUi::ProcessorReset => {
                    self.log_serial.clear();
                }
                ThreadToUi::RegisterState(regs) => {
                    for (i, r) in regs.registers.iter().enumerate() {
                        //register_fields[i].set_markup(&format!("<tt>0x{:08x}</tt>", r));
                    }
                }
                ThreadToUi::ProgramCounterValue(pc, val) => {
                    /*
                    if let Some(disp_val) = inst.get_display_inst(val.into()) {
                        serial_details
                            .label_instruction_details
                            .set_markup(&format!("<tt>{disp_val}</tt>"));
                    } else {
                        serial_details
                            .label_instruction_details
                            .set_markup("<tt>??</tt>");
                    }

                    serial_details
                        .label_instruction
                        .set_markup(&format!("<tt>Mem[0x{:08x}] = 0x{:08x}</tt>", pc, val));
                    */
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
                    /*
                    for (i, l) in serial_details.memory.labels.iter().enumerate() {
                        l.set_text(&format!(
                            "L{:08x}",
                            base + serial_details.memory.num_cols as u32 * i as u32
                        ));
                    }

                    for (i, m) in serial_details.memory.locations.iter().enumerate() {
                        if i < vals.len() {
                            m.set_text(&format!("{:02x}", vals[i]));
                        } else {
                            m.set_text("");
                        }
                    }
                    */
                }
                ThreadToUi::ThreadExit => break,
            };
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.with_layout(egui::Layout::left_to_right(egui::Align::Center), |ui| {
                ui.vertical(|ui| {
                    ui.heading("Code Entry");
                    if ui.button("Compile").clicked() {}
                    egui::ScrollArea::both()
                        .stick_to_right(true)
                        .stick_to_bottom(true)
                        .show(ui, |ui| {
                            ui.add(egui::TextEdit::multiline(&mut self.code_cbuoy).code_editor());
                        });
                });
                ui.vertical(|ui| {
                    ui.heading("CPU");
                });
                ui.vertical(|ui| {
                    ui.heading("Memory");
                })
            });

            /*
            ui.heading("This is a ui.heading. ");

            ui.label("This is a ui.label");

            // This literally creates the button AND checks to see if it was clicked
            if ui.button("Quit").clicked() {
                std::process::exit(0);
            };
            */
        });
    }
}

fn main() -> eframe::Result<()> {
    let mut icon_bytes = Vec::new();
    let img = image::load_from_memory(include_bytes!("../../doc/images/logo.png")).unwrap();
    img.write_to(&mut Cursor::new(&mut icon_bytes), image::ImageFormat::Png)
        .unwrap();

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size((1024.0, 600.0))
            .with_icon(IconData {
                rgba: icon_bytes,
                width: img.width(),
                height: img.height(),
            })
            .with_app_id("com.orourke.Solarium.VJib"),
        ..eframe::NativeOptions::default()
    };

    eframe::run_native(
        VisualJib::name(),
        native_options,
        Box::new(|_| Ok(Box::<VisualJib>::default())),
    )
}
