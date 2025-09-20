use std::io::Cursor;

use eframe::egui::{self, IconData};

#[derive(Default)]
struct VisualJib {}

impl VisualJib {
    fn name() -> &'static str {
        "V/Jib"
    }
}

impl eframe::App for VisualJib {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.set_pixels_per_point(1.5);

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.heading("This is a ui.heading. ");

            ui.label("This is a ui.label");

            // This literally creates the button AND checks to see if it was clicked
            if ui.button("Quit").clicked() {
                std::process::exit(0);
            };
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
