mod app;
mod cpu_thread;
mod messages;

use app::VisualJib;
use eframe::egui::{self, IconData, Visuals};
use std::io::Cursor;

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
        Box::new(|cc| {
            cc.egui_ctx.set_visuals(Visuals::light());
            Ok(Box::<VisualJib>::default())
        }),
    )
}
