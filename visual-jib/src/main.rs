mod app;
mod cpu_thread;
mod messages;

use app::VisualJib;
use eframe::egui::Visuals;

#[cfg(not(target_arch = "wasm32"))]
fn main() -> eframe::Result<()> {
    use eframe::egui::{self, IconData};
    use std::io::Cursor;

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

#[cfg(target_arch = "wasm32")]
fn main() {
    use eframe::wasm_bindgen::JsCast as _;

    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Debug).ok();

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        let document = web_sys::window()
            .expect("No window")
            .document()
            .expect("No document");

        let canvas = document
            .get_element_by_id("the_canvas_id")
            .expect("Failed to find the_canvas_id")
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .expect("the_canvas_id was not a HtmlCanvasElement");

        let start_result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|cc| {
                    cc.egui_ctx.set_visuals(Visuals::light());
                    Ok(Box::<VisualJib>::default())
                }),
            )
            .await;

        // Remove the loading text and spinner:
        if let Some(loading_text) = document.get_element_by_id("loading_text") {
            match start_result {
                Ok(_) => {
                    loading_text.remove();
                }
                Err(e) => {
                    loading_text.set_inner_html(
                        "<p> The app has crashed. See the developer console for details. </p>",
                    );
                    panic!("Failed to start eframe: {e:?}");
                }
            }
        }
    });
}
