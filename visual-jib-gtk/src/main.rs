mod main_window;

use gtk::prelude::*;
use gtk::{Application, glib};

const APP_ID: &str = "com.orourke.Solarium.VJib";

fn main() -> glib::ExitCode {
    let app = Application::builder().application_id(APP_ID).build();
    app.connect_activate(main_window::build_ui);
    app.run()
}
