pub mod cpu_thread;
pub mod messages;

pub const EXAMPLE_CB_OS: &str = include_str!(concat!(env!("OUT_DIR"), "/os.cb"));
pub const EXAMPLE_CB_DEFAULT: &str = include_str!(concat!(env!("OUT_DIR"), "/default.cb"));
pub const EXAMPLE_CB_THREADING: &str = include_str!(concat!(env!("OUT_DIR"), "/threading.cb"));
pub const EXAMPLE_CB_TEST_KMALLOC: &str =
    include_str!(concat!(env!("OUT_DIR"), "/test_kmalloc.cb"));
pub const EXAMPLE_CB_TEST_STRUCT_PTR: &str =
    include_str!(concat!(env!("OUT_DIR"), "/test_struct_ptr.cb"));
