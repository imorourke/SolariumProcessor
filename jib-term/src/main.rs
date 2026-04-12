use std::time::Duration;

use jib_computer::{ComputerError, JibComputer};

fn main() -> Result<(), ComputerError> {
    let mut computer = JibComputer::new()?;
    computer.use_bootloader(true)?;
    computer.reset(None)?;
    computer.set_running_request(true);

    let stdin_channel = spawn_stdin_channel();

    loop {
        match stdin_channel.try_recv() {
            Ok(input) => {
                computer.set_serial_input(&input)?;
            }
            Err(std::sync::mpsc::TryRecvError::Empty) => (),
            Err(std::sync::mpsc::TryRecvError::Disconnected) => panic!("stdin disconnected"),
        }

        if computer.get_running() {
            for _ in 0..10000 {
                if !computer.step_cpu(None, true)? {
                    break;
                }
            }
            std::thread::sleep(Duration::from_millis(10));
        } else {
            computer.step_devices()?;
            std::thread::sleep(Duration::from_millis(100));
        }

        for c in computer.get_serial_output_unknown() {
            print!("{c}");
        }
    }
}

fn spawn_stdin_channel() -> std::sync::mpsc::Receiver<String> {
    let (tx, rx) = std::sync::mpsc::channel::<String>();
    std::thread::spawn(move || {
        loop {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer).unwrap();
            tx.send(buffer).unwrap();
        }
    });
    rx
}
