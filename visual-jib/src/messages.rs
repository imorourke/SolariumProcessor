use jib::cpu::RegisterManager;
use jib_asm::AssemblerOutput;

#[derive(Debug, Clone)]
pub enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    DiskReset,
    CpuIrq(u8),
    SetCode(AssemblerOutput),
    SerialInput(String),
    RequestMemory(u32, u32),
    SetMultiplier(f64),
    #[cfg(not(target_arch = "wasm32"))]
    Exit,
}

#[derive(Debug, Clone)]
pub enum ThreadToUi {
    ResponseMemory(u32, Vec<u8>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState(Box<RegisterManager>),
    ProgramCounterValue(u32, u32),
    ProcessorReset,
    #[cfg(not(target_arch = "wasm32"))]
    ThreadExit,
    CpuRunning(bool),
}
