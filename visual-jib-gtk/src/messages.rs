use jib::cpu::RegisterManager;
use jib_asm::AssemblerOutput;

#[derive(Clone)]
pub enum UiToThread {
    CpuStep,
    CpuStart,
    CpuStop,
    CpuReset,
    CpuIrq(u8),
    SetCode(AssemblerOutput),
    SerialInput(String),
    RequestMemory(u32, u32),
    SetBreakpoint(u32),
    SetMultiplier(f64),
    Exit,
}

#[derive(Clone)]
pub enum ThreadToUi {
    ResponseMemory(u32, Vec<u8>),
    SerialOutput(String),
    LogMessage(String),
    RegisterState(Box<RegisterManager>),
    ProgramCounterValue(u32, u32),
    ProcessorReset,
    ThreadExit,
}
