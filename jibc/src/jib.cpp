#include "jib.h"
#include <cstddef>

namespace jib {

/// Instructions

static const uint8_t OP_BASE_CPU = 0x00;
static const uint8_t OP_CPU_NOOP = OP_BASE_CPU | 0;
static const uint8_t OP_CPU_RESET = OP_BASE_CPU | 1;
static const uint8_t OP_CPU_INTERRUPT = OP_BASE_CPU | 2;
static const uint8_t OP_CPU_INTERRUPT_REGISTER = OP_BASE_CPU | 3;
static const uint8_t OP_CPU_INTERRUPT_RETURN = OP_BASE_CPU | 4;
static const uint8_t OP_CPU_CALL = OP_BASE_CPU | 5;
static const uint8_t OP_CPU_RETURN = OP_BASE_CPU | 6;
static const uint8_t OP_CPU_PUSH = OP_BASE_CPU | 7;
static const uint8_t OP_CPU_POP = OP_BASE_CPU | 8;
static const uint8_t OP_CPU_POP_REG = OP_BASE_CPU | 9;
static const uint8_t OP_CPU_JUMP = OP_BASE_CPU | 0xA;
static const uint8_t OP_CPU_JUMP_REL = OP_BASE_CPU | 0xB;
static const uint8_t OP_CPU_JUMP_REL_IMM = OP_BASE_CPU | 0xC;
static const uint8_t OP_CPU_HALT = OP_BASE_CPU | 0xF;

static const uint8_t OP_BASE_MEM = 0x10;
static const uint8_t OP_MEM_LOAD = OP_BASE_MEM | 0;
static const uint8_t OP_MEM_LOAD_REL = OP_BASE_MEM | 1;
static const uint8_t OP_MEM_LOAD_IMM = OP_BASE_MEM | 2;
static const uint8_t OP_MEM_LOAD_IMM_REL = OP_BASE_MEM | 3;
static const uint8_t OP_MEM_LOAD_NEXT = OP_BASE_MEM | 4;
static const uint8_t OP_MEM_LOAD_NEXT_OFFSET = OP_BASE_MEM | 5;
static const uint8_t OP_MEM_SAVE = OP_BASE_MEM | 6;
static const uint8_t OP_MEM_SAVE_REL = OP_BASE_MEM | 7;
static const uint8_t OP_MEM_COPY = OP_BASE_MEM | 8;
static const uint8_t OP_MEM_CONV = OP_BASE_MEM | 9;

static const uint8_t OP_BASE_TEST = 0x20;
static const uint8_t OP_TEST_EQ = OP_BASE_TEST | 0;
static const uint8_t OP_TEST_NEQ = OP_BASE_TEST | 1;
static const uint8_t OP_TEST_GREATER = OP_BASE_TEST | 2;
static const uint8_t OP_TEST_GREATER_EQ = OP_BASE_TEST | 3;
static const uint8_t OP_TEST_LESS = OP_BASE_TEST | 4;
static const uint8_t OP_TEST_LESS_EQ = OP_BASE_TEST | 5;

static const uint8_t OP_BASE_LOGIC = 0x30;
static const uint8_t OP_LOGIC_NOT = OP_BASE_LOGIC | 0;
static const uint8_t OP_LOGIC_BOOL = OP_BASE_LOGIC | 1;
static const uint8_t OP_LOGIC_TEST_ZERO = OP_BASE_LOGIC | 2;
static const uint8_t OP_LOGIC_TEST_NOT_ZERO = OP_BASE_LOGIC | 3;

static const uint8_t OP_BASE_STATUS_FLAGS = 0x40;
static const uint8_t OP_FLAGS_INTERRUPT_ENABLE = OP_BASE_STATUS_FLAGS | 0;
static const uint8_t OP_FLAGS_INTERRUPT_DISABLE = OP_BASE_STATUS_FLAGS | 1;

static const uint8_t OP_BASE_DEBUG = 0x50;
static const uint8_t OP_DEBUG_BREAK = OP_BASE_DEBUG | 0;

static const uint8_t OP_BASE_MATH = 0xA0;
static const uint8_t OP_MATH_ADD = OP_BASE_MATH | 0;
static const uint8_t OP_MATH_SUB = OP_BASE_MATH | 1;
static const uint8_t OP_MATH_MUL = OP_BASE_MATH | 2;
static const uint8_t OP_MATH_DIV = OP_BASE_MATH | 3;
static const uint8_t OP_MATH_REM = OP_BASE_MATH | 4;
static const uint8_t OP_MATH_NEG = OP_BASE_MATH | 5;

static const uint8_t OP_BASE_BITS = 0xB0;
static const uint8_t OP_BITS_BAND = OP_BASE_BITS | 0;
static const uint8_t OP_BITS_BOR = OP_BASE_BITS | 1;
static const uint8_t OP_BITS_BXOR = OP_BASE_BITS | 2;
static const uint8_t OP_BITS_BSHL = OP_BASE_BITS | 3;
static const uint8_t OP_BITS_BSHR = OP_BASE_BITS | 4;
static const uint8_t OP_BITS_BNOT = OP_BASE_BITS | 5;
static const uint8_t OP_BITS_BSWAP = OP_BASE_BITS | 6;

/// EXCEPTIONS

ProcessorException::ProcessorException(const std::string& msg)
    : msg(msg) {}

ProcessorException::~ProcessorException() {}

const char* ProcessorException::what() const { return msg.c_str(); }

MemoryException::MemoryException(const std::string& msg, uint32_t addr)
    : ProcessorException(msg),
      addr(addr) {}

/// STATUS FLAGS

StatusFlags::StatusFlags(uint32_t val)
    : val(val) {}

bool StatusFlags::get_flag(word_t flag) const { return (val & (1 << flag)) != 0; }

void StatusFlags::set_flag(word_t flag, bool value) {
    word_t mask = 1 << flag;
    if (value) {
        val |= mask;
    } else {
        val &= ~mask;
    }
}

void StatusFlags::set_executing_interrupt(size_t ind) {
    val = (val & ~(VAL_INTERRUPT_MASK << VAL_INTERRUPT_BASE)) | ((static_cast<word_t>(ind) & VAL_INTERRUPT_MASK) << VAL_INTERRUPT_BASE);
}

word_t StatusFlags::get_executing_interrupt() const { return (val >> VAL_INTERRUPT_BASE) & VAL_INTERRUPT_MASK; }

const word_t StatusFlags::FLAG_INTERRUPT_ENABLED = 0;
const word_t StatusFlags::FLAG_INTERRUPT_EXECUTING = 1;
const word_t StatusFlags::VAL_INTERRUPT_BASE = 2;
const word_t StatusFlags::VAL_INTERRUPT_MASK = 0x3F;
const word_t StatusFlags::FLAG_CARRY = 8;

/// REGISTERS

word_t Registers::get(size_t i) const { return registers[i]; }

void Registers::set(size_t i, word_t val) { registers[i] = val; }

bool Registers::interrupt_enabled() const { return get_flags().get_flag(StatusFlags::FLAG_INTERRUPT_ENABLED); }

StatusFlags Registers::get_flags() const { return StatusFlags(registers[REG_STAT]); }

void Registers::set_flags(StatusFlags& flags) { registers[REG_SP] = flags.val; }

void Registers::reset() { memset(&registers, 0, sizeof(registers)); }

const size_t Registers::REG_PC = 0;
const size_t Registers::REG_STAT = 1;
const size_t Registers::REG_SP = 2;
const size_t Registers::REG_LDO = 3;
const size_t Registers::REG_RET = 4;
const size_t Registers::REG_ARG_BASE = 5;

/// Interrupts

const size_t InterruptController::NUM_INTERRUPTS = 64;
const size_t InterruptController::NUM_NON_MASKABLE = 8;

int32_t InterruptController::has_interrupt() const {
    for (size_t i = 0; i < NUM_INTERRUPTS; ++i) {
        if ((interrupts & (1 << i)) != 0) {
            return static_cast<int32_t>(i);
        }
    }

    return -1;
}

void InterruptController::queue_interrupt(size_t num) {
    if (num < NUM_INTERRUPTS) {
        interrupts |= (1 << num);
    } else {
        throw ProcessorException("invalid interrupt index provided to set");
    }
}

void InterruptController::clear_interrupt(size_t num) {
    if (num < NUM_INTERRUPTS) {
        interrupts &= ~(1 << num);
    } else {
        throw ProcessorException("invalid interrupt index provided to clear");
    }
}

void InterruptController::reset() { interrupts = 0; }

/// Instruction

class Instruction {
    uint32_t value;

public:
    Instruction(uint32_t val)
        : value(val) {}

    uint8_t get_opcode() const { return static_cast<uint8_t>(value & 0xFF); }
    uint8_t get_arg0() const { return static_cast<uint8_t>((value >> 8) & 0xFF); }
    uint8_t get_arg1() const { return static_cast<uint8_t>((value >> 16) & 0xFF); }
    uint8_t get_arg2() const { return static_cast<uint8_t>((value >> 24) & 0xFF); }

    uint16_t imm_unsigned() const { return (static_cast<uint16_t>(get_arg1()) << 8) | static_cast<uint16_t>(get_arg2()); }

    int16_t imm_signed() const { return static_cast<int16_t>(imm_unsigned()); }
};

// Processor

const uint32_t Processor::INTERRUPT_BASE = 0x100;

word_t Processor::get_interrupt_address(size_t interrupt) {
    if (interrupt < InterruptController::NUM_INTERRUPTS) {
        return INTERRUPT_BASE + sizeof(uint32_t) * interrupt;
    } else {
        throw ProcessorException("interrupt count exceeds limit");
    }
}

bool Processor::call_interrupt(size_t interrupt, uint32_t* data) {
    if (interrupt >= InterruptController::NUM_NON_MASKABLE && !registers.interrupt_enabled()) {
        return false;
    }

    word_t new_pc = memory.get_u32(get_interrupt_address(interrupt));
    if (new_pc == 0) {
        interrupts.clear_interrupt(interrupt);
        return true;
    }

    if (get_current_op() == OP_CPU_HALT) {
        registers.set(Registers::REG_PC, get_current_pc() + sizeof(word_t));
    }

    // Update the register values
    push_all_registers();
    StatusFlags flags = registers.get_flags();

    flags.set_flag(StatusFlags::FLAG_INTERRUPT_EXECUTING, true);
    flags.set_executing_interrupt(interrupt);

    registers.set_flags(flags);
    registers.set(Registers::REG_PC, new_pc);

    // Add data if provided
    if (data != NULL) {
        registers.set(Registers::REG_ARG_BASE, *data);
    }

    return true;
}

void Processor::stack_push(word_t val) {
    word_t sp_curr = registers.get(Registers::REG_SP);
    memory.set_u32(sp_curr, val);
    registers.set(Registers::REG_SP, sp_curr + sizeof(word_t));
}

word_t Processor::stack_pop() {
    word_t sp_curr = registers.get(Registers::REG_SP);
    if (sp_curr < sizeof(word_t)) {
        throw ProcessorException("stack underflow");
    }

    sp_curr -= sizeof(word_t);
    registers.set(Registers::REG_SP, sp_curr);

    return memory.get_u32(sp_curr);
}

int32_t Processor::get_executing_interrupt() const {
    StatusFlags flags = registers.get_flags();
    if (flags.get_flag(StatusFlags::FLAG_INTERRUPT_EXECUTING)) {
        return flags.get_executing_interrupt();
    } else {
        return -1;
    }
}

void Processor::push_all_registers() {
    for (size_t i = 0; i < NUM_REGISTERS; ++i) {
        stack_push(registers.get(i));
    }
}

void Processor::pop_all_registers(bool save_return) {
    for (size_t i = NUM_REGISTERS; i > 0; --i) {
        word_t val = stack_pop();
        size_t reg = i - 1;

        if (save_return && reg != Registers::REG_RET) {
            registers.registers[reg] = val;
        }
    }
}

void Processor::reset(ResetType reset) {
    if (reset == RESET_HARD) {
        memory.reset();
    }

    const word_t vec = memory.get_u32(0x100 + sizeof(word_t) * (reset == RESET_HARD) ? 0 : 1);

    registers.reset();
    registers.set(Registers::REG_PC, memory.get_u32(vec));

    interrupts.reset();
}

bool Processor::queue_interrupt(size_t interrupt) { return false; }

void Processor::step() {
    word_t pc = memory.get_u32(registers.get(Registers::REG_PC));
    const Instruction inst(pc);
    const uint8_t op = inst.get_opcode();
    int32_t jump_val = sizeof(word_t);

    switch (op) {
    case OP_CPU_NOOP:
    case OP_DEBUG_BREAK:
        break;
    case OP_CPU_HALT:
        jump_val = 0;
        break;
    case OP_CPU_RESET:
        reset(RESET_SOFT);
        break;
    case OP_FLAGS_INTERRUPT_ENABLE: {
        StatusFlags flags = registers.get_flags();
        flags.set_flag(StatusFlags::FLAG_INTERRUPT_ENABLED, true);
        registers.set_flags(flags);
    } break;
    case OP_FLAGS_INTERRUPT_DISABLE: {
        StatusFlags flags = registers.get_flags();
        flags.set_flag(StatusFlags::FLAG_INTERRUPT_ENABLED, false);
        registers.set_flags(flags);
    } break;
    case OP_CPU_INTERRUPT:
        queue_interrupt(inst.get_arg0());
        break;
    case OP_CPU_INTERRUPT_REGISTER:
        queue_interrupt(inst.get_arg0());
        break;
    case OP_CPU_CALL:
        registers.set(Registers::REG_PC, pc + sizeof(word_t));
        push_all_registers();
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0()));
        jump_val = 0;
        break;
    case OP_CPU_INTERRUPT_RETURN: {
        int int_index = get_executing_interrupt();
        if (int_index >= 0) {
            interrupts.clear_interrupt(int_index);
        }
    }
    case OP_CPU_RETURN:
        pop_all_registers(op == OP_CPU_RETURN);
        jump_val = 0;
        break;
    case OP_CPU_PUSH:
        stack_push(registers.get(inst.get_arg0()));
        break;
    case OP_CPU_POP:
        stack_pop();
        break;
    case OP_CPU_POP_REG: {
        word_t val = stack_pop();
        registers.set(inst.get_arg0(), val);
    } break;
    case OP_CPU_JUMP:
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0()));
        jump_val = 0;
        break;
    case OP_CPU_JUMP_REL:
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0()) + pc);
        break;
    case OP_CPU_JUMP_REL_IMM:
        registers.set(Registers::REG_PC, pc + inst.imm_signed());
        break;
    default:
        throw "error value";
    }

    if (jump_val != 0) {
#if !defined(__GNUC__) || __GNUC__ >= 7
        __builtin_add_overflow(pc, jump_val, &pc);
#else
        pc += jump_val;
#endif

        registers.set(Registers::REG_PC, pc);
    }
}

}
