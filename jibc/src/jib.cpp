#include "jib.h"

namespace jib {

/// EXCEPTIONS

ProcessorException::ProcessorException(const std::string& msg)
    : msg(msg) {}

const char* ProcessorException::what() const { return msg.c_str(); }

MemoryException::MemoryException(const std::string& msg, uint32_t addr)
    : ProcessorException(msg),
      addr(addr) {}

/// STATUS FLAGS

StatusFlags::StatusFlags(uint32_t val)
    : val(val) {}

bool StatusFlags::get_flag(uint32_t flag) const { return (val & (1 << flag)) != 0; }

StatusFlags StatusFlags::with_flag(uint32_t flag, bool value) const {
    uint32_t v = val;
    uint32_t mask = 1 << flag;
    if (value) {
        v |= mask;
    } else {
        v &= ~mask;
    }
    return v;
}

void StatusFlags::set_flag(uint32_t flag, bool value) { val = with_flag(flag, value).val; }

const uint32_t StatusFlags::FLAG_INTERRUPTS_ENABLED = 0;

/// REGISTERS

uint32_t Registers::get(size_t i) const { return registers[i]; }

void Registers::set(size_t i, uint32_t val) { registers[i] = val; }

const size_t Registers::REG_PC = 0;
const size_t Registers::REG_STAT = 1;
const size_t Registers::REG_SP = 2;
const size_t Registers::REG_LDO = 3;
const size_t Registers::REG_RET = 4;
const size_t Registers::REG_BASE = 5;

/// CPU

class Instruction {
    uint32_t value;

public:
    Instruction(uint32_t val)
        : value(val) {}

    uint8_t get_opcode() const { return static_cast<uint8_t>(value & 0xFF); }
    uint8_t get_arg0() const { return static_cast<uint8_t>((value >> 8) && 0xFF); }
    uint8_t get_arg1() const { return static_cast<uint8_t>((value >> 16) && 0xFF); }
    uint8_t get_arg2() const { return static_cast<uint8_t>((value >> 24) && 0xFF); }
};

uint32_t Processor::get_reset_vector(uint32_t vector_num) { return memory.get_u32(0x100 + sizeof(uint32_t) * vector_num); }

void Processor::reset(ResetType reset) { const uint32_t vec = get_reset_vector((reset == ResetType::Hard) ? 0 : 1); }

bool Processor::queue_interrupt(size_t interrupt) { return false; }

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

void Processor::step() {
    uint32_t pc = memory.get_u32(registers.get(Registers::REG_PC));
    const Instruction inst(pc);
    const uint8_t op = inst.get_opcode();
    int32_t jump_val = sizeof(uint32_t);

    switch (op) {
    case OP_CPU_NOOP:
    case OP_DEBUG_BREAK:
    case OP_CPU_HALT:
        break;
    case OP_CPU_RESET:
        reset(ResetType::Soft);
        break;
    case OP_FLAGS_INTERRUPT_ENABLE:
        registers.set(
            Registers::REG_STAT, StatusFlags(registers.get(Registers::REG_STAT)).with_flag(StatusFlags::FLAG_INTERRUPTS_ENABLED, true).val
        );
        break;
    case OP_FLAGS_INTERRUPT_DISABLE:
        registers.set(
            Registers::REG_STAT, StatusFlags(registers.get(Registers::REG_STAT)).with_flag(StatusFlags::FLAG_INTERRUPTS_ENABLED, false).val
        );
        break;
    case OP_CPU_INTERRUPT:
        queue_interrupt(inst.get_arg0());
        break;
    default:
        throw "error value";
    }

    if (jump_val != 0) {
        __builtin_add_overflow(pc, jump_val, &pc);
        registers.set(Registers::REG_PC, pc);
    }
}

}
