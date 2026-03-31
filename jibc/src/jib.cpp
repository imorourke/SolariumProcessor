#include "jib.h"

#include <cmath>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef __APPLE__
#include <machine/endian.h>
#else
#include <endian.h>
#endif

#if !defined(__GNUC__) || __GNUC__ >= 7
#define USE_OVERFLOW_FUNC
#endif

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

/// DATA TYPES

size_t data_type_byte_size(DataType dt) {
    switch (dt) {
    case DT_I8:
    case DT_U8:
        return 1;
    case DT_I16:
    case DT_U16:
        return 2;
    case DT_I32:
    case DT_U32:
    case DT_F32:
        return 4;
    default:
        throw ProcessorException("unknown data type");
    }
}

bool data_type_is_signed(DataType dt) {
    switch (dt) {
    case DT_I8:
    case DT_I16:
    case DT_I32:
    case DT_F32:
        return true;
    default:
        return false;
    }
}

bool data_type_is_integral(DataType dt) {
    switch (dt) {
    case DT_I8:
    case DT_U8:
    case DT_I16:
    case DT_U16:
    case DT_I32:
    case DT_U32:
        return true;
    default:
        return false;
    }
}

static word_t f32_to_bits(float val) {
    word_t out;
    memcpy(&out, &val, sizeof(word_t));
    return out;
}

static float bits_to_f32(word_t val) {
    float out;
    memcpy(&out, &val, sizeof(float));
    return out;
}

template <typename SRC, typename DST> word_t resulting_value_signed_dest(word_t value) {
    return static_cast<uint32_t>(static_cast<int32_t>(static_cast<DST>(static_cast<SRC>(value))));
}

word_t data_type_convert(word_t value, DataType src, DataType dst) {
    switch (dst) {
    case DT_U8:
        switch (src) {
        case DT_U8:
        case DT_U16:
        case DT_U32:
            return value & 0xFF;
        case DT_I8:
        case DT_I16:
        case DT_I32:
            return static_cast<uint32_t>(static_cast<int32_t>(value)) & 0xFF;
        case DT_F32:
            return static_cast<uint32_t>(static_cast<int32_t>(bits_to_f32(value))) & 0xFF;
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_U16:
        switch (src) {
        case DT_U8:
        case DT_U16:
        case DT_U32:
            return value & 0xFFFF;
        case DT_I8:
        case DT_I16:
        case DT_I32:
            return static_cast<uint32_t>(static_cast<int32_t>(value)) & 0xFFFF;
        case DT_F32:
            return static_cast<uint32_t>(static_cast<int32_t>(bits_to_f32(value))) & 0xFFFF;
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_U32:
        switch (src) {
        case DT_U8:
        case DT_U16:
        case DT_U32:
            return value;
        case DT_I8:
        case DT_I16:
        case DT_I32:
            return static_cast<uint32_t>(static_cast<int32_t>(value));
        case DT_F32:
            return static_cast<uint32_t>(static_cast<int32_t>(bits_to_f32(value)));
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_I8:
        switch (src) {
        case DT_U8:
            return resulting_value_signed_dest<uint8_t, int8_t>(value);
        case DT_U16:
            return resulting_value_signed_dest<uint16_t, int8_t>(value);
        case DT_U32:
            return resulting_value_signed_dest<uint32_t, int8_t>(value);
        case DT_I8:
            return resulting_value_signed_dest<int8_t, int8_t>(value);
        case DT_I16:
            return resulting_value_signed_dest<int16_t, int8_t>(value);
        case DT_I32:
            return resulting_value_signed_dest<int32_t, int8_t>(value);
        case DT_F32:
            return resulting_value_signed_dest<int32_t, int8_t>(static_cast<int32_t>(bits_to_f32(value)));
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_I16:
        switch (src) {
        case DT_U8:
            return resulting_value_signed_dest<uint8_t, int16_t>(value);
        case DT_U16:
            return resulting_value_signed_dest<uint16_t, int16_t>(value);
        case DT_U32:
            return resulting_value_signed_dest<uint32_t, int16_t>(value);
        case DT_I8:
            return resulting_value_signed_dest<int8_t, int16_t>(value);
        case DT_I16:
            return resulting_value_signed_dest<int16_t, int16_t>(value);
        case DT_I32:
            return resulting_value_signed_dest<int32_t, int16_t>(value);
        case DT_F32:
            return resulting_value_signed_dest<int32_t, int16_t>(static_cast<int32_t>(bits_to_f32(value)));
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_I32:
        switch (src) {
        case DT_U8:
            return resulting_value_signed_dest<uint8_t, int32_t>(value);
        case DT_U16:
            return resulting_value_signed_dest<uint16_t, int32_t>(value);
        case DT_U32:
            return resulting_value_signed_dest<uint32_t, int32_t>(value);
        case DT_I8:
            return resulting_value_signed_dest<int8_t, int32_t>(value);
        case DT_I16:
            return resulting_value_signed_dest<int16_t, int32_t>(value);
        case DT_I32:
            return resulting_value_signed_dest<int32_t, int32_t>(value);
        case DT_F32:
            return resulting_value_signed_dest<int32_t, int32_t>(static_cast<int32_t>(bits_to_f32(value)));
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    case DT_F32:
        switch (src) {
        case DT_U8:
            return f32_to_bits(static_cast<float>(static_cast<uint8_t>(value)));
        case DT_U16:
            return f32_to_bits(static_cast<float>(static_cast<uint16_t>(value)));
        case DT_U32:
            return f32_to_bits(static_cast<float>(static_cast<uint32_t>(value)));
        case DT_I8:
            return f32_to_bits(static_cast<float>(static_cast<int8_t>(value)));
        case DT_I16:
            return f32_to_bits(static_cast<float>(static_cast<int16_t>(value)));
        case DT_I32:
            return f32_to_bits(static_cast<float>(static_cast<int32_t>(value)));
        case DT_F32:
            return value;
        default:
            throw ProcessorException("unknown data type");
        }
        break;
    default:
        throw ProcessorException("unknown data type");
    }
}

/// REGISTERS

RegisterValue::RegisterValue(uint8_t reg, DataType dt)
    : reg(reg),
      dt(dt) {}

word_t Registers::get(size_t i) const { return registers[i]; }

word_t Registers::get(const RegisterValue& r) const { return registers[r.reg]; }

void Registers::set(size_t i, word_t val) { registers[i] = val; }

void Registers::set(const RegisterValue& r, word_t val) { registers[r.reg] = val; }

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

    RegisterValue get_arg0_register() const { return reg_from_arg(get_arg0()); }
    RegisterValue get_arg1_register() const { return reg_from_arg(get_arg1()); }
    RegisterValue get_arg2_register() const { return reg_from_arg(get_arg2()); }

    uint16_t imm_unsigned() const { return (static_cast<uint16_t>(get_arg1()) << 8) | static_cast<uint16_t>(get_arg2()); }

    int16_t imm_signed() const { return static_cast<int16_t>(imm_unsigned()); }

private:
    static RegisterValue reg_from_arg(uint8_t val) { return RegisterValue(val & 0x1F, static_cast<DataType>((val >> 5) & 7)); }
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

struct OperationValue {
    word_t value;
    bool carry;

    OperationValue()
        : value(0),
          carry(false) {}

    OperationValue(word_t value, bool carry = false)
        : value(value),
          carry(carry) {}
};

template <typename T>
static T dt_swap_bytes(T val);

template <>
int8_t dt_swap_bytes<int8_t>(int8_t val) {
    return val;
}

template <>
uint8_t dt_swap_bytes<uint8_t>(uint8_t val) {
    return val;
}

template <>
int16_t dt_swap_bytes<int16_t>(int16_t val) {
    return __builtin_bswap16(val);
}

template <>
uint16_t dt_swap_bytes<uint16_t>(uint16_t val) {
    return __builtin_bswap16(val);
}

template <>
int32_t dt_swap_bytes<int32_t>(int32_t val) {
    return __builtin_bswap32(val);
}

template <>
uint32_t dt_swap_bytes<uint32_t>(uint32_t val) {
    return __builtin_bswap32(val);
}

struct ArithmaticOperationsBase {
    virtual OperationValue add(word_t a, word_t b) = 0;
    virtual OperationValue sub(word_t a, word_t b) = 0;
    virtual OperationValue mul(word_t a, word_t b) = 0;
    virtual OperationValue div(word_t a, word_t b) = 0;
    virtual OperationValue rem(word_t a, word_t b) = 0;
    virtual OperationValue neg(word_t a) = 0;
};

struct BitwiseOperationsBase {
    virtual OperationValue band(word_t a, word_t b) = 0;
    virtual OperationValue bor(word_t a, word_t b) = 0;
    virtual OperationValue bxor(word_t a, word_t b) = 0;
    virtual OperationValue bsftr(word_t a, word_t b) = 0;
    virtual OperationValue bsftl(word_t a, word_t b) = 0;
    virtual OperationValue bnot(word_t a) = 0;
    virtual OperationValue bswap(word_t a) = 0;
};

struct RelationalOperationsBase {
    virtual bool gt(word_t a, word_t b) = 0;
    virtual bool geq(word_t a, word_t b) = 0;
    virtual bool lt(word_t a, word_t b) = 0;
    virtual bool leq(word_t a, word_t b) = 0;
    virtual bool eq(word_t a, word_t b) = 0;
    virtual bool neq(word_t a, word_t b) = 0;
};

template <typename T> struct TypeOperations : public ArithmaticOperationsBase, public RelationalOperationsBase, public BitwiseOperationsBase {
    static word_t to_word(T res) { return static_cast<uint32_t>(static_cast<int32_t>(res)); }

    OperationValue add(word_t a, word_t b) {
        T res;
        bool carry;
#ifdef USE_OVERFLOW_FUNC
        carry = __builtin_add_overflow(static_cast<T>(a), static_cast<T>(b), &res);
#else
        res = static_cast<T>(a) + static_cast<T>(b);
        carry = false;
#endif
        return OperationValue(to_word(res), carry);
    }

    OperationValue sub(word_t a, word_t b) {
        T res;
        bool carry;
#ifdef USE_OVERFLOW_FUNC
        carry = __builtin_sub_overflow(static_cast<T>(a), static_cast<T>(b), &res);
#else
        res = static_cast<T>(a) - static_cast<T>(b);
        carry = false;
#endif
        return OperationValue(to_word(res), carry);
    }

    OperationValue mul(word_t a, word_t b) {
        T res;
        bool carry;
#ifdef USE_OVERFLOW_FUNC
        carry = __builtin_mul_overflow(static_cast<T>(a), static_cast<T>(b), &res);
#else
        res = static_cast<T>(a) * static_cast<T>(b);
        carry = false;
#endif
        return OperationValue(to_word(res), carry);
    }

    OperationValue div(word_t a, word_t b) {
        word_t res = to_word(static_cast<T>(a) / static_cast<T>(b));
        return OperationValue(res, false);
    }

    OperationValue rem(word_t a, word_t b) {
        word_t res = to_word(static_cast<T>(a) % static_cast<T>(b));
        return OperationValue(res, false);
    }

    OperationValue neg(word_t a) {
        word_t res = to_word(-static_cast<T>(a));
        return OperationValue(res, false);
    }

    OperationValue band(word_t a, word_t b) {
        return OperationValue(static_cast<T>(a) & static_cast<T>(b));
    }

    OperationValue bor(word_t a, word_t b) {
        return OperationValue(static_cast<T>(a) | static_cast<T>(b));
    }

    OperationValue bxor(word_t a, word_t b) {
        return OperationValue(static_cast<T>(a) ^ static_cast<T>(b));
    }

    OperationValue bsftr(word_t a, word_t b) {
        return OperationValue(static_cast<T>(a) >> b, false); // TODO
    }

    OperationValue bsftl(word_t a, word_t b) {
        return OperationValue(static_cast<T>(a) << b, false); // TODO
    }

    OperationValue bnot(word_t a) {
        return OperationValue(to_word(~static_cast<T>(a)));
    }

    OperationValue bswap(word_t a) {
        return OperationValue(to_word(dt_swap_bytes(static_cast<T>(a))));
    }

    bool gt(word_t a, word_t b) { return static_cast<T>(a) > static_cast<T>(b); }

    bool geq(word_t a, word_t b) { return static_cast<T>(a) >= static_cast<T>(b); }

    bool lt(word_t a, word_t b) { return static_cast<T>(a) < static_cast<T>(b); }

    bool leq(word_t a, word_t b) { return static_cast<T>(a) <= static_cast<T>(b); }

    bool eq(word_t a, word_t b) { return static_cast<T>(a) == static_cast<T>(b); }

    bool neq(word_t a, word_t b) { return static_cast<T>(a) != static_cast<T>(b); }
};

template <> struct TypeOperations<float> : public ArithmaticOperationsBase, public RelationalOperationsBase {
    OperationValue add(word_t a, word_t b) {
        word_t res = f32_to_bits(bits_to_f32(a) + bits_to_f32(b));
        return OperationValue(res);
    }

    OperationValue sub(word_t a, word_t b) {
        word_t res = f32_to_bits(bits_to_f32(a) - bits_to_f32(b));
        return OperationValue(res);
    }

    OperationValue mul(word_t a, word_t b) {
        word_t res = f32_to_bits(bits_to_f32(a) * bits_to_f32(b));
        return OperationValue(res);
    }

    OperationValue div(word_t a, word_t b) {
        word_t res = f32_to_bits(bits_to_f32(a) / bits_to_f32(b));
        return OperationValue(res);
    }

    OperationValue rem(word_t a, word_t b) {
        word_t res = f32_to_bits(fmod(bits_to_f32(a), bits_to_f32(b)));
        return OperationValue(res);
    }

    OperationValue neg(word_t a) {
        word_t res = f32_to_bits(-bits_to_f32(a));
        return OperationValue(res);
    }

    bool gt(word_t a, word_t b) { return bits_to_f32(a) > bits_to_f32(b); }

    bool geq(word_t a, word_t b) { return bits_to_f32(a) >= bits_to_f32(b); }

    bool lt(word_t a, word_t b) { return bits_to_f32(a) < bits_to_f32(b); }

    bool leq(word_t a, word_t b) { return bits_to_f32(a) <= bits_to_f32(b); }

    bool eq(word_t a, word_t b) { return bits_to_f32(a) == bits_to_f32(b); }

    bool neq(word_t a, word_t b) { return bits_to_f32(a) != bits_to_f32(b); }
};

static TypeOperations<uint8_t> OPERS_U8;
static TypeOperations<uint16_t> OPERS_U16;
static TypeOperations<uint32_t> OPERS_U32;
static TypeOperations<int8_t> OPERS_I8;
static TypeOperations<int16_t> OPERS_I16;
static TypeOperations<int32_t> OPERS_I32;
static TypeOperations<float> OPERS_F32;

void Processor::step() {
    word_t pc = memory.get_u32(registers.get(Registers::REG_PC));
    const Instruction inst(pc);
    const uint8_t op = inst.get_opcode();
    int32_t jump_val = 1;

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
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0_register()));
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
        stack_push(registers.get(inst.get_arg0_register()));
        break;
    case OP_CPU_POP:
        stack_pop();
        break;
    case OP_CPU_POP_REG: {
        word_t val = stack_pop();
        registers.set(inst.get_arg0_register(), val);
    } break;
    case OP_CPU_JUMP:
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0_register()));
        jump_val = 0;
        break;
    case OP_CPU_JUMP_REL:
        registers.set(Registers::REG_PC, registers.get(inst.get_arg0_register()) + pc);
        break;
    case OP_CPU_JUMP_REL_IMM:
        registers.set(Registers::REG_PC, pc + inst.imm_signed());
        break;
    case OP_LOGIC_NOT:
        registers.set(inst.get_arg0_register(), (registers.get(inst.get_arg1_register()) != 0) ? 0 : 1);
        break;
    case OP_LOGIC_BOOL:
        registers.set(inst.get_arg0_register(), (registers.get(inst.get_arg1_register()) != 0) ? 0 : 1);
        break;
    case OP_LOGIC_TEST_ZERO:
        jump_val = (registers.get(inst.get_arg0_register()) == 0) ? 1 : 2;
        break;
    case OP_LOGIC_TEST_NOT_ZERO:
        jump_val = (registers.get(inst.get_arg0_register()) != 0) ? 1 : 2;
        break;
    case OP_MEM_LOAD_IMM: {
        RegisterValue reg = inst.get_arg0_register();
        switch (reg.dt) {
        case DT_I16:
            registers.set(reg, static_cast<uint32_t>(inst.imm_signed()));
            break;
        case DT_U16:
            registers.set(reg, static_cast<uint32_t>(inst.imm_unsigned()));
            break;
        default:
            throw ProcessorException("unknown immediate data type provided");
        }
    } break;
    case OP_MEM_LOAD:
    case OP_MEM_LOAD_REL:
    case OP_MEM_LOAD_IMM_REL:
    case OP_MEM_LOAD_NEXT:
    case OP_MEM_LOAD_NEXT_OFFSET: {
        RegisterValue reg_target = inst.get_arg0_register();
        word_t addr;
        switch (op) {
        case OP_MEM_LOAD:
            addr = registers.get(inst.get_arg1_register());
            break;
        case OP_MEM_LOAD_REL:
            addr = registers.get(inst.get_arg1_register()) + pc;
            break;
        case OP_MEM_LOAD_IMM_REL:
            addr = pc + inst.imm_signed();
            break;
        case OP_MEM_LOAD_NEXT:
            addr = pc + sizeof(word_t);
            jump_val = 2;
            break;
        case OP_MEM_LOAD_NEXT_OFFSET:
            addr = pc + sizeof(word_t) + registers.get(Registers::REG_LDO);
            break;
        default:
            throw ProcessorException("unknown load instruction found");
        }

        if (data_type_is_integral(reg_target.dt)) {
            if (data_type_is_signed(reg_target.dt)) {
                switch (data_type_byte_size(reg_target.dt)) {
                case 1:
                    registers.set(reg_target, static_cast<uint32_t>(static_cast<int32_t>(static_cast<int8_t>(memory.get_u8(addr)))));
                    break;
                case 2:
                    registers.set(reg_target, static_cast<uint32_t>(static_cast<int32_t>(static_cast<int16_t>(memory.get_u16(addr)))));
                    break;
                case 4:
                    registers.set(reg_target, memory.get_u32(addr));
                    break;
                default:
                    throw ProcessorException("unknown signed byte size");
                }
            } else {
                switch (data_type_byte_size(reg_target.dt)) {
                case 1:
                    registers.set(reg_target, memory.get_u8(addr));
                    break;
                case 2:
                    registers.set(reg_target, memory.get_u16(addr));
                    break;
                case 4:
                    registers.set(reg_target, memory.get_u32(addr));
                    break;
                default:
                    throw ProcessorException("unknown unsigned byte size");
                }
            }
        } else {
            throw ProcessorException("unable to load non-integral data");
        }

    } break;
    case OP_MEM_SAVE:
    case OP_MEM_SAVE_REL: {
        RegisterValue reg_target = inst.get_arg0_register();
        word_t val_src = registers.get(inst.get_arg1_register());

        word_t addr;
        switch (op) {
        case OP_MEM_SAVE_REL:
            addr = inst.imm_signed() + registers.get(reg_target);
            break;
        default:
            addr = registers.get(reg_target);
            break;
        }

        switch (data_type_byte_size(reg_target.dt)) {
        case 1:
            memory.set_u8(addr, static_cast<uint8_t>(val_src & 0xFF));
            break;
        case 2:
            memory.set_u16(addr, static_cast<uint16_t>(val_src & 0xFFFF));
            break;
        case 4:
            memory.set_u32(addr, val_src);
            break;
        default:
            throw ProcessorException("unknown save data type size");
        }
    } break;
    case OP_MEM_COPY:
        registers.set(inst.get_arg0_register(), registers.get(inst.get_arg1_register()));
        break;
    case OP_MEM_CONV: {
        const RegisterValue r0 = inst.get_arg0_register();
        const RegisterValue r1 = inst.get_arg1_register();
        registers.set(r0, data_type_convert(registers.get(r1), r1.dt, r0.dt));
    } break;

    case OP_MATH_ADD:
    case OP_MATH_SUB:
    case OP_MATH_MUL:
    case OP_MATH_DIV:
    case OP_MATH_REM:
    case OP_MATH_NEG: {
        RegisterValue reg = inst.get_arg0_register();

        ArithmaticOperationsBase* arith;
        switch (reg.dt) {
        case DT_U8:
            arith = &OPERS_U8;
            break;
        case DT_U16:
            arith = &OPERS_U16;
            break;
        case DT_U32:
            arith = &OPERS_U32;
            break;
        case DT_I8:
            arith = &OPERS_I8;
            break;
        case DT_I16:
            arith = &OPERS_I16;
            break;
        case DT_I32:
            arith = &OPERS_I32;
            break;
        case DT_F32:
            arith = &OPERS_F32;
            break;
        default:
            throw ProcessorException("unknown data type");
        }

        word_t val_a = registers.get(inst.get_arg1_register());
        word_t val_b = registers.get(inst.get_arg2_register());

        OperationValue res;
        switch (op) {
        case OP_MATH_ADD:
            res = arith->add(val_a, val_b);
            break;
        case OP_MATH_SUB:
            res = arith->sub(val_a, val_b);
            break;
        case OP_MATH_MUL:
            res = arith->mul(val_a, val_b);
            break;
        case OP_MATH_DIV:
            res = arith->div(val_a, val_b);
            break;
        case OP_MATH_REM:
            res = arith->rem(val_a, val_b);
            break;
        case OP_MATH_NEG:
            res = arith->neg(val_a);
            break;
        default:
            throw ProcessorException("unknown instruction");
        }

        registers.set(reg, res.value);
        StatusFlags sf = registers.get_flags();
        sf.set_flag(StatusFlags::FLAG_CARRY, res.carry);
        registers.set_flags(sf);
    } break;

    case OP_BITS_BAND:
    case OP_BITS_BOR:
    case OP_BITS_BXOR:
    case OP_BITS_BSHL:
    case OP_BITS_BSHR:
    case OP_BITS_BNOT:
   case OP_BITS_BSWAP: {
        RegisterValue reg = inst.get_arg0_register();

        BitwiseOperationsBase* opers;
        switch (reg.dt) {
        case DT_U8:
            opers = &OPERS_U8;
            break;
        case DT_U16:
            opers = &OPERS_U16;
            break;
        case DT_U32:
            opers = &OPERS_U32;
            break;
        case DT_I8:
            opers = &OPERS_I8;
            break;
        case DT_I16:
            opers = &OPERS_I16;
            break;
        case DT_I32:
            opers = &OPERS_I32;
            break;
        case DT_F32:
        throw ProcessorException("unsupported data type");
        default:
            throw ProcessorException("unknown data type");
        }

        word_t val_a = registers.get(inst.get_arg1_register());
        word_t val_b = registers.get(inst.get_arg2_register());

        OperationValue res;
        switch (op) {
        case OP_BITS_BAND:
            res = opers->band(val_a, val_b);
            break;
        case OP_BITS_BOR:
            res = opers->bor(val_a, val_b);
            break;
        case OP_BITS_BXOR:
            res = opers->bxor(val_a, val_b);
            break;
        case OP_BITS_BSHL:
            res = opers->bsftl(val_a, val_b);
            break;
        case OP_BITS_BSHR:
            res = opers->bsftr(val_a, val_b);
            break;
        case OP_BITS_BNOT:
            res = opers->bnot(val_a);
            break;
            case OP_BITS_BSWAP:
                res = opers->bswap(val_a);
                break;
        default:
            throw ProcessorException("unknown instruction");
        }

        registers.set(reg, res.value);
        StatusFlags sf = registers.get_flags();
        sf.set_flag(StatusFlags::FLAG_CARRY, res.carry);
        registers.set_flags(sf);
    } break;

    case OP_TEST_EQ:
    case OP_TEST_NEQ:
    case OP_TEST_GREATER:
    case OP_TEST_GREATER_EQ:
    case OP_TEST_LESS:
    case OP_TEST_LESS_EQ: {
        RegisterValue reg = inst.get_arg0_register();

        RelationalOperationsBase* arith;
        switch (reg.dt) {
        case DT_U8:
            arith = &OPERS_U8;
            break;
        case DT_U16:
            arith = &OPERS_U16;
            break;
        case DT_U32:
            arith = &OPERS_U32;
            break;
        case DT_I8:
            arith = &OPERS_I8;
            break;
        case DT_I16:
            arith = &OPERS_I16;
            break;
        case DT_I32:
            arith = &OPERS_I32;
            break;
        case DT_F32:
            arith = &OPERS_F32;
            break;
        default:
            throw ProcessorException("unknown data type");
        }

        word_t val_a = registers.get(inst.get_arg1_register());
        word_t val_b = registers.get(inst.get_arg2_register());

        bool res;
        switch (op) {
        case OP_TEST_EQ:
            res = arith->eq(val_a, val_b);
            break;
        case OP_TEST_NEQ:
            res = arith->neq(val_a, val_b);
            break;
        case OP_TEST_GREATER:
            res = arith->gt(val_a, val_b);
            break;
        case OP_TEST_GREATER_EQ:
            res = arith->geq(val_a, val_b);
            break;
        case OP_TEST_LESS:
            res = arith->lt(val_a, val_b);
            break;
        case OP_TEST_LESS_EQ:
            res = arith->leq(val_a, val_b);
            break;
        default:
            throw ProcessorException("unknown instruction");
        }

        registers.set(reg, res ? 1 : 0);
    } break;

    default:
        throw "error value";
    }

    if (jump_val != 0) {
        jump_val *= sizeof(word_t);
#ifdef USE_OVERFLOW_FUNC
        __builtin_add_overflow(pc, jump_val, &pc);
#else
        pc += jump_val;
#endif

        registers.set(Registers::REG_PC, pc);
    }
}

}
