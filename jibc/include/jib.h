#pragma once

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <string>
#include <vector>

namespace jib {

typedef uint32_t word_t;

static const size_t NUM_REGISTERS = 32;

enum DataType { DT_U8 = 1, DT_I8 = 2, DT_U16 = 3, DT_I16 = 4, DT_U32 = 5, DT_I32 = 6, DT_F32 = 7 };

enum DeviceType { DEV_NONE = 0, DEV_SERIAL = 1, DEV_IRQ_CLOCK = 2, DEV_RTC_CLOCK = 3, DEV_RTC_TIMER = 4, DEV_BLOCK = 5 };

class ProcessorException {
public:
    virtual ~ProcessorException();
};

class StackUnderflow : public ProcessorException {};

class InterruptException : public ProcessorException {
public:
    InterruptException(size_t irq);

    size_t irq;
};

class InstructionException : public ProcessorException {
public:
    InstructionException(word_t inst);

    word_t inst;
};

class UnknownInstructionException : public InstructionException {
public:
    UnknownInstructionException(word_t inst);
};

class DataTypeException : public InstructionException {
public:
    DataTypeException(word_t inst, DataType dt);

    DataType dt;
};

class UnknownException : public ProcessorException {
public:
    UnknownException(const std::string& msg);

    virtual const char* what() const;

protected:
    std::string msg;
};

enum MemoryExceptionType { MemFaultInvalidAddress, MemFaultReadOnlyMemory, MemFaultNoSegment };

class MemoryException : public ProcessorException {
public:
    MemoryException(MemoryExceptionType status, word_t addr);
    MemoryExceptionType status;
    word_t addr;
};

struct StatusFlags {
    word_t val;

    StatusFlags(word_t val);

    bool get_flag(word_t flag) const;
    void set_flag(word_t flag, bool value);

    void set_executing_interrupt(size_t ind);
    word_t get_executing_interrupt() const;

    static const word_t FLAG_INTERRUPT_ENABLED;
    static const word_t FLAG_INTERRUPT_EXECUTING;
    static const word_t VAL_INTERRUPT_BASE;
    static const word_t VAL_INTERRUPT_MASK;
    static const word_t FLAG_CARRY;
};

size_t data_type_byte_size(DataType dt);
bool data_type_is_signed(DataType dt);
bool data_type_is_integral(DataType dt);

class MemorySegment {
public:
    virtual ~MemorySegment();

    virtual size_t size() const = 0;

    virtual uint8_t get_u8(word_t addr);
    virtual uint16_t get_u16(word_t addr);
    virtual uint32_t get_u32(word_t addr);

    virtual uint8_t peek_u8(word_t addr) const = 0;
    virtual uint16_t peek_u16(word_t addr) const;
    virtual uint32_t peek_u32(word_t addr) const;

    virtual void set_u8(word_t addr, uint8_t val) = 0;
    virtual void set_u16(word_t addr, uint16_t val);
    virtual void set_u32(word_t addr, uint32_t val);

    virtual void reset() = 0;
};

class ReadWriteMemorySegment : public MemorySegment {
    const size_t _size;
    uint8_t* const _data;

public:
    ReadWriteMemorySegment(size_t size);
    ~ReadWriteMemorySegment();

    size_t size() const;

    uint8_t peek_u8(word_t addr) const;
    uint16_t peek_u16(word_t addr) const;
    uint32_t peek_u32(word_t addr) const;

    void set_u8(word_t addr, uint8_t val);
    void set_u16(word_t addr, uint16_t val);
    void set_u32(word_t addr, uint32_t val);

    void reset();
};

class ReadOnlyMemorySegment : public MemorySegment {
    const size_t _size;
    const uint8_t* const _data;

public:
    ReadOnlyMemorySegment(const uint8_t* data, size_t size);

    size_t size() const;

    uint8_t peek_u8(word_t addr) const;
    uint16_t peek_u16(word_t addr) const;
    uint32_t peek_u32(word_t addr) const;

    void set_u8(word_t addr, uint8_t val);
    void set_u16(word_t addr, uint16_t val);
    void set_u32(word_t addr, uint32_t val);

    void reset();
};

class MemoryMap {
    struct SegmentInfo {
        MemorySegment* segment;
        word_t base;
        word_t top;

        bool contains(word_t addr) const;
    };

    std::vector<SegmentInfo> segments;
    SegmentInfo* last_segment;

public:
    MemoryMap();

    uint8_t get_u8(word_t addr);
    uint16_t get_u16(word_t addr);
    uint32_t get_u32(word_t addr);

    uint8_t peek_u8(word_t addr) const;
    uint16_t peek_u16(word_t addr) const;
    uint32_t peek_u32(word_t addr) const;

    void set_u8(word_t addr, uint8_t val);
    void set_u16(word_t addr, uint16_t val);
    void set_u32(word_t addr, uint32_t val);

    void reset();

private:
    const SegmentInfo& get_segment(word_t addr) const;
};

struct DeviceAction {
    size_t interrupt_num;
    bool call_interrupt;

    DeviceAction()
        : interrupt_num(0),
          call_interrupt(false) {}
};

const size_t DEFAULT_DEVICE_SIZE = 32;

class DeviceBase : public MemorySegment {
public:
    virtual ~DeviceBase() {}
    virtual DeviceAction on_step() {
        DeviceAction act;
        act.call_interrupt = false;
        return act;
    }

    virtual DeviceType device_type() const = 0;

    virtual size_t size() const { return DEFAULT_DEVICE_SIZE; }
};

template <typename T> struct MemoryLinker {
    T& value;
    MemoryLinker(T& value)
        : value(value) {}

    uint8_t get_be_byte(size_t i) const {
        uint8_t arr[sizeof(T)];
        memcpy(&arr, &value, sizeof(T));
        return arr[i];
    }

    void set_be_byte(size_t i, uint8_t x) {
        uint8_t arr[sizeof(T)];
        memcpy(&arr, &value, sizeof(T));
        arr[i] = x;
        memcpy(&value, &arr, sizeof(T));
    }
};

class BlankDevice : public DeviceBase {
public:
    DeviceType device_type() const { return DEV_NONE; }

    uint8_t peek_u8(word_t addr) const {
        if (addr < size()) {
            return 0;
        } else {
            throw MemoryException(MemFaultInvalidAddress, addr);
        }
    }

    void set_u8(word_t addr, uint8_t val) {
        if (addr < size()) {
            (void)val;
        } else {
            throw MemoryException(MemFaultInvalidAddress, addr);
        }
    }
};

struct RegisterValue {
    uint8_t reg;
    DataType dt;

    RegisterValue(uint8_t reg, DataType dt);
};

struct Registers {
    word_t registers[NUM_REGISTERS];

    word_t get(size_t i) const;
    word_t get(const RegisterValue& r) const;
    void set(size_t i, word_t val);
    void set(const RegisterValue& r, word_t val);

    bool interrupt_enabled() const;

    StatusFlags get_flags() const;
    void set_flags(StatusFlags& flags);

    void reset();

    static const size_t REG_PC;
    static const size_t REG_STAT;
    static const size_t REG_SP;
    static const size_t REG_LDO;
    static const size_t REG_RET;
    static const size_t REG_ARG_BASE;
};

class InterruptController {
    uint64_t interrupts;

public:
    InterruptController();

    // Returns >= 0 for a valid interrupt
    int32_t has_interrupt() const;
    void queue_interrupt(size_t num);
    void clear_interrupt(size_t num);

    void reset();

    static const size_t NUM_INTERRUPTS;
    static const size_t NUM_NON_MASKABLE;
};

class Processor {
    MemoryMap memory;
    Registers registers;
    InterruptController interrupts;

public:
    enum ResetType { RESET_HARD, RESET_SOFT };

private:
    word_t get_interrupt_address(size_t interrupt);

    bool call_interrupt(size_t interrupt, uint32_t* data = NULL);

    void stack_push(word_t val);
    word_t stack_pop();

    int32_t get_executing_interrupt() const;

    void push_all_registers();
    void pop_all_registers(bool save_return);

public:
    uint8_t get_current_op();
    word_t get_current_inst();
    word_t get_current_pc() const;

    void reset(ResetType reset = RESET_HARD);

    bool queue_interrupt(size_t interrupt);

    void step();

public:
    static const word_t INTERRUPT_BASE;
};

} // namespace jib
