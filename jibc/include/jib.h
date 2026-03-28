#pragma once

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <string>
#include <vector>

namespace jib {

typedef uint32_t word_t;

static const size_t NUM_REGISTERS = 32;

class ProcessorException {
public:
    ProcessorException(const std::string& msg);
    ~ProcessorException();

    virtual const char* what() const;

protected:
    std::string msg;
};

class MemoryException : public ProcessorException {
public:
    MemoryException(const std::string& msg, word_t addr);

private:
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

struct Registers {
    word_t registers[NUM_REGISTERS];

    word_t get(size_t i) const;
    void set(size_t i, word_t val);

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

class MemorySegment {
    std::vector<uint8_t> data;

public:
    size_t size() const;

    virtual uint8_t get_u8(word_t addr);
    virtual uint16_t get_u16(word_t addr);
    virtual uint32_t get_u32(word_t addr);

    virtual uint8_t peek_u8(word_t addr) const;
    virtual uint16_t peek_u16(word_t addr) const;
    virtual uint32_t peek_u32(word_t addr) const;

    virtual void set_u8(word_t addr, uint8_t val);
    virtual void set_u16(word_t addr, uint16_t val);
    virtual void set_u32(word_t addr, uint32_t val);

    virtual void reset();
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
    ~MemoryMap();

    MemoryMap(const MemoryMap&);
    MemoryMap& operator=(const MemoryMap&);

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

class InterruptController {
    uint64_t interrupts;

    InterruptController();

public:
    // Returns > 0 for a valid interrupt
    int32_t has_interrupt() const;
    void queue_interrupt(size_t num);
    void clear_interrupt(size_t num);

    void reset();

    static const size_t NUM_INTERRUPTS;
    static const size_t NUM_NON_MASKABLE;
};

enum DataType {
    DT_U8 = 1,
    DT_I8 = 2,
    DT_U16 = 3,
    DT_I16 = 4,
    DT_U32 = 5,
    DT_I32 = 6,
    DT_F32 = 7,
};

class Processor {
    MemoryMap memory;
    Registers registers;
    InterruptController interrupts;

public:
    enum ResetType {
        RESET_HARD,
        RESET_SOFT,
    };

private:
    word_t get_interrupt_address(size_t interrupt);

    bool call_interrupt(size_t interrupt, uint32_t* data = NULL);

    void stack_push(word_t val);
    word_t stack_pop();

    void push_all_registers();
    void pop_all_registers(bool save_return);

public:
    uint8_t get_current_op() const;
    uint32_t get_current_pc() const;

    void reset(ResetType reset = RESET_HARD);

    bool queue_interrupt(size_t interrupt);

    void step();

public:
    static const word_t INTERRUPT_BASE;
};

} // namespace jib
