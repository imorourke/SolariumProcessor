#pragma once

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <string>
#include <vector>

namespace jib {

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
    MemoryException(const std::string& msg, uint32_t addr);

private:
    uint32_t addr;
};

struct StatusFlags {
    uint32_t val;

    StatusFlags(uint32_t val);

    bool get_flag(uint32_t flag) const;
    StatusFlags with_flag(uint32_t flag, bool value) const;
    void set_flag(uint32_t flag, bool value);

    static const uint32_t FLAG_INTERRUPTS_ENABLED;
};

struct Registers {
    uint32_t registers[NUM_REGISTERS];

    uint32_t get(size_t i) const;

    void set(size_t i, uint32_t val);

    static const size_t REG_PC;
    static const size_t REG_STAT;
    static const size_t REG_SP;
    static const size_t REG_LDO;
    static const size_t REG_RET;
    static const size_t REG_BASE;
};

class MemorySegment {
    std::vector<uint8_t> data;

public:
    size_t size() const;

    uint8_t get_u8(uint32_t addr);
    uint16_t get_u16(uint32_t addr);
    uint32_t get_u32(uint32_t addr);

    uint8_t peek_u8(uint32_t addr) const;
    uint16_t peek_u16(uint32_t addr) const;
    uint32_t peek_u32(uint32_t addr) const;

    void set_u8(uint32_t addr, uint8_t val);
    void set_u16(uint32_t addr, uint16_t val);
    void set_u32(uint32_t addr, uint32_t val);
};

class MemoryMap {
    struct SegmentInfo {
        MemorySegment* segment;
        uint32_t base;
        uint32_t top;

        bool contains(uint32_t addr) const;
    };

    std::vector<SegmentInfo> segments;
    SegmentInfo* last_segment;

public:
    MemoryMap();
    ~MemoryMap();

    MemoryMap(const MemoryMap&);
    MemoryMap& operator=(const MemoryMap&);

    uint8_t get_u8(uint32_t addr);
    uint16_t get_u16(uint32_t addr);
    uint32_t get_u32(uint32_t addr);

    uint8_t peek_u8(uint32_t addr) const;
    uint16_t peek_u16(uint32_t addr) const;
    uint32_t peek_u32(uint32_t addr) const;

    void set_u8(uint32_t addr, uint8_t val);
    void set_u16(uint32_t addr, uint16_t val);
    void set_u32(uint32_t addr, uint32_t val);

private:
    const SegmentInfo& get_segment(uint32_t addr) const;
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

public:
    enum ResetType {
        RESET_HARD,
        RESET_SOFT,
    };

    uint32_t get_reset_vector(uint32_t vector_num);

    void reset(ResetType reset = RESET_HARD);

    bool queue_interrupt(size_t interrupt);

    void step();
};

} // namespace jib
