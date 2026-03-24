#pragma once

#include <stddef.h>
#include <stdint.h>

namespace jib {

const size_t NUM_REGISTERS = 32;

struct StatusFlags {
  uint32_t val;

  StatusFlags(uint32_t val) : val(val) {}
};

struct Registers {
  uint32_t registers[NUM_REGISTERS];

  const size_t REG_PC = 0;
  const size_t REG_STAT = 1;
  const size_t REG_SP = 2;
  const size_t REG_LDO = 3;
  const size_t REG_RET = 4;
  const size_t REG_BASE = 5;
};

class MemoryMap {
public:
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

enum class DataType : uint32_t {
  U8 = 1,
  I8 = 2,
  U16 = 3,
  I16 = 4,
  U32 = 5,
  I32 = 6,
  F32 = 7,
};

class Instruction {
  uint32_t value;

  Instruction(uint32_t val) : value(val) {}

  uint8_t get_opcode() const { return static_cast<uint8_t>(value & 0xFF); }
};

class Processor {
  MemoryMap memory;
  Registers registers;

public:
  enum class ResetType {
    Hard,
    Soft,
  };

  uint32_t get_reset_vector(uint32_t vector_num) {
    return memory.get_u32(0x100 + sizeof(uint32_t) * vector_num);
  }

  void reset(ResetType reset = ResetType::Hard) {
    const uint32_t vec = get_reset_vector((reset == ResetType::Hard) ? 0 : 1);
  }
};

} // namespace jib
