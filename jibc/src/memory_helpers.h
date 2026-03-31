#pragma once

#include <stdint.h>

namespace jib {

int8_t swap_bytes(int8_t val);
uint8_t swap_bytes(uint8_t val);
int16_t swap_bytes(int16_t val);
uint16_t swap_bytes(uint16_t val);
int32_t swap_bytes(int32_t val);
uint32_t swap_bytes(uint32_t val);

int8_t to_native(int8_t val);
uint8_t to_native(uint8_t val);
int16_t to_native(int16_t val);
uint16_t to_native(uint16_t val);
int32_t to_native(int32_t val);
uint32_t to_native(uint32_t val);

int8_t to_be(int8_t val);
uint8_t to_be(uint8_t val);
int16_t to_be(int16_t val);
uint16_t to_be(uint16_t val);
int32_t to_be(int32_t val);
uint32_t to_be(uint32_t val);

}
