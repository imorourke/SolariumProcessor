#include "memory_helpers.h"

#ifdef __APPLE__
#include <machine/endian.h>

#define be16toh(x) ntohs(x)
#define be32toh(x) ntohl(x)

#define htobe16(x) htons(x)
#define htobe32(x) htonl(x)

#else
#include <endian.h>
#endif

namespace jib {

int8_t swap_bytes(int8_t val) { return val; }
uint8_t swap_bytes(uint8_t val) { return val; }
int16_t swap_bytes(int16_t val) { return __builtin_bswap16(val); }
uint16_t swap_bytes(uint16_t val) { return __builtin_bswap16(val); }
int32_t swap_bytes(int32_t val) { return __builtin_bswap32(val); }
uint32_t swap_bytes(uint32_t val) { return __builtin_bswap32(val); }

int8_t to_native(int8_t val) { return val; }
uint8_t to_native(uint8_t val) { return val; }
int16_t to_native(int16_t val) { return be16toh(val); }
uint16_t to_native(uint16_t val) { return be16toh(val); }
int32_t to_native(int32_t val) { return be32toh(val); }
uint32_t to_native(uint32_t val) { return be32toh(val); }

int8_t to_be(int8_t val) { return val; }
uint8_t to_be(uint8_t val) { return val; }
int16_t to_be(int16_t val) { return htobe16(val); }
uint16_t to_be(uint16_t val) { return htobe16(val); }
int32_t to_be(int32_t val) { return htobe32(val); }
uint32_t to_be(uint32_t val) { return htobe32(val); }

}
