#include "jib.h"

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

/// MEMORY SEGMENT

size_t MemorySegment::size() const { return data.size(); }

uint8_t MemorySegment::get_u8(uint32_t addr) { return peek_u8(addr); }

uint16_t MemorySegment::get_u16(uint32_t addr) { return peek_u16(addr); }

uint32_t MemorySegment::get_u32(uint32_t addr) { return peek_u32(addr); }

uint8_t MemorySegment::peek_u8(uint32_t addr) const {
    if (addr < data.size()) {
        return data[addr];
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

uint16_t MemorySegment::peek_u16(uint32_t addr) const {
    if (addr + sizeof(uint16_t) <= data.size()) {
        uint16_t val;
        memcpy(&val, &data[addr], sizeof(val));
        return be16toh(val);
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

uint32_t MemorySegment::peek_u32(uint32_t addr) const {
    if (addr + sizeof(uint32_t) <= data.size()) {
        uint16_t val;
        memcpy(&val, &data[addr], sizeof(val));
        return be32toh(val);
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

void MemorySegment::set_u8(uint32_t addr, uint8_t val) {
    if (addr < data.size()) {
        data[addr] = val;
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

void MemorySegment::set_u16(uint32_t addr, uint16_t val) {
    if (addr + sizeof(val) <= data.size()) {
        val = htobe16(val);
        memcpy(&data[addr], &val, sizeof(val));
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

void MemorySegment::set_u32(uint32_t addr, uint32_t val) {
    if (addr + sizeof(val) <= data.size()) {
        val = htobe32(val);
        memcpy(&data[addr], &val, sizeof(val));
    } else {
        throw MemoryException("address out of bounds", addr);
    }
}

/// MEMORY MAP

MemoryMap::MemoryMap()
    : last_segment(NULL) {}

MemoryMap::~MemoryMap() {
    for (size_t i = 0; i < segments.size(); ++i) {
        if (segments[i].segment != NULL) {
            delete segments[i].segment;
        }
    }
    segments.clear();
    last_segment = NULL;
}

bool MemoryMap::SegmentInfo::contains(uint32_t addr) const { return addr >= base && addr < base + segment->size(); }

uint8_t MemoryMap::get_u8(uint32_t addr) {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->get_u8(addr - seg.base);
}

uint16_t MemoryMap::get_u16(uint32_t addr) {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->get_u16(addr - seg.base);
}
uint32_t MemoryMap::get_u32(uint32_t addr) {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->get_u32(addr - seg.base);
}

uint8_t MemoryMap::peek_u8(uint32_t addr) const {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->peek_u8(addr - seg.base);
}

uint16_t MemoryMap::peek_u16(uint32_t addr) const {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->peek_u16(addr - seg.base);
}

uint32_t MemoryMap::peek_u32(uint32_t addr) const {
    SegmentInfo seg = get_segment(addr);
    return seg.segment->peek_u32(addr - seg.base);
}

void MemoryMap::set_u8(uint32_t addr, uint8_t val) {
    SegmentInfo seg = get_segment(addr);
    seg.segment->set_u8(addr - seg.base, val);
}
void MemoryMap::set_u16(uint32_t addr, uint16_t val) {
    SegmentInfo seg = get_segment(addr);
    seg.segment->set_u16(addr - seg.base, val);
}

void MemoryMap::set_u32(uint32_t addr, uint32_t val) {
    SegmentInfo seg = get_segment(addr);
    seg.segment->set_u32(addr - seg.base, val);
}

const MemoryMap::SegmentInfo& MemoryMap::get_segment(uint32_t addr) const {
    if (last_segment != NULL && last_segment->contains(addr)) {
        return *last_segment;
    }

    for (size_t i = 0; i < segments.size(); ++i) {
        const SegmentInfo& seg = segments[i];
        if (seg.contains(addr)) {
            return seg;
        }
    }

    throw MemoryException("no segment found", addr);
}

}
