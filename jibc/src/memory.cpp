#include "jib.h"
#include <cstring>

#include "memory_helpers.h"

namespace jib {

// MEMORY EXCEPTION

MemoryException::MemoryException(MemoryExceptionType status, word_t addr)
    : status(status),
      addr(addr) {}

// MEMORY SEGMENT

MemorySegment::~MemorySegment() {}

uint8_t MemorySegment::get_u8(word_t addr) { return peek_u8(addr); }
uint16_t MemorySegment::get_u16(word_t addr) { return peek_u16(addr); }
uint32_t MemorySegment::get_u32(word_t addr) { return peek_u32(addr); }

uint16_t MemorySegment::peek_u16(word_t addr) const {
    uint8_t vals[sizeof(uint16_t)] = {
        peek_u8(addr),
        peek_u8(addr + 1),
    };
    uint16_t mem_val;
    memcpy(&mem_val, &vals, sizeof(uint16_t));
    return mem_val;
}

uint32_t MemorySegment::peek_u32(word_t addr) const {
    uint8_t vals[sizeof(uint32_t)] = {
        peek_u8(addr),
        peek_u8(addr + 1),
        peek_u8(addr + 2),
        peek_u8(addr + 3),
    };
    uint32_t mem_val;
    memcpy(&mem_val, &vals, sizeof(uint32_t));
    return mem_val;
}

void MemorySegment::set_u16(word_t addr, uint16_t val) {
    uint8_t vals[sizeof(uint16_t)];
    memcpy(&vals, &val, sizeof(uint16_t));
    set_u8(addr, vals[0]);
    set_u8(addr + 1, vals[1]);
}

void MemorySegment::set_u32(word_t addr, uint32_t val) {
    uint8_t vals[sizeof(uint32_t)];
    memcpy(&vals, &val, sizeof(uint32_t));
    set_u8(addr, vals[0]);
    set_u8(addr + 1, vals[1]);
    set_u8(addr + 2, vals[2]);
    set_u8(addr + 3, vals[3]);
}

/// READ-WRITE MEMORY SEGMENT

ReadWriteMemorySegment::ReadWriteMemorySegment(size_t size)
    : _size(size),
      _data(new uint8_t[_size]) {}

ReadWriteMemorySegment::~ReadWriteMemorySegment() {
    if (_data != NULL) {
        delete[] _data;
    }
}

size_t ReadWriteMemorySegment::size() const { return _size; }

uint8_t ReadWriteMemorySegment::peek_u8(word_t addr) const {
    if (addr < _size) {
        return _data[addr];
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

uint16_t ReadWriteMemorySegment::peek_u16(word_t addr) const {
    if (addr + sizeof(uint16_t) <= _size) {
        uint16_t val;
        memcpy(&val, &_data[addr], sizeof(val));
        return to_native(val);
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

uint32_t ReadWriteMemorySegment::peek_u32(word_t addr) const {
    if (addr + sizeof(uint32_t) <= _size) {
        uint16_t val;
        memcpy(&val, &_data[addr], sizeof(val));
        return to_native(val);
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

void ReadWriteMemorySegment::set_u8(word_t addr, uint8_t val) {
    if (addr < _size) {
        _data[addr] = val;
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

void ReadWriteMemorySegment::set_u16(word_t addr, uint16_t val) {
    if (addr + sizeof(val) <= _size) {
        val = to_be(val);
        memcpy(&_data[addr], &val, sizeof(val));
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

void ReadWriteMemorySegment::set_u32(word_t addr, uint32_t val) {
    if (addr + sizeof(val) <= _size) {
        val = to_be(val);
        memcpy(&_data[addr], &val, sizeof(val));
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

void ReadWriteMemorySegment::reset() { memset(_data, 0, _size); }

/// READ-ONLY MEMORY SEGMENT

ReadOnlyMemorySegment::ReadOnlyMemorySegment(const uint8_t* data, size_t size)
    : _size(size),
      _data(data) {}

size_t ReadOnlyMemorySegment::size() const { return _size; }

uint8_t ReadOnlyMemorySegment::peek_u8(word_t addr) const {
    if (addr < _size) {
        return _data[addr];
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

uint16_t ReadOnlyMemorySegment::peek_u16(word_t addr) const {
    if (addr + sizeof(uint16_t) <= _size) {
        uint16_t val;
        memcpy(&val, &_data[addr], sizeof(val));
        return to_native(val);
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

uint32_t ReadOnlyMemorySegment::peek_u32(word_t addr) const {
    if (addr + sizeof(uint32_t) <= _size) {
        uint16_t val;
        memcpy(&val, &_data[addr], sizeof(val));
        return to_native(val);
    } else {
        throw MemoryException(MemFaultInvalidAddress, addr);
    }
}

void ReadOnlyMemorySegment::set_u8(word_t addr, uint8_t val) {
    (void)val;
    throw MemoryException(MemFaultReadOnlyMemory, addr);
}

void ReadOnlyMemorySegment::set_u16(word_t addr, uint16_t val) {
    (void)val;
    throw MemoryException(MemFaultReadOnlyMemory, addr);
}

void ReadOnlyMemorySegment::set_u32(word_t addr, uint32_t val) {
    (void)val;
    throw MemoryException(MemFaultReadOnlyMemory, addr);
}

void ReadOnlyMemorySegment::reset() {}

/// MEMORY MAP

MemoryMap::MemoryMap()
    : last_segment(NULL) {}

bool MemoryMap::SegmentInfo::contains(word_t addr) const { return addr >= base && addr < base + segment->size(); }

uint8_t MemoryMap::get_u8(word_t addr) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->get_u8(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

uint16_t MemoryMap::get_u16(word_t addr) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->get_u16(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}
uint32_t MemoryMap::get_u32(word_t addr) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->get_u32(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

uint8_t MemoryMap::peek_u8(word_t addr) const {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->peek_u8(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

uint16_t MemoryMap::peek_u16(word_t addr) const {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->peek_u16(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

uint32_t MemoryMap::peek_u32(word_t addr) const {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->peek_u32(addr - seg.base);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

void MemoryMap::set_u8(word_t addr, uint8_t val) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->set_u8(addr - seg.base, val);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}
void MemoryMap::set_u16(word_t addr, uint16_t val) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->set_u16(addr - seg.base, val);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

void MemoryMap::set_u32(word_t addr, uint32_t val) {
    SegmentInfo seg = get_segment(addr);
    try {
        return seg.segment->set_u32(addr - seg.base, val);
    } catch (const MemoryException& err) {
        throw MemoryException(err.status, err.addr + seg.base);
    }
}

void MemoryMap::reset() {
    for (size_t i = 0; i < segments.size(); ++i) {
        segments[i].segment->reset();
    }
}

const MemoryMap::SegmentInfo& MemoryMap::get_segment(word_t addr) const {
    if (last_segment != NULL && last_segment->contains(addr)) {
        return *last_segment;
    }

    for (size_t i = 0; i < segments.size(); ++i) {
        const SegmentInfo& seg = segments[i];
        if (seg.contains(addr)) {
            return seg;
        }
    }

    throw MemoryException(MemFaultNoSegment, addr);
}

}
