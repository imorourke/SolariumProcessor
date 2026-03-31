#include "jib.h"

#include <iostream>

int main() {
    jib::ReadWriteMemorySegment seg1(1024);
    jib::ReadOnlyMemorySegment seg2(0, 1024);

    jib::MemoryMap map;
    jib::Processor proc;

    // map.add_segment(0, &seg1);
    // map.add_segment(1024, &seg2);

    while (true) {
        if (std::cin) {
            char c;
            std::cin >> c;
        }

        proc.step();

        std::cout << "a" << std::endl;
    }

    return 0;
}
