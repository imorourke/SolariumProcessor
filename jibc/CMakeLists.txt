cmake_minimum_required(VERSION 3.18)
cmake_policy(SET CMP0171 NEW)

add_library(
    jibc
    include/jib.h
    src/jib.cpp
    src/memory.cpp
    )

target_include_directories(jibc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")

if(MSVC)
    target_compile_options(cbfs PRIVATE /W4 /WX)
else()
    target_compile_options(cbfs PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
