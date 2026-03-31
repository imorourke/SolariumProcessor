cmake_minimum_required(VERSION 3.18)
cmake_policy(SET CMP0171 NEW)

add_library(
    jibc
    include/jib.h src/jib.cpp
    src/memory.cpp
    src/devices.cpp
    src/memory_helpers.h src/memory_helpers.cpp
    )

target_include_directories(jibc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")
set_target_properties(jibc PROPERTIES CXX_STANDARD 98)

if(MSVC)
    target_compile_options(jibc PRIVATE /W4 /WX)
else()
    target_compile_options(jibc PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

add_executable(
    jibc_test
    bin/main.cpp
    )

set_target_properties(jibc_test PROPERTIES CXX_STANDARD 98)
target_link_libraries(jibc_test PUBLIC jibc)

if(MSVC)
    target_compile_options(jibc_test PRIVATE /W4 /WX)
else()
    target_compile_options(jibc_test PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
