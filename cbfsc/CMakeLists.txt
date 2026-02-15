cmake_minimum_required(VERSION 3.18)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(cbfsc)

add_executable(
    cbfsc
    src/main.cpp
    )

set_target_properties(cbfsc PROPERTIES CXX_STANDARD 20)
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_link_libraries(cbfsc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../target/debug/libcbfs_lib.a")
else()
    target_link_libraries(cbfsc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/../target/release/libcbfs_lib.a")
endif()

target_link_libraries(cbfsc PUBLIC fuse3)

set(CBFSLIB_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../cbfs-lib)

target_compile_definitions(cbfsc PUBLIC _FILE_OFFSET_BITS=64 FUSE_USE_VERSION=31)
target_include_directories(cbfsc PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include ${CBFSLIB_DIR}/gen)

if(MSVC)
    target_compile_options(cbfsc PRIVATE /W4 /WX)
else()
    target_compile_options(cbfsc PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
