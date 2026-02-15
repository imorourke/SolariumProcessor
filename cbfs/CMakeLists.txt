cmake_minimum_required(VERSION 3.18)
cmake_policy(SET CMP0171 NEW)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(cbfs)

add_executable(
    cbfs
    src/main.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/include/cbfs.h
    )

cmake_path(GET CMAKE_CURRENT_SOURCE_DIR PARENT_PATH PARENT_DIR)

set_target_properties(cbfs PROPERTIES CXX_STANDARD 20)
if ((CMAKE_BUILD_TYPE STREQUAL "Debug") OR (NOT CMAKE_BUILD_TYPE))
    set(CBFSLIB_BIN_DIR "${PARENT_DIR}/target/debug")
    set(CBFS_CARGO_ARGS "")
else()
    set(CBFSLIB_BIN_DIR "${PARENT_DIR}/target/release")
    set(CBFS_RELEASE ON)
    set(CBFS_CARGO_ARGS "--release")
endif()

file(GLOB_RECURSE CBFS_COMPAT_SRC CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/compat/**/*.rs" "${CMAKE_CURRENT_SOURCE_DIR}/compat/*.toml")
file(GLOB_RECURSE CBFS_LIB_SRC CONFIGURE_DEPENDS "${PARENT_DIR}/cbfs-lib/**/*.rs" "${PARENT_DIR}/cbfs-lib/*.toml")

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/include/cbfs.h
    COMMAND cbindgen --output ${CMAKE_CURRENT_BINARY_DIR}/include/cbfs.h
    DEPENDS ${CBFS_COMPAT_SRC}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/compat
    CODEGEN
  )

if (WIN32)
    target_link_libraries(cbfs PUBLIC "C:\\Program Files (x86)\\WinFsp\\lib\\winfsp-x64.lib")
    target_include_directories(cbfs PUBLIC "C:\\Program Files (x86)\\WinFsp\\inc")
    add_custom_target(cbfs_lib DEPENDS "${PARENT_DIR}/target/debug/cbfs_compat.dll.lib" "${PARENT_DIR}/target/release/cbfs_compat.dll.lib" )
    add_custom_command(
        OUTPUT "${PARENT_DIR}/target/release/cbfs_compat.dll.lib"
        COMMAND cargo build --release
        COMMAND ${CMAKE_COMMAND} -E copy "${PARENT_DIR}/target/release/cbfs_compat.dll" ${CMAKE_BINARY_DIR}/Release/cbfs_compat.dll
        COMMAND ${CMAKE_COMMAND} -E copy "C:\\Program Files (x86)\\WinFsp\\bin\\winfsp-x64.dll" "${CMAKE_CURRENT_BINARY_DIR}/Release/winfsp-x64.dll"
        DEPENDS ${CBFS_COMPAT_SRC} ${CBFS_LIB_SRC}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/compat
    )
    add_custom_command(
        OUTPUT "${PARENT_DIR}/target/debug/cbfs_compat.dll.lib"
        COMMAND cargo build
        COMMAND ${CMAKE_COMMAND} -E copy "${PARENT_DIR}/target/debug/cbfs_compat.dll" ${CMAKE_BINARY_DIR}/Debug/cbfs_compat.dll
        COMMAND ${CMAKE_COMMAND} -E copy "C:\\Program Files (x86)\\WinFsp\\bin\\winfsp-x64.dll" "${CMAKE_CURRENT_BINARY_DIR}/Debug/winfsp-x64.dll"
        DEPENDS ${CBFS_COMPAT_SRC} ${CBFS_LIB_SRC}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/compat
    )

    target_link_libraries(cbfs PUBLIC "${PARENT_DIR}/target/release/cbfs_compat.dll.lib" debug "${PARENT_DIR}/target/debug/cbfs_compat.dll.lib")
    add_dependencies(cbfs cbfs_lib)
else()
    if(APPLE)
        set(CBFSLIB_FILE "${CBFSLIB_BIN_DIR}/libcbfs_compat.dylib")
    else()
        set(CBFSLIB_FILE "${CBFSLIB_BIN_DIR}/libcbfs_compat.a")
    endif()

    target_link_libraries(cbfs PUBLIC fuse3)
    target_link_libraries(cbfs PUBLIC "${CBFSLIB_FILE}")
    add_custom_target(cbfs_lib DEPENDS "${CBFSLIB_FILE}")
    add_custom_command(
        OUTPUT "${CBFSLIB_FILE}"
        COMMAND cargo build ${CBFS_CARGO_ARGS}
        DEPENDS ${CBFS_COMPAT_SRC} ${CBFS_LIB_SRC}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/compat
    )

    add_dependencies(cbfs cbfs_lib)

    target_link_libraries(cbfs PUBLIC "${CBFSLIB_FILE}")
endif()

target_compile_definitions(cbfs PUBLIC _FILE_OFFSET_BITS=64 FUSE_USE_VERSION=35)
target_include_directories(cbfs PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/include)

if(CBFS_RELEASE)
    if(MSVC)
        target_compile_options(cbfs PRIVATE /W4 /WX)
    else()
        target_compile_options(cbfs PRIVATE -Wall -Wextra -Wpedantic -Werror)
    endif()
endif()
