cmake_minimum_required(VERSION 3.18)
cmake_policy(SET CMP0171 NEW)

add_library(
    jibc
    src/jib.cpp include/jib.h
    )

target_include_directories(jibc PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")
