cmake_minimum_required(VERSION 3.16)

project(Goldberg VERSION 1.0)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)

find_package(Catch2 CONFIG REQUIRED)

include_directories(include)

add_compile_definitions(_USE_MATH_DEFINES)

enable_testing()

include(CTest)
include(Catch)

add_library(goldberg src/goldberg.cpp)
