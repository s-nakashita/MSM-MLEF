cmake_minimum_required(VERSION 3.16)

file(GLOB_RECURSE sources *.F get_cc.f90)
add_library(grmsm_utils STATIC ${sources})
target_link_libraries(grmsm_utils common)
include_directories(${CMAKE_BINARY_DIR}/common)