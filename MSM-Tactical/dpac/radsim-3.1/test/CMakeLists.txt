cmake_minimum_required(VERSION 3.16)

add_executable(test_get_cc test_get_cc.f90)
target_link_libraries(test_get_cc common grmsm_utils)
include_directories(${CMAKE_BINARY_DIR}/src/code/grmsm_utils
                    ${CMAKE_BINARY_DIR}/common)