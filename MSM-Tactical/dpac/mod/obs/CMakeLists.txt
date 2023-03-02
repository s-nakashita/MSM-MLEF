cmake_minimum_required(VERSION 3.16)

link_directories(../../../sys/lib)

file(GLOB_RECURSE sources *.f90)
add_library(modobs STATIC ${sources})
target_link_libraries(modobs PUBLIC common param parallel fortran_stdlib::fortran_stdlib w3_4)
include_directories(${CMAKE_BINARY_DIR}/mod/common
	${CMAKE_BINARY_DIR}/mod/param
	${CMAKE_BINARY_DIR}/mod/parallel)
