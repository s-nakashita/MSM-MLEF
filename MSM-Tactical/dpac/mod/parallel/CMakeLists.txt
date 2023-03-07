cmake_minimum_required(VERSION 3.16)

link_libraries(${LAPACK_LIBRARIES})

file(GLOB_RECURSE sources *.f90)
add_library(parallel STATIC ${sources})
target_link_libraries(parallel PUBLIC common param)
include_directories(${CMAKE_BINARY_DIR}/mod/common
	${CMAKE_BINARY_DIR}/mod/param)
