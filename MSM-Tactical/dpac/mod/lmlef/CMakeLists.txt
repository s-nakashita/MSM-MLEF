cmake_minimum_required(VERSION 3.16)

file(GLOB_RECURSE sources *.f90 *.f)
add_library(modlmlef STATIC ${sources})
target_link_libraries(modlmlef PUBLIC common param parallel modobs)
include_directories(${CMAKE_BINARY_DIR}/mod/common
	${CMAKE_BINARY_DIR}/mod/param
	${CMAKE_BINARY_DIR}/mod/parallel
	${CMAKE_BINARY_DIR}/mod/obs)
