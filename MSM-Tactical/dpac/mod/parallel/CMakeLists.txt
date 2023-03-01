cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)
link_libraries(${LAPACK_LIBRARIES})

file(GLOB_RECURSE sources *.f90)
add_library(parallel STATIC ${sources})
target_link_libraries(parallel common)
#target_compile_options(common PRIVATE -fno-range-check)
include_directories(${CMAKE_BINARY_DIR}/mod/common)