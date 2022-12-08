cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

file(GLOB_RECURSE sources *.f90)
add_library(obs STATIC ${sources})
target_link_libraries(obs PUBLIC common fortran_stdlib::fortran_stdlib)
include_directories(${CMAKE_BINARY_DIR}/mod/common_modules)
