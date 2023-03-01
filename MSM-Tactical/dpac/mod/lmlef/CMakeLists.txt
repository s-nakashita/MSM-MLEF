cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

file(GLOB_RECURSE sources *.f90 *.f)
add_library(modlmlef STATIC ${sources})
target_link_libraries(modlmlef PUBLIC common parallel modobs)
include_directories(${CMAKE_BINARY_DIR}/mod/common
                    ${CMAKE_BINARY_DIR}/mod/parallel
                    ${CMAKE_BINARY_DIR}/mod/obs)
