cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)
link_libraries(${LAPACK_LIBRARIES})

file(GLOB_RECURSE sources *.f90)
add_library(common STATIC ${sources})
