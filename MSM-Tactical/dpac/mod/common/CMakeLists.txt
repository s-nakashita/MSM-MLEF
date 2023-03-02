cmake_minimum_required(VERSION 3.16)

link_libraries(${LAPACK_LIBRARIES})

file(GLOB_RECURSE sources *.f90)
add_library(common STATIC ${sources})
target_link_libraries(common w3_4)
#target_compile_options(common PRIVATE -fno-range-check)
