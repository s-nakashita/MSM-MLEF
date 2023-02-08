cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)

add_executable(addprtb addprtb.f90)
target_link_libraries(addprtb common)
add_executable(addprtbbase addprtbbase.f90)
target_link_libraries(addprtbbase common)
add_executable(replacedate replacedate.f90)
target_link_libraries(replacedate common)
include_directories(${CMAKE_BINARY_DIR}/mod/common)
