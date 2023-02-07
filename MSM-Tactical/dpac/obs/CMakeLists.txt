cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)

add_executable(decode_dcdf decode_dcdf.f90)
target_link_libraries(decode_dcdf common modobs)
add_executable(obsope_serial obsope_serial.f90)
target_link_libraries(obsope_serial common modobs modlmlef)
add_executable(obsope_parallel obsope_parallel.f90)
target_link_libraries(obsope_parallel common modobs modlmlef)
add_executable(obsmake obsmake.f90)
target_link_libraries(obsmake common modobs modlmlef)
include_directories(${CMAKE_BINARY_DIR}/mod/common
                    ${CMAKE_BINARY_DIR}/mod/obs
                    ${CMAKE_BINARY_DIR}/mod/lmlef)
