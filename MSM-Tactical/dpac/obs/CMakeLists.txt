cmake_minimum_required(VERSION 3.16)

#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)
link_directories($ENV{HOME}/.local/lib)

add_executable(decode_dcdf decode_dcdf.f90)
target_link_libraries(decode_dcdf common modobs)
add_executable(decode_prepbufr decode_prepbufr.f90)
target_link_libraries(decode_prepbufr common modobs bufr)
add_executable(obsope_serial obsope_serial.f90)
target_link_libraries(obsope_serial common param modobs modlmlef)
add_executable(obsope_parallel obsope_parallel.f90)
target_link_libraries(obsope_parallel common param parallel modobs modlmlef)
add_executable(obsmake obsmake.f90)
target_link_libraries(obsmake common param parallel modobs modlmlef)
include_directories(${CMAKE_BINARY_DIR}/mod/common
		    ${CMAKE_BINARY_DIR}/mod/param
		    ${CMAKE_BINARY_DIR}/mod/parallel
                    ${CMAKE_BINARY_DIR}/mod/obs
                    ${CMAKE_BINARY_DIR}/mod/lmlef)
