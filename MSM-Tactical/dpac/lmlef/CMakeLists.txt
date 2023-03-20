cmake_minimum_required(VERSION 3.16)

#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)

add_executable(lmlef lmlef.f90)
target_link_libraries(lmlef common param parallel modobs modlmlef)
add_executable(read_info read_info.f90)
target_link_libraries(read_info common param parallel)
#add_executable(test_corr test_corr.f90)
#target_link_libraries(test_corr common param parallel modlmlef)
include_directories(${CMAKE_BINARY_DIR}/mod/common
		    ${CMAKE_BINARY_DIR}/mod/param
		    ${CMAKE_BINARY_DIR}/mod/parallel
                    ${CMAKE_BINARY_DIR}/mod/obs
                    ${CMAKE_BINARY_DIR}/mod/lmlef)
