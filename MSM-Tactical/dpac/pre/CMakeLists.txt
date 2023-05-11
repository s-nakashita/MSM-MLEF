cmake_minimum_required(VERSION 3.16)

#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)

add_executable(addprtb addprtb.f90)
target_link_libraries(addprtb common modobs)
add_executable(addprtbbase addprtbbase.f90)
target_link_libraries(addprtbbase common modobs)
add_executable(replacedate replacedate.f90)
target_link_libraries(replacedate common modobs)
include_directories(${CMAKE_BINARY_DIR}/mod/common
	${CMAKE_BINARY_DIR}/mod/obs)
