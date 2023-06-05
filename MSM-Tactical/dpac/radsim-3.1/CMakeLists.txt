cmake_minimum_required(VERSION 3.16)

project(RADSIM LANGUAGES Fortran)
set(CMAKE_Fortran_FLAGS "-O2 -fconvert=big-endian")

#find_package(LAPACK REQUIRED)
#message(STATUS "LAPACK_FOUND=${LAPACK_FOUND}")
#link_directories(${LAPACK_LIBRARY_DIR})

message(STATUS "${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "${CMAKE_CURRENT_BINARY_DIR}")

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../mod/common common)
add_subdirectory(src/code/grmsm_utils)
add_subdirectory(test)
