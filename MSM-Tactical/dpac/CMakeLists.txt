cmake_minimum_required(VERSION 3.16)

#set(CMAKE_Fortran_COMPILER "/home/nakashita/.local/bin/caf")
#set(CMAKE_Fortran_COMPILER "/home/nakashita/.local/bin/gfortran")

project(dpac LANGUAGES Fortran)
#set(CMAKE_Fortran_FLAGS "-O2 -fconvert=big-endian")
#set(CMAKE_Fortran_PREPROCESS ON)

find_package(MPI REQUIRED)
message(STATUS "MPI_FOUND=${MPI_FOUND}")

find_package(LAPACK REQUIRED)
message(STATUS "LAPACK_FOUND=${LAPACK_FOUND}")

find_package(fortran_stdlib REQUIRED)
message(STATUS "fortran_stdlib_FOUND=${fortran_stdlib_FOUND}")

add_subdirectory(mod/common)
add_subdirectory(mod/param)
add_subdirectory(mod/parallel)
add_subdirectory(mod/lmlef)
add_subdirectory(mod/obs)
add_subdirectory(obs)
add_subdirectory(lmlef)
add_subdirectory(post)
add_subdirectory(pre)
#add_subdirectory(test)
