cmake_minimum_required(VERSION 3.5)

project(dummy NONE)
#set(CMAKE_Fortran_COMPILER "/Users/nakashita/.local/bin/caf")
set(CMAKE_Fortran_COMPILER "/usr/local/bin/nagfor")
#set(CMAKE_Fortran_COMPILER "/opt/local/bin/gfortran")

project(dpac)
enable_language(Fortran)
#set(CMAKE_Fortran_FLAGS "-O2 -fconvert=big-endian")
set(CMAKE_Fortran_FLAGS "-O2 -convert=big_endian")
#set(CMAKE_Fortran_PREPROCESS ON)

#find_package(MPI REQUIRED)
#message(STATUS "MPI_FOUND=${MPI_FOUND}")

find_package(LAPACK REQUIRED)
message(STATUS "LAPACK_FOUND=${LAPACK_FOUND}")

find_package(PkgConfig REQUIRED)
pkg_check_modules(NETCDFF REQUIRED IMPORTED_TARGET GLOBAL netcdf-fortran)
include_directories(${NETCDFF_INCLUDE_DIRS})
message(STATUS "NETCDFF_INCLUDE_DIRS=${NETCDFF_INCLUDE_DIRS}")

#find_package(fortran_stdlib REQUIRED)
#message(STATUS "fortran_stdlib_FOUND=${fortran_stdlib_FOUND}")

add_subdirectory(mod/common)
#add_subdirectory(mod/param)
#add_subdirectory(mod/parallel)
#add_subdirectory(mod/lmlef)
#add_subdirectory(mod/obs)
#add_subdirectory(obs)
#add_subdirectory(lmlef)
#add_subdirectory(post)
#add_subdirectory(pre)
add_subdirectory(test)
