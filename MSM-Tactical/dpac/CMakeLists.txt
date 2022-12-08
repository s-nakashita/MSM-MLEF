cmake_minimum_required(VERSION 3.5)

project(dummy NONE)
set(CMAKE_Fortran_COMPILER "/home/nakashita/.local/bin/caf")
#set(CMAKE_Fortran_COMPILER "/home/nakashita/.local/bin/gfortran")

project(dpac)
enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "-O2 -fconvert=big-endian")
#set(CMAKE_Fortran_PREPROCESS ON)

find_package(MPI REQUIRED)
message(STATUS "MPI_FOUND=${MPI_FOUND}")

find_package(LAPACK REQUIRED)
message(STATUS "LAPACK_FOUND=${LAPACK_FOUND}")

find_package(PkgConfig REQUIRED)
pkg_check_modules(NETCDF REQUIRED IMPORTED_TARGET GLOBAL netcdf)
include_directories(${NETCDF_INCLUDE_DIRS})
message(STATUS "NETCDF_INCLUDE_DIRS=${NETCDF_INCLUDE_DIRS}")

find_package(fortran_stdlib REQUIRED)
message(STATUS "fortran_stdlib_FOUND=${fortran_stdlib_FOUND}")

add_subdirectory(mod/common)
#add_subdirectory(mod/lmlef)
add_subdirectory(mod/obs)
add_subdirectory(obs)
add_subdirectory(test)
