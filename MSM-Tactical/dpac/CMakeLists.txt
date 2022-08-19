cmake_minimum_required(VERSION 3.5)

set(CMAKE_Fortran_COMPILER "/home/nakashita/.local/bin/caf")

project(dpac)
enable_language(Fortran)

find_package(MPI REQUIRED)
message(STATUS "MPI_FOUND=${MPI_FOUND}")

find_package(LAPACK REQUIRED)
message(STATUS "LAPACK_FOUND=${LAPACK_FOUND}")

add_subdirectory(src/common_modules)
#add_subdirectory(src/lmlef)
#add_subdirectory(src/obs)
add_subdirectory(test)
