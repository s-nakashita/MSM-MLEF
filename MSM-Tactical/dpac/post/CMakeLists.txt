cmake_minimum_required(VERSION 3.5)

enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2 -fconvert=big-endian")
link_libraries(${LAPACK_LIBRARIES})
link_directories(../../sys/lib)

add_executable(ensmspr ensmspr.f90)
target_link_libraries(ensmspr common)
add_executable(calcte calcte.f90)
target_link_libraries(calcte common)
add_executable(spectra spectra.f90)
target_link_libraries(spectra common)
add_executable(similarity similarity.f90)
target_link_libraries(similarity common)
include_directories(${CMAKE_BINARY_DIR}/mod/common)
