#!/bin/sh
set -e
cmake -B build -DCMAKE_Fortran_COMPILER=/home/nakashita/.local/bin/caf \
	-DCMAKE_Fortran_FLAGS="-O2 -fconvert=big-endian"
cd build && gmake
