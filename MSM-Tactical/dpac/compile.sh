#!/bin/sh
set -e
bindir=build
cmake -B $bindir -DCMAKE_Fortran_COMPILER=/home/nakashita/.local/bin/caf \
	-DCMAKE_Fortran_FLAGS="-O2 -fconvert=big-endian"
cd $bindir && gmake
