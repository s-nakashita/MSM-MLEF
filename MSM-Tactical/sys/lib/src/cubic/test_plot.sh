#!/bin/sh

CMPL=ifort
OPTN=-r8
CMPL=ncargf77
OPTN=-qrealsize=8

$CMPL $OPTN -o test_plot.x test_cubic_plot.F *.o &&  \
test_plot.x  >out
