#!/bin/sh

set -x

CMPL=ifort
OPTN=-r8
CMPL=ncargf77
OPTN=-qrealsize=8

$CMPL $OPTN -c tri_diag.F &&  \
$CMPL $OPTN -o test_plot.x test_ccsm_plot.F *.o &&  \
test_plot.x  >out
