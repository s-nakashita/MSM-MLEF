#!/bin/sh

##### MAC OSX 
CMPL=ifort
OPTN=-r8
##### others
CMPL=ncargf77
OPTN=-qrealsize=8

$CMPL $OPTN -c cubic_hermite_prep.F && \
$CMPL $OPTN -c cubic_hermite_intp.F && \
$CMPL $OPTN -c vert_cell_cubic_intp.F && \
$CMPL $OPTN -c tri_diag.F && \
$CMPL $OPTN -o test_cell.x test_cubic_cell.F *.o &&  \
test_cell.x  >out
