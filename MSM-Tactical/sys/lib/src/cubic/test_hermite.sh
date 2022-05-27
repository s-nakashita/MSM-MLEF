#!/bin/sh
OPTN=-r8
ncargf77 $OPTN -c cubic_hermite_prep.F && \
ncargf77 $OPTN -c cubic_hermite_intp.F && \
ncargf77 $OPTN -c cubic_hermite_prep_mono.F && \
ncargf77 $OPTN -o test_hermite.x test_cubic_hermite.F cubic_*.o && \
test_hermite.x 
