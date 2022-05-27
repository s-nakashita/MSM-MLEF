#!/bin/sh

ncargf77 -qrealsize=8 -c cubic_mass_intp.F && \
ncargf77 -qrealsize=8 -c cubic_hermite_intp.F && \
ncargf77 -qrealsize=8 -c quad_diag_solver.F && \
ncargf77 -qrealsize=8 -o test_mass.x test_cubic_mass.F *.o &&  \
test_mass.x 
