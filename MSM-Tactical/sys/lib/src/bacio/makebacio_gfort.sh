#!/bin/sh
#
#     Remove make file, if it exists.  May need a new make file
#
if [ -f make.bacio ] 
then
  rm -f make.bacio
fi
#
#     Generate a make file ( make.bacio) from this HERE file.
#
cat > make.bacio << EOF
SHELL=/bin/sh

\${LIB}:	 bacio.v1.3.o baciof.o 

bacio.v1.3.o:       bacio.v1.3.c \${INC}
	ln -f \${INC} clib.h
	gcc -c \${CFLAGS} bacio.v1.3.c
	ar -rv \${LIB} bacio.v1.3.o
	rm clib.h

baciof.o:   baciof.f
	gfortran -c \${FFLAGS} baciof.f
	ar -rv \${LIB} baciof.o
	rm -f baciof.o

EOF
#
#     Update 4-byte version of libbacio_4.a
#
export LIB="../../libbacio_4.a"
export INC="clib4.h"
export FFLAGS=" -fconvert=big-endian "
export CFLAGS="-g -m32 -DLINUX"
make -f make.bacio
#
#     Update 8-byte version of libbacio_8.a
#
export LIB="../../libbacio_8.a"
export INC="clib8.h"
export FFLAGS=" -fdefault-integer-8 -fdefault-real-8 -fconvert=big-endian "
export CFLAGS=" -g -m32 -DLINUX"
make -f make.bacio

rm -f make.bacio
