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

\$(LIB):	bacio.v1.3.o baciof.o 

bacio.v1.3.o:       bacio.v1.3.c \$(INC)
	ln -f \$(INC) clib.h
	\$(CC) -c \$(CFLAGS) bacio.v1.3.c
	ar -rv \$(ARFLAGS) \$(LIB) bacio.v1.3.o
	rm clib.h

baciof.o:   baciof.f
	\$(FC) -c \$(FFLAGS) baciof.f
	ar -rv \$(ARFLAGS) \$(LIB) baciof.o
	rm -f baciof.o

EOF
#
#     Update 4-byte version of libbacio_4.a
#
export LIB="../../libbacio_4.a"
export INC="clib4.h"
export FFLAGS=$FFLAGS_4
export CFLAGS=$CFLAGS_R
export ARFLAGS=$ARFLAGS_R
make -f make.bacio
#
#     Update 8-byte version of libbacio_8.a
#
export LIB="../../libbacio_8.a"
export INC="clib8.h"
export FFLAGS=$FFLAGS_8
export CFLAGS=$CFLAGS_R
export ARFLAGS=$ARFLAGS_R
make -f make.bacio

#rm -f make.bacio
