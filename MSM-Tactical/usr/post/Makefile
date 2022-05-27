FC=gfortran10
LD=$(FC)
FFLAGS=-O2 -fconvert=big-endian
LDFLAGS=
LIBS=

PARENT=${PWD}
SRCS=$(PARENT)/src/read_sig.f90
OBJS=read_sig.o
EXEC=read_sig

$(EXEC):	$(OBJS)
	$(LD) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

$(OBJS):	$(SRCS)
	$(FC) $(FFLAGS) -c $(SRCS)

