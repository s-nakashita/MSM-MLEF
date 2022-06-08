FC=gfortran10
LD=$(FC)
FFLAGS=-O2 -fconvert=big-endian
LDFLAGS=
LIBS=

PARENT=${PWD}
SRCSIG=$(PARENT)/src/read_sig.f90
OBJSIG=read_sig.o
EXECSIG=read_sig
SRCSFC=$(PARENT)/src/read_sfc.f90
OBJSFC=read_sfc.o
EXECSFC=read_sfc

$(EXECSIG):	$(OBJSIG)
	$(LD) $(LDFLAGS) -o $(EXECSIG) $(OBJSIG) $(LIBS)

$(OBJSIG):	$(SRCSIG)
	$(FC) $(FFLAGS) -c $(SRCSIG)

$(EXECSFC):	$(OBJSFC)
	$(LD) $(LDFLAGS) -o $(EXECSFC) $(OBJSFC) $(LIBS)

$(OBJSFC):	$(SRCSFC)
	$(FC) $(FFLAGS) -c $(SRCSFC)
