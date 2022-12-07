FC=gfortran10
LD=$(FC)
FFLAGS=-O2 -fconvert=big-endian
LDFLAGS=
LIBS=

PARENT=${PWD}
MODDIR=${PWD}/../../dpac/src/common_modules
SRCSIG=$(PARENT)/src/read_sig.f90
OBJSIG=read_sig.o
EXECSIG=read_sig
SRCSFC=$(PARENT)/src/read_sfc.f90
OBJSFC=read_sfc.o
EXECSFC=read_sfc
SRCFLX=$(PARENT)/src/read_flx.f90
OBJFLX=read_flx.o
EXECFLX=read_flx

$(EXECSIG):	kind_module.o phconst_module.o read_module.o $(OBJSIG)
	$(LD) $(LDFLAGS) -o $(EXECSIG) $^ $(LIBS)

$(OBJSIG):	$(SRCSIG)
	$(FC) $(FFLAGS) -c $(SRCSIG)

$(EXECSFC):	$(OBJSFC)
	$(LD) $(LDFLAGS) -o $(EXECSFC) $(OBJSFC) $(LIBS)

$(OBJSFC):	$(SRCSFC)
	$(FC) $(FFLAGS) -c $(SRCSFC)

$(EXECFLX):	$(OBJFLX)
	$(LD) $(LDFLAGS) -o $(EXECFLX) $(OBJFLX) $(LIBS)

$(OBJFLX):	$(SRCFLX)
	$(FC) $(FFLAGS) -c $(SRCFLX)

ensmspr:	kind_module.o rsmcom_module.o read_module.o write_module.o ensmspr.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)

calcte:	kind_module.o phconst_module.o rsmcom_module.o read_module.o write_module.o norm_module.o calcte.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)

addprtb:	kind_module.o phconst_module.o rsmcom_module.o read_module.o write_module.o norm_module.o addprtb.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)

test_read:	read_module.o write_module.o test_read.o
	$(LD) $(LDFLAGS) -o $@ $^ $(LIBS)

kind_module.o:	$(MODDIR)/kind_module.f90
	$(FC) $(FFLAGS) -c $^

read_module.o:	$(MODDIR)/read_module.f90
	$(FC) $(FFLAGS) -c $^

write_module.o:	$(MODDIR)/write_module.f90
	$(FC) $(FFLAGS) -c $^

phconst_module.o:	$(MODDIR)/phconst_module.f90
	$(FC) $(FFLAGS) -c $^

rsmcom_module.o:	$(MODDIR)/rsmcom_module.f90
	$(FC) $(FFLAGS) -c $^

norm_module.o:	$(MODDIR)/norm_module.f90
	$(FC) $(FFLAGS) -c $^

ensmspr.o:	$(PARENT)/src/ensmspr.f90
	$(FC) $(FFLAGS) -c $^

calcte.o:	$(PARENT)/src/calcte.f90
	$(FC) $(FFLAGS) -c $^

addprtb.o:	$(PARENT)/src/addprtb.f90
	$(FC) $(FFLAGS) -c $^

test_read.o:	$(PARENT)/src/test_read.f90
	$(FC) $(FFLAGS) -c $^
