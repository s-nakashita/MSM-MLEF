SHELL = /bin/sh
EXEC = 		prepare_terrain.x

OBJ_MOD = 	comrloc.o comrfft.o comrpln.o

OBJ_RMTN= 	prepare_terrain.o 	\
		terrain_sub.o		\
		rmt05min.o		\
		rmt30sec.o		\
		rmt01sec.o		\
		rmakeoao05.o		\
		rmakeoal05.o		\
		rmakepc30.o		\
		setgrd.o		\
		defg2r.o		\
		defloc.o		\
		bilinear.o		\
		maxmin.o		

OBJ_SPECT =	lnczs.o ffscos.o ffacos.o sumffc.o sumggc.o 

OBJ_FFT99M= 	fax.o fftrig.o vpassm.o fft99m.o fft99a.o fft99b.o

OBJS = 		${OBJ_MOD} ${OBJ_RMTN} ${OBJ_SPECT} ${OBJ_FFT99M}

CPP = 	${C_PREP}
F90 = 	${F_COMP}
LIBS = 	${F_LIBS}
CPP_FLAGS = ${C_FLAG}
MOD_FLAGS = ${M_FLAG}
FOR_FLAGS = ${F_FLAG}
INCLUDE_DIR = -I.

#
# *****************************************************************
#
all: 	${EXEC}
	echo ${EXEC} is up to date.

${EXEC}: 	${OBJS}
	${F90} ${FOR_FLAGS} -o ${EXEC} ${OBJS} ${LIBS} 

.SUFFIXES : .o .f90 .F

.F.f90 :
	${CPP} ${CPP_FLAGS} ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f90 ; rm -f $*.i

.F.o :
	${CPP} ${CPP_FLAGS} ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f90 ; rm -f  $*.i
	${F90} ${FOR_FLAGS} -c $*.f90

.f90.o :
	${F90} ${FOR_FLAGS} -c $*.f90

clean:
	rm -f *.o *.f90 *.mod core

#
# *****************************************************************
#
comrloc.o: comrloc.F
	$(F90) $(MOD_FLAGS) -c comrloc.F
comrpln.o: comrpln.F
	$(F90) $(MOD_FLAGS) -c comrpln.F
comrfft.o: comrfft.F
	$(F90) $(MOD_FLAGS) -c comrfft.F
