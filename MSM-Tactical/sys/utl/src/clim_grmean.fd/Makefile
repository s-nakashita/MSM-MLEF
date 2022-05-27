SHELL = /bin/sh
EXECUTABLES = clim_grmean
OBJS = grmean.o

CP = cp
RM = rm
CD = cd
CHMOD = chmod
CC = cc
CPP = /usr/ccs/lib/cpp

F77 = xlf
FFLAGS = -qrealsize=8 
LDFLAGS = -qrealsize=8 
INCLUDE_DIR = -I/nwprod/lib/sorc/clim/include

LIBS = -L/nwprod/lib -lclimw3l 

${EXECUTABLES} : ${OBJS}
	${F77} ${LDFLAGS} -o $@ $? ${LIBS}

.SUFFIXES : .x .o .f .F

.F.f :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i

.F.o :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FFLAGS} -c $*.f

.F.x :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FFLAGS} -c $*.f
	${F77} ${LDFLAGS} -o $@ $< ${LIBS}

.o.x :
	${F77} ${LDFLAGS} -o $@ $< ${LIBS}

.f.x :
	${F77} ${FFLAGS} -c $<
	${F77} ${LDFLAGS} -o $@ $*.o ${LIBS}

.f.o :
	${F77} ${FFLAGS} -c $<

clean :
	- ${RM} -rf *.f *.o ${EXECUTABLES} core rii_files

dist_clean :
	${RM} -f ${EXECUTABLES}

# DO NOT DELETE
