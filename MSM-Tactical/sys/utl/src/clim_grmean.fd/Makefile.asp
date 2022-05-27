EXECUTABLES = clim_grmean
OBJS = grmean.o

SHELL = /bin/sh
CP = cp
RM = rm
CD = cd
CHMOD = chmod
CC = cc
CPP = /usr/ccs/lib/cpp

LIB_DIR = /clim/cpcprod/climlib/configure/..
ETC_DIR = /clim/cpcprod/climlib/configure/../../exec
F77 = xlf
FORT_FLAGS = -qrealsize=8
LOAD_FLAGS = -qrealsize=8
INCLUDE_DIR = -I/clim/cpcprod/climlib/configure/../include

LIB = ${LIB_DIR}/climw3l.a 

${EXECUTABLES} : ${OBJS}
	${F77} ${LOAD_FLAGS} -o $@ $? ${LIB}
	${CP} $@ ${ETC_DIR}
	${CD} ${ETC_DIR}; \
	${CHMOD} 755 $@

.SUFFIXES : .x .o .f .F

.F.f :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i

.F.o :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f

.F.x :
	${CPP} -P ${INCLUDE_DIR} $*.F >$*.i
	sed '/^ *$$/d' $*.i >$*.f ; rm $*.i
	${F77} ${FORT_FLAGS} -c $*.f
	${F77} ${LOAD_FLAGS} -o $@ $< ${LIB}

.o.x :
	${F77} ${LOAD_FLAGS} -o $@ $< ${LIB}

.f.x :
	${F77} ${FORT_FLAGS} -c $<
	${F77} ${LOAD_FLAGS} -o $@ $*.o ${LIB}

.f.o :
	${F77} ${FORT_FLAGS} -c $<

clean :
	- ${RM} -rf *.f *.o ${EXECUTABLES} core rii_files

dist_clean :
	- ${CD} ${ETC_DIR}; \
	${RM} -f ${EXECUTABLES}

# DO NOT DELETE
