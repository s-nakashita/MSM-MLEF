#/bin/sh

set -ex

#
PROG=rinp
dtype=$1
fh=$2
lmd=${3:-F}
#
if [ do$dtype = doG2R ] ; then
   sig2rg=.TRUE.
   sfc2rg=.TRUE.
   pgb2rg=.FALSE.
fi
if [ do$dtype = doP2R ] ; then
   sig2rg=.FALSE.
   sfc2rg=.FALSE.
   pgb2rg=.TRUE.
fi  
if [ do$dtype = doC2R -o do$dtype = doN2R ] ; then
   sig2rg=.TRUE.
   sfc2rg=.TRUE.
   pgb2rg=.FALSE.
fi
newmtn=${NEWMTN:-.TRUE.}

   
rm -f fort.[0-9]* 2>/dev/null
#
    echo " Regional input starts:"
    echo " &NAMRIN                                                "  >rinpparm
    echo "    SIG2RG=$sig2rg,SFC2RG=$sfc2rg,PERCMTN=$PERCMTN,     " >>rinpparm
    echo "    NEWSIG=$NEWSIG,NEWMTN=$newmtn,NEWHOR=.FALSE.,       " >>rinpparm
    echo "    PGB2RG=$pgb2rg,NEWSST=$NEWSST,                      " >>rinpparm
    echo "    ivs=$IVS,iqvar=$IQVAR,igribversion=$IGRIBVERSION,   " >>rinpparm
if [ $lmd = T ]; then
    echo "    MODDATE=.TRUE.,                                     " >>rinpparm
fi
    echo " &END                                                   " >>rinpparm
#
    cat rsmlocation >> rinpparm

    echo " &namsig                                         " >>rinpparm
#    echo "    fcsttime=-9999,                              " >>rinpparm
    echo " &end                                            " >>rinpparm
    echo " &namsfc                                         " >>rinpparm
#    echo "    fcsttime=-9999,                              " >>rinpparm
    echo " &end                                            " >>rinpparm
    echo " &namclim                                        " >>rinpparm
if [ $CLIM = 1 ]
then
    echo "    iclim=1,                                    " >>rinpparm
    echo "     fnmskh=\"$FNMSKH\",                             " >>rinpparm
    echo "     fnalbc=\"$FNALBC\",                             " >>rinpparm
    echo "     fnsotc=\"$FNSOTC\",                             " >>rinpparm
    echo "     fnvegc=\"$FNVEGC\",                             " >>rinpparm
    echo "     fnvetc=\"$FNVETC\",                             " >>rinpparm
    echo "     fnzorc=\"$FNZORC\",                             " >>rinpparm
    echo "    fntg3c=\"$FNTG3C\",                             " >>rinpparm
fi
    echo " &end                                            " >>rinpparm

if [ do$dtype = doG2R -o do$dtype = doC2R -o do$dtype = doN2R ] ; then
    ln -fs rb_sigf$fh   fort.11
    ln -fs rb_sfcf$fh   fort.12
fi
if [ do$dtype = doP2R ] ; then
    ln -fs rb_pgbf$fh   input.grib
fi
if [ do$NEWSST = do.TRUE. ] ; then
    #ln -fs rb_sstf$fh   sst.grib
    echo " &namsst                                         " >>rinpparm
    echo "    issttype=0,                                  " >>rinpparm
    echo " &end                                            " >>rinpparm
fi
     
#
    ln -fs rmtnslm             fort.13
    ln -fs rmtnoss             fort.14
    ln -fs ${FIXDIR}/siglevel.l${LEVR}.txt       fort.15
if [ do$dtype = doP2R ] ; then
    ln -fs ${FIXDIR}/pgblevel.l${LEVS}.txt       fort.17
fi

if [ $CLIM = 1 ]
then
    ln -fs ${FIXDIR}/$FNMSKH .
    ln -fs ${FIXDIR}/$FNALBC .
    ln -fs ${FIXDIR}/$FNSOTC .
    ln -fs ${FIXDIR}/$FNVEGC .
    ln -fs ${FIXDIR}/$FNVETC .
    ln -fs ${FIXDIR}/$FNZORC .
    ln -fs ${FIXDIR}/$FNTG3C .
fi
#
    ln -fs r_sigtmp            fort.51
    ln -fs r_sfctmp            fort.52
    ln -fs r_sigi              fort.61
    ln -fs r_sfci              fort.62
#
# (note: If NEWMTN=.TRUE. --> output=61, 62)
# (note: If NEWMTN=.FALSE. --> output=51, 52)
#
rm -f $PROG.x
ln -fs $EXPEXE/$PROG.x $PROG.x
${PWD}/$PROG.x <rinpparm 2>&1 | tee stdout.rinp$fh 
#${RUNDIR}/$PROG.x <rinpparm 2>&1 | tee stdout.rinp$fh 
if [ $? -ne 0 ] ; then
   echo " Error after "$PROG.x
   exit 101
fi
#
#NEWMTN=YES
if [ $NEWMTN = .FALSE. ]
then
    cp r_sigtmp r_sigi || exit 102
    cp r_sfctmp r_sfci || exit 103
fi
  rm r_sigtmp r_sfctmp
  cp r_sigi r_sigitdt
#
# cp r_sigi  r_sig.f${fh}
# cp r_sfci  r_sfc.f${fh}
#
rm -f fort.[0-9]* 2>/dev/null
