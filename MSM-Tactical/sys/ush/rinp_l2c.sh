#
PROG=rinp
rb_pgbf=$1
fh=$2
rm -f fort.[0-9]* 2>/dev/null
#
    echo " Regional input starts:"
    echo " &NAMRIN                                         "  >rinpparm
    echo "    SIG2RG=.FALSE.,SFC2RG=.FALSE.,PERCMTN=0.2,   " >>rinpparm
    echo "    NEWSIG=.TRUE. ,NEWMTN=.TRUE.,NEWHOR=.FALSE., " >>rinpparm
    echo "    PGB2RG=.TRUE.,iqvar=$IQVAR,                  " >>rinpparm
    echo "    ivs=$IVS,igribversion=$IGRIBVERSION,         " >>rinpparm
    echo " &END                                            " >>rinpparm
##    echo " /                                               " >>rinpparm
    cat rsmlocation >> rinpparm
    echo " &namsig                                         " >>rinpparm
#    echo "    fcsttime=-9999,                              " >>rinpparm
    echo " &end                                            " >>rinpparm
    echo " &namsfc                                         " >>rinpparm
#    echo "    fcsttime=-9999,                              " >>rinpparm
    echo " &end                                            " >>rinpparm
    echo " &namclim                                       " >>rinpparm
if [ $CLIM = 1 ]
then
    echo "    iclim=1,                                    " >>rinpparm
    echo "    fnmskh=\"$FNMSKH\",                             " >>rinpparm
    echo "    fnalbc=\"$FNALBC\",                             " >>rinpparm
    echo "    fnsotc=\"$FNSOTC\",                             " >>rinpparm
    echo "    fnvegc=\"$FNVEGC\",                             " >>rinpparm
    echo "    fnvetc=\"$FNVETC\",                             " >>rinpparm
    echo "    fnzorc=\"$FNZORC\",                             " >>rinpparm
    echo "    fntg3c=\"$FNTG3C\",                             " >>rinpparm
fi
    echo " &end                                            " >>rinpparm


#    ln -fs $1   fort.11
    ln -fs $1   input.grib
#
    ln -fs rmtnslm             fort.13
    ln -fs rmtnoss             fort.14
    ln -fs ${FIXDIR}/siglevel.l${LEVR}.txt            fort.15
    ln -fs ${FIXDIR}/pgblevel.l${LEVS}.txt            fort.17
    ln -fs ${FIXDIR}/$FNMSKH .
    ln -fs ${FIXDIR}/$FNALBC .
    ln -fs ${FIXDIR}/$FNSOTC .
    ln -fs ${FIXDIR}/$FNVEGC .
    ln -fs ${FIXDIR}/$FNVETC .
    ln -fs ${FIXDIR}/$FNZORC .
    ln -fs ${FIXDIR}/$FNTG3C .
#
    ln -fs r_sigtmp            fort.51
    ln -fs r_sfctmp            fort.52
    ln -fs r_sigi              fort.61
    ln -fs r_sfci              fort.62
#
# (note: If NEWMTN=.TRUE. --> output=61, 62)
# (note: If NEWMTN=.FALSE. --> output=51, 52)
#
rm $PROG.x
ln -fs $EXPEXE/$PROG.x $PROG.x
./$PROG.x <rinpparm > stdout.rinp_l2c 
if [ $? -ne 0 ] ; then
   echo " Error in rinp_l2c after "$PROG.x
   exit 
fi
#
NEWMTN=YES
if [ $NEWMTN = NO ]
then
    cp r_sigtmp c_sig$fh || exit 2
    cp r_sfctmp c_sfc$fh || exit 3
fi
  rm r_sigtmp r_sfctmp
#
  cp r_sigi c_sig${fh}
  cp r_sfci c_sfc${fh}
  cp r_sigi r_sig.f${fh}
  cp r_sfci r_sfc.f${fh}
 
  rm r_sigi r_sfci

#
#
rm -f fort.[0-9]* 2>/dev/null
