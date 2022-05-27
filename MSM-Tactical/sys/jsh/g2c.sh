#!/bin/sh
#
#-----------------------------------------------
# specify starting date and end date
#-----------------------------------------------
#define directories
set -x
syear=`echo $sdate |cut -c1-4`          ## starting year
smonth=`echo $sdate |cut -c5-6`         ## starting month
sday=`echo $sdate |cut -c7-8`           ## starting day
CHOUR=`echo $sdate |cut -c9-10`         ## starting hour
CDATE=$syear$smonth$sday
dayend=01                ## end date
hourend=$CHOUR           ## end hour
rmm=$smonth
ryyyy=$syear
if [ $sday -gt 15 ]; then
  rmm=`expr $rmm + 1`
  if [ $rmm -gt 12 ]; then
     rmm=`expr $rmm - 12`
     ryyyy=`expr $ryyyy + 1`
  fi
  if [ $rmm -lt 10 ]; then
     rmm=0$rmm
  fi
fi
#------------------------------------------------
# determine length of integration
#------------------------------------------------
yearend=$syear
if [ $sday -gt 15 ]; then
   monthend=`expr $smonth + $lenmon + 1`
else
   monthend=`expr $smonth + $lenmon`
fi
if [ $monthend -gt 12 ]; then
    monthend=`expr $monthend - 12`
    yearend=`expr $yearend + 1`
fi
if [ $monthend -lt 10 ]; then
     monthend=0$monthend
fi
eidate=$yearend$monthend$dayend$hourend
end_hr=`$UTLDIR/nhour $eidate $CDATE$CHOUR`
ENDHOUR=$end_hr

#-----------------------------------------------
# set running space
#-----------------------------------------------
RUNDIR=$TEMP/$ryyyy$rmm/r${sdate}.$CASE
base_dir=$TEMP/$ryyyy$rmm/g${sdate}.$CASE
BASEDIR=${BASEDIR:-$base_dir}
mkdir -p $RUNDIR
cd $RUNDIR || exit
#
#-----------------------------------------------
# determine model run parameters
#-----------------------------------------------
RESVER=$LEVS
MTNRES=${MTNRES:-4}
RSFC_MERGE=${RSFC_MERGE:-yes}
sfc_freq=24
DELTAT_REG=${TIMESTEP:-120}
INCHOUR=${INCHOUR:-6}
INCBASE=${INCBASE:-6}
PRTHOUR=${PRTHOUR:-6}
FHMAX=${FHMAX:-1440}
RSWRHOUR=${RSWRHOUR:-1}
RLWRHOUR=${RLWRHOUR:-1}
RDFISEC=${RDFISEC:-0.}
RLXHSEC=${RLXHSEC:-1800.}
RLXMSEC=${RLXMSEC:-1800.}
RLXZSEC=${RLXZSEC:-1800.}
DIFH=${DIFH:-3}
DIFM=${DIFM:-2}
ISEMIMP=${ISEMIMP:-1}
IIMPRLX=${IIMPRLX:-1}
IDMPJET=${IDMPJET:-0}
IMDLPHY=${IMDLPHY:-1}
IOUTNHR=${IOUTNHR:-1}
if [ $RSFC_MERGE = "yes" ]; then
   ISFC_MERGE=1
else
   ISFC_MERGE=0
fi
#
# NO NEED TO CHANGE BELOW THIS!
#
FCSTSEC=`expr $INCHOUR \* 3600`
PRNTSEC=`expr $PRTHOUR \* 3600`
RSWRSEC=`expr $RSWRHOUR \* 3600`
RLWRSEC=`expr $RLWRHOUR \* 3600`
BASESEC=`expr $INCBASE \* 3600`
DIFHSEC=`expr $DIFH \* $DELTAT_REG `
DIFMSEC=`expr $DIFM \* $DELTAT_REG `
if [ $NONHYD = "yes" ]; then
FILTA=0.80
else
FILTA=0.92
fi
IGRD1=`expr $IGRD + 1`
JGRD1=`expr $JGRD + 1`
RSFCSEC=`expr $sfc_freq \* 3600`;
#
#### define fcst parameters ######
 cat >rsmlocation <<EOF
 &NAMLOC
 RPROJ    = $RPROJ,
 RTRUTH   = $RTRUTH,
 RORIENT  = $RORIENT,
 RDELX    = $RDELX,
 RDELY    = $RDELY,
 RCENLAT  = $RCENLAT,
 RCENLON  = $RCENLON,
 RLFTGRD  = $RLFTGRD,
 RBTMGRD  = $RBTMGRD,
 CPROJ    = $CPROJ,
 CTRUTH   = $CTRUTH,
 CORIENT  = $CORIENT,
 CDELX    = $CDELX,
 CDELY    = $CDELY,
 CCENLAT  = $CCENLAT,
 CCENLON  = $CCENLON,
 CLFTGRD  = $CLFTGRD,
 CBTMGRD  = $CBTMGRD,
 &END

EOF
########
cat >rfcstparm <<EOF
 &NAMRSM
 DELTIME=$DELTAT_REG,
 FCSTSEC=$FCSTSEC,
 PRNTSEC=$PRNTSEC,
 RSWRSEC=$RSWRSEC,
 RLWRSEC=$RLWRSEC,
 BASESEC=$BASESEC,
 RDFISEC=$RDFISEC,
 FILTA=$FILTA,
 RLXHSEC=$RLXHSEC,
 RLXMSEC=$RLXMSEC,
 RLXZSEC=$RLXZSEC,
 DIFHSEC=$DIFHSEC,
 DIFMSEC=$DIFMSEC,
 RSFCSEC=$RSFCSEC,
 ISEMIMP=$ISEMIMP,
 IIMPRLX=$IIMPRLX,
 IDMPJET=$IDMPJET,
 IMDLPHY=$IMDLPHY,
 IOUTNHR=$IOUTNHR,
 ISFCMRG=$ISFC_MERGE,
 
 &END
EOF
#
  cat rsmlocation >> rfcstparm
  cp rsmlocation $RUNDIR
  cp $STTPRM $RUNDIR/station.parm
#
##########################################################
#
        FH=00
        FEND=`expr $FH + $FHMAX`
        if [ $FEND -gt $ENDHOUR ]; then
           FEND=$ENDHOUR
        fi
#
#   Regional mountain
#
    $USHDIR/rmtn.sh $MTNRES || exit 8
#
# RSM INITIAL forecast
#
  while [ $FH -le $FEND ]; do
#       rfti=`$UTLDIR/ndate $FH $CDATE$CHOUR`
#       ln -fs $BASEDIR/sigf$rfti rb_sigf$FH
#       ln -fs $BASEDIR/sfcf$rfti rb_sfcf$FH
       ln -fs $BASEDIR/sigf$FH rb_sigf$FH
       ln -fs $BASEDIR/sfcf$FH rb_sfcf$FH
#
#  Initial field for rsm run
  $USHDIR/rinp_g2c.sh rb_sigf${FH} rb_sfcf${FH} ${FH}|| exit 8
#  $USHDIR/rpgb_post.sh ${FH} || exit 8
#
      FH=`expr $FH + $INCBASE`
      if [ $FH -lt 10 ]; then FH=0$FH; fi
  done
#llsubmit $EXPDIR/llrun_c2r.q
