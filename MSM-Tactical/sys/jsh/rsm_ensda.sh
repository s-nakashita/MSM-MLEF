#!/bin/sh
#
#-----------------------------------------------
# specify starting date and end date
#-----------------------------------------------
#define directories
set -x
syear=`echo $SDATE |cut -c1-4`          ## starting year
smonth=`echo $SDATE |cut -c5-6`         ## starting month
sday=`echo $SDATE |cut -c7-8`           ## starting day
CHOUR=`echo $SDATE |cut -c9-10`         ## starting hour
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
   monthend=`expr $smonth + $LENMON + 1`
else
   monthend=`expr $smonth + $LENMON`
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
ENDHOUR=${ENDHOUR:-$end_hr}
ENDHOUR0=$ENDHOUR #save
INCCYCLE=${INCCYCLE:-$ENDHOUR}
INCCYCLEBASE=${INCCYCLEBASE:-$INCCYCLE}
IOFFSET=${IOFFSET:-$INCCYCLE}
EXTEND=${EXTEND:-0}
DANEST=${DANEST:-F}
HEADBASE=${HEADBASE:-HEAD}
export DANEST
#
#-----------------------------------------------
# determine model run parameters
#-----------------------------------------------
# move some parameters to utl/rsm_default.option
RESVER=${RESVER:-$LEVS}
sfc_freq=${sfc_freq:-24}
IOUTNHR=${IOUTNHR:-1}
if [ $RSFC_MERGE = "yes" ]; then
   ISFC_MERGE=1
else
   ISFC_MERGE=0
fi
if [ do$NEWSST = do.TRUE. ]; then
   INEWSST=1
else
   INEWSST=0
fi
SSTLAG=${SSTLAG:-0}
# BGM
BP=${BP}
PSUB=${PSUB:-no}
export BP PSUB
#
# NO NEED TO CHANGE BELOW THIS!
#
FCSTSEC=`expr $INCHOUR \* 3600`
PRNTSEC=`expr $PRTHOUR \* 3600`
PRNTSEC=${PSEC:-$PRNTSEC}
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
#-----------------------------------------------
# start cycle
#-----------------------------------------------
CYCLE=${CYCLESTART:-0}
if [ $CYCLE -gt $DASTART ]; then
  CYCLEDA=`expr $CYCLE - $DASTART`
else
  CYCLEDA=0
fi
SDATE0=$SDATE
export SDATE0
while [ $CYCLE -le $CYCLEMAX ];do
  export CYCLE
  inch=0
  if [ $CYCLE -ge 1 ]; then
  inch=$IOFFSET
  inch=`expr $inch + $INCCYCLE \* \( $CYCLE - 1 \)`
  elif [ $CYCLE -lt 0 ]; then
  inch=`expr $INCCYCLE \* $CYCLE`
  fi
  SDATE=`${UTLDIR}/ndate $inch ${SDATE0}`
  export SDATE
  syear=`echo $SDATE |cut -c1-4`          ## starting year
  smonth=`echo $SDATE |cut -c5-6`         ## starting month
  sday=`echo $SDATE |cut -c7-8`           ## starting day
  CHOUR=`echo $SDATE |cut -c9-10`         ## starting hour
  CDATE=$syear$smonth$sday
  if [ $CYCLE -eq 0 ]; then
    export ENDHOUR=$IOFFSET
  else
    if [ $EXTEND -eq 1 ];then
      HZ=`echo $SDATE | cut -c9-10`
      HZ=`expr $HZ`
      if [ $HZ -ne 0 ] && [ $HZ -ne 12 ]; then
      export ENDHOUR=$INCCYCLE
      fi
    else
      export ENDHOUR=$INCCYCLE
    fi
  fi
  if [ $CYCLE -lt $DASTART ];then
    BGM=yes
    DA=no
    head=${HEAD:-bv}
    if [ $DANEST = T ]; then
      head=${HEADBASE}
    fi
  else
    CYCLEDA=`expr $CYCLEDA + 1`
    BGM=no
    DA=yes
    head=${HEAD2:-da}
  fi
#-----------------------------------------------
# set running space
#-----------------------------------------------
RUNDIR=$RUNDIR0/$SDATE
base_dir=$BASEDIR0/$SDATE
PDATE=$SDATE
f0base=0
if [ ! -d $base_dir ];then
base_dir=$BASEDIR1/$SDATE
if [ $OSSE = F ] && [ ! -d $base_dir ];then
  while [ $f0base -lt $INCCYCLEBASE ];do
    PDATE=`${UTLDIR}/ndate -$INCCYCLE ${PDATE}`
    f0base=`expr $f0base + $INCCYCLE`
    base_dir=$BASEDIR0/$PDATE
    if [ -d $base_dir ];then
      break
    else
      base_dir=$BASEDIR1/$PDATE
      if [ -d $base_dir ];then break; fi
    fi
  done
  if [ ! -d $base_dir ];then
    echo 'Cannot find boundary data '$base_dir
    exit 1
  fi
fi
fi
BASEDIR=$base_dir
BASESFCDIR=$BASEDIR
export RUNDIR BASEDIR BASESFCDIR
mkdir -p $RUNDIR
cd $RUNDIR || exit 1
#### define fcst parameters ######
echo "CYCLE $CYCLE (ENDHOUR $ENDHOUR) (DA $CYCLEDA) / $CYCLEMAX" > cycle.txt
# rsm location first
if [ -s $WORK/route ] ; then
  cp -f $WORK/route route
  read hour cenlat cenlon < route
  export RCENLAT=$cenlat
  export RCENLON=$cenlon
fi
$USHDIR/rloc.sh || exit 2

cat >rsmparm <<EOF
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
 RLXZSEC=$RLZMSEC,
 DIFHSEC=$DIFHSEC,
 DIFMSEC=$DIFMSEC,
 RSFCSEC=$RSFCSEC,
 ISEMIMP=$ISEMIMP,
 IIMPRLX=$IIMPRLX,
 IDMPJET=$IDMPJET,
 IMDLPHY=$IMDLPHY,
 IOUTNHR=$IOUTNHR,
 ISFCMRG=$ISFC_MERGE,
 INEWSST=$INEWSST,
 RDFISEC=$RDFISEC,
 
 &END
EOF
#
  cat rsmparm rsmlocation > rfcstparm
  cp rsmlocation $RUNDIR
  cp $STTPRM $RUNDIR/station.parm
#
##########################################################
#
#   Ensemble DA
#
if [ do$DA = doyes ]; then
  if [ -d ${head}mean ]; then
    echo 'DA already done'
  else
    echo 'ensemble DA : '$SDATE' cycle='$CYCLEDA
    #
    #   Regional mountain
    #
    if [ do$RUNRMTN = doyes ] ; then
      $USHDIR/rmtn.sh $MTNRES || exit 3
    fi
    #
    #  DA
    #
    $USHDIR/rensda.sh $CYCLEDA || exit 7
    if [ do$POSTTYPE = dosync ]; then
      if [ $NODA = T ]; then
        $USHDIR/rpgb_post.sh 00 || exit 5
      fi
      if [ $DA_MEAN = T ]; then
      mem=mean
      else
      mem=000
      fi
      cd ${head}${mem}
      $USHDIR/rpgb_post.sh 00 || exit 5
      cd ..
      mem=1
      while [ $mem -le $MEMBER ];do
        if [ $mem -lt 10 ]; then
          mem=00$mem
        elif [ $mem -lt 100 ];then
          mem=0$mem
        fi
        cd ${head}${mem}
        $USHDIR/rpgb_post.sh 00 || exit 5
        cd ..
        mem=`expr $mem + 1`
      done
    fi
  fi # -d ${head}000
else
  #
  # control
  if [ $DANEST = T ]; then
    mkdir -p ${head}000
    cd ${head}000
    cp ../rsmparm . 
    cp ../rsmlocation . 
    cp ../rfcstparm . 
    cp ../station.parm . 
  fi
  if [ -s r_sigi -a -s r_sigitdt -a -s r_sfci ] ; then
    #
    #  Restart
    #
        echo 'Restart files existed!!!!!!!'
        rm fort.*
        ln -fs r_sigi fort.11
        $UTLDIR/fhour.x >dte.out
        read hour month day year FH <dte.out && rm dte.out
        if [ $FH -lt 10 ];then FH=0$FH;fi
        FEND=`expr $FH + $FHMAX`
        if [ $FEND -gt $ENDHOUR ]; then
           FEND=$ENDHOUR
        fi
  else 
        FH=00
        FEND=`expr $FH + $FHMAX`
        if [ $FEND -gt $ENDHOUR ]; then
           FEND=$ENDHOUR
        fi
    #
    #   Regional mountain
    #
    if [ do$RUNRMTN = doyes ] ; then
      $USHDIR/rmtn.sh $MTNRES || exit 3
    fi
    #
    # RSM INITIAL forecast (control)
    #
    fh0=$f0base
    if [ $fh0 -lt 10 ];then fh0=0$fh0;fi
    if [ $HEADBASE = $HEAD ]; then
      BASEDIR=${BASEDIR0}/${PDATE}
    else
      BASEDIR=${BASEDIR0}/${PDATE}/${HEADBASE}000
    fi
    BASESFCDIR=${BASEDIR}
    #     ln -fs $BASEDIR/sigf$CDATE$CHOUR rb_sigf00
    #     ln -fs $BASEDIR/sfcf$CDATE$CHOUR rb_sfcf00
     if [ do$G2R = doyes ] ; then
       ln -fs $BASEDIR/sigf$fh0 rb_sigf00
       ln -fs $BASEDIR/sfcf$fh0 rb_sfcf00
     fi
     if [ do$P2R = doyes ] ; then
       if [ do$CWBGFS = doyes ] ; then
         ln -fs $BASEDIR/otgb2_0$fh0 rb_pgbf00
       else
         ln -fs $BASEDIR/pgbf$fh0 rb_pgbf00
       fi
     else
       if [ do$C2R = doyes ] ; then
         ln -fs $BASEDIR/r_sig.f$fh0 rb_sigf00
         ln -fs $BASESFCDIR/r_sfc.f$fh0 rb_sfcf00
       fi
     fi
     if [ do$NEWSST = do.TRUE. ] ; then
       #ln -fs $BASEDIR/sstf$fh0 rb_sstf00
       slag=$SSTLAG
       slag=`expr $hhr + $SSTLAG`
       cymdh=`${UTLDIR}/ndate $slag ${SDATE}`
       cyyyy=`echo ${cymdh} | cut -c1-4`
       cymd=`echo ${cymdh} | cut -c1-8`
       if [ ! -f ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt ] ; then
         exit 99
       fi
       ln -fs ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt himsst.txt
     fi

    #
    #  Initial field for rsm run
    #
    if [ do$RUNRINP = doyes ] ; then
      lmd=F
      if [ $fh0 -gt 0 ]; then
	lmd=T
      fi
      $USHDIR/rinp.sh $NEST 00 $lmd || exit 4
      cp r_sigi  r_sig.f00
      cp r_sfci  r_sfc.f00
    fi
    if [ do$POSTTYPE = dosync ]; then
      $USHDIR/rpgb_post.sh 00 || exit 5
    fi
  #
  fi #restart
  if [ $DANEST = T ]; then cd ..;fi
  #
  # BGM rescaling (ensemble)
  #
  if [ do$BGM = doyes ] && [ $MEMBER -gt 0 ] && [ $IRES -gt 9 ]; then
    #rescaling
    if [ ! -s pdate.txt ]; then
    if [ $GLOBAL = GFS ] && [ $CYCLE -eq $CYCLESTART ]; then
      cp $DISKUSR/exp/$EXPN/$SAMPLETXT .
      NSAMPLE=`expr $MEMBER \* 2`
      $PYENV $UTLDIR/random_sample.py $SAMPLETXT $NSAMPLE > pdate.txt || exit 6
    fi
    fi
    $USHDIR/raddprtb.sh $CYCLE $CYCLESTART || exit 7
    if [ do$POSTTYPE = dosync ]; then
      mem=1
      while [ $mem -le $MEMBER ];do
      pmem=$mem
      if [ $pmem -lt 10 ]; then
        pmem=00$pmem
      else
        pmem=0$pmem
      fi
      if [ do$PSUB = doyes ]; then
        pmem=m$pmem
      fi
      cd ${head}${pmem}
      $USHDIR/rpgb_post.sh 00 || exit 5
      cd ..
      mem=`expr $mem + 1`
      done
    fi #POST sync
  else #downscaling
  fh0=$f0base
  if [ $fh0 -lt 10 ];then fh0=0$fh0;fi
  mem=1
  while [ $mem -le $MEMBER ];do
    pmem=$mem
    if [ $pmem -lt 10 ]; then
      pmem=00$pmem
    elif [ $mem -lt 100 ]; then
      pmem=0$pmem
    fi
    if [ do$BGM = doyes ] && [ do$PSUB = doyes ]; then
      pmem=m$pmem
    fi
    if [ -s ${head}${pmem}/r_sigi -a -s ${head}${pmem}/r_sigitdt -a -s ${head}${pmem}/r_sfci ] ; then
      echo 'Restart file exists'
    else
    GBASEDIR=${BASEDIR0}/${PDATE}/${head}${pmem}
#    if [ $DANEST = T ]; then
#    GBASEDIR=${BASEDIR0}/${PDATE}/${HEADBASE}${pmem}
#    fi
    GBASESFCDIR=${GBASEDIR}
    GUESDIR=${RUNDIR0}/${SDATE}/${head}${pmem}
    mkdir -p $GUESDIR
    cd ${GUESDIR}
    ### copy namelists
    cp ${RUNDIR}/rsmparm .
    cp ${RUNDIR}/rsmlocation .
    cp ${RUNDIR}/rfcstparm .
    cp ${RUNDIR}/station.parm .
    ### copy orography data
    cp ${RUNDIR}/rmtn.parm .
    cp ${RUNDIR}/rmtnoss .
    cp ${RUNDIR}/rmtnslm .
    cp ${RUNDIR}/rmtnvar .
    if [ do$G2R = doyes ] ; then
      ln -fs $GBASEDIR/sigf$fh0 rb_sigf00
      ln -fs $GBASEDIR/sfcf$fh0 rb_sfcf00
    fi
    if [ do$P2R = doyes ] ; then
      if [ do$CWBGFS = doyes ] ; then
        ln -fs $GBASEDIR/otgb2_0$fh0 rb_pgbf00
      else
        ln -fs $GBASEDIR/pgbf$fh0 rb_pgbf00
      fi
    else
      if [ do$C2R = doyes ] ; then
        ln -fs $GBASEDIR/r_sig.f$fh0 rb_sigf00
        ln -fs $GBASESFCDIR/r_sfc.f$fh0 rb_sfcf00
      fi
    fi
    if [ do$NEWSST = do.TRUE. ] ; then
      #ln -fs $BASEDIR/sstf00 rb_sstf00
      slag=$SSTLAG
      cymdh=`${UTLDIR}/ndate $slag ${SDATE}`
      cyyyy=`echo ${cymdh} | cut -c1-4`
      cymd=`echo ${cymdh} | cut -c1-8`
      if [ ! -f ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt ] ; then
        exit 99
      fi
      ln -fs ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt himsst.txt
    fi
    lmd=F
    if [ $fh0 -gt 0 ]; then
      lmd=T
    fi
    $USHDIR/rinp.sh $NEST 00 $lmd || exit 4
    cp r_sigi  r_sig.f00
    cp r_sfci  r_sfc.f00
    if [ do$POSTTYPE = dosync ]; then
      $USHDIR/rpgb_post.sh 00 || exit 5
    fi #POST sync
    cd $RUNDIR
    fi #restart
  mem=`expr $mem + 1`
  done #while [ $mem -le $MEMBER ]
  fi #doBGM=doyes
#
fi #doDA=doyes
#######################################
# Ensemble Forecast loop
########################################
PREPBASE=F
if [ $CYCLEDA -ge 1 ] && [ $OSSE = T ]; then
  $USHDIR/rprepbase.sh $CYCLEDA $DA_MEAN || exit 6
  PREPBASE=T
#fi
elif [ do$BP = dowbp ] && [ $GLOBAL = GFS ] && [ $IRES -gt 9 ]; then
  if [ ! -s pdatebase.txt ]; then
  # Base field perturbation
  cp $DISKUSR/exp/$EXPN/$SAMPLETXT .
  NSAMPLE=`expr $MEMBER \* 2`
  $PYENV $UTLDIR/random_sample.py $SAMPLETXT $NSAMPLE > pdatebase.txt
  fi
  $USHDIR/raddprtbbase.sh || exit 5
  PREPBASE=T
fi
if [ $CYCLEDA -ge 1 ] && [ $DA_MEAN = T ]; then
mem0=1
else
mem0=0
fi
mem=$mem0
if [ $OSSE = T ] && [ $NODA = T ] && [ $CYCLEDA -eq 1 ]; then
  mem=`expr $mem - 1`
fi
while [ $mem -le $MEMBER ];do
  if [ $mem -lt $mem0 ] && [ $OSSE = T ] && [ $NODA = T ]; then
    export ENDHOUR=$ENDHOUR0
    cd $RUNDIR
    if [ $DANEST = T ];then
    export BASEDIR=$base_dir
    export BASESFCDIR=$BASEDIR
    fi
  elif [ $mem -eq 0 ]; then ## control
    if [ do$DA = doyes ] || [ $DANEST = T ]; then
#    export ENDHOUR=$INCCYCLE
    cd $RUNDIR/${head}000
    else
    cd $RUNDIR
    fi
    if [ $DANEST = T ];then
    if [ $HEADBASE = $HEAD ]; then
      export BASEDIR=$base_dir
    else
      export BASEDIR=$base_dir/${HEADBASE}000
    fi
    export BASESFCDIR=$BASEDIR
    fi
  else ## member
#    if [ do$DA = doyes ]; then
#    export ENDHOUR=$INCCYCLE
#    fi
    pmem=$mem
    if [ $pmem -lt 10 ]; then
      pmem=00$pmem
    else
      pmem=0$pmem
    fi
    if [ do$BGM = doyes ] && [ do$PSUB = doyes ]; then
      pmem=m$pmem
    fi
    cd $RUNDIR/${head}${pmem}
    #if [ do$BP = dowbp ]; then
    #  export BASEDIR=${base_dir}/${pmem}
    #  export BASESFCDIR=$BASEDIR
    #fi
    if [ $DANEST = T ];then
    export BASEDIR=${base_dir}/${HEADBASE}${pmem}
    export BASESFCDIR=$BASEDIR
    fi
  fi
        rm fort.*
        ln -fs r_sigi fort.11
        $UTLDIR/fhour.x >dte.out
        read hour month day year FH <dte.out && rm dte.out
        if [ $FH -lt 10 ];then FH=0$FH;fi
        FEND=`expr $FH + $FHMAX`
        if [ $FEND -gt $ENDHOUR ]; then
           FEND=$ENDHOUR
        fi
h=$FH
while [ $h -lt $FEND ]; do
  hx=`expr $h + $INCHOUR`
  if [ $hx -gt $FEND ]; then  hx=$FEND; fi
#  hh=$hx
  if [ $hx -lt 10 ];then hx=0$hx;fi
  hh=`expr $hx + $f0base`
  if [ $hh -lt 10 ];then hh=0$hh;fi
  hr=`expr $h + 0`
  hhr=`expr $h + $f0base`
  while [ $hhr -le $hh ]; do
       if [ $hr -lt 10 ]; then hr=0$hr; fi
       if [ $hhr -lt 10 ]; then hhr=0$hhr; fi
         rfti=`$UTLDIR/ndate $hhr $CDATE$CHOUR`
  if [ $PREPBASE = F ] || \
	  ( [ $mem -lt $mem0 ] && [ $OSSE = T ] && [ $NODA = T ] ); then
       if [ do$G2R = doyes ] ; then
         ln -fs $BASEDIR/sigf$hhr rb_sigf$hr
         ln -fs $BASEDIR/sfcf$hhr rb_sfcf$hr
       fi
       if [ do$P2R = doyes ] ; then
         if [ do$CWBGFS = doyes ] ; then
           if [ $hhr -lt 100 ] ; then hhrr=0$hhr ; fi
           ln -fs $BASEDIR/otgb2_$hhrr rb_pgbf$hr
         else
           ln -fs $BASEDIR/pgbf$hhr rb_pgbf$hr
         fi
       else
         if [ do$C2R = doyes ] ; then
           ln -fs $BASEDIR/r_sig.f$hhr rb_sigf$hr
           ln -fs $BASESFCDIR/r_sfc.f$hhr rb_sfcf$hr
         fi
       fi
  fi #PREPBASE
       if [ do$NEWSST = do.TRUE. ] ; then
         #ln -fs $BASEDIR/sstf$hhr rb_sstf$hr
         slag=`expr $hx + $SSTLAG`
         cymdh=`${UTLDIR}/ndate $slag ${SDATE}`
         cyyyy=`echo ${cymdh} | cut -c1-4`
         cymd=`echo ${cymdh} | cut -c1-8`
         if [ ! -f ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt ] ; then
           exit 99
         fi
         ln -fs ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt himsst.txt
       fi
       hr=`expr $hr + $INCBASE`
       hhr=`expr $hhr + $INCBASE`
  done
#
# rinp for g2c and l2c
  if [ do$RUNRINP2 = doyes ] ; then
    $USHDIR/rinp.sh $NEST ${hx}|| exit 6
    cp r_sigi  r_sig.f${hx}
    cp r_sfci  r_sfc.f${hx}
    if [ do$POSTTYPE = dosync ]; then
      $USHDIR/rpgb_post.sh ${hx} || exit 7
    fi
  fi
#
# move domain
  if [ do$RUNMOVE = doyes ] ; then
    $USHDIR/rmove.sh $MTNRES $NEST $h $hx || exit 8
    cat rsmparm rsmlocation > rfcstparm
  fi
# fcst
  if [ do$LAMMPI = doyes ]; then
     cp -f $EXPDIR/lamhosts . || exit 9     # always copy from submit directory
     recon lamhosts
     lamboot -v lamhosts
  fi

  if [ do$RUNFCST = doyes ] ; then
    if [ $mem -eq 0 ]; then
    echo "Control forecast starting from hour $h..." >>stdout
    else
    echo "Ensemble forecast member $mem starting from hour $h..." >>stdout
    fi
    timeout 300 $USHDIR/rfcst.sh $hx || exit 10
    #$USHDIR/rfcst.sh $hx || exit 10

    if [ do$LAMMPI = doyes ]; then
       lamclean
       lamhalt
    fi

#
#  Interpolate global surface forecast to regional grid to merge
#  $sfc_freq should be equal to or larger than $INCHOUR
#
# prepare for next fcst step
    cp r_sig.f$hx r_sigi || exit 11
    mv r_sigftdt r_sigitdt  || exit 12
    cp r_sfc.f$hx r_sfci || exit 13
#
# output at every PRTHOUR:
    hr=`expr $h + $PRTHOUR`
    while [ $hr -lt $hx ];do
      if [ $hr -lt 10 ];then hr=0$hr;fi
      if [ $IOUTNHR -eq 1 ]; then
      mv r_sigf$hr   r_sig.f$hr
      mv r_sfcf$hr   r_sfc.f$hr
      mv r_flxf$hr   r_flx.f$hr
      else
      mv r_sigf${hr}:00:00   r_sig.f$hr
      mv r_sfcf${hr}:00:00   r_sfc.f$hr
      mv r_flxf${hr}:00:00   r_flx.f$hr
      fi
      hr=`expr $hr + $PRTHOUR`
    done
#
#  r_pgb
#
    if [ do$POSTTYPE = dosync ]; then

      hr=$h
      hr=`expr $h`

      while [ $hr -le $hx ];do
        if [ $hr -lt 10 ];then hr=0$hr;fi
        $USHDIR/rpgb_post.sh $hr || exit 14
        if [ do$PLOT = doyes ]; then
#
# panel plot (hr>=3) added by SN
#
          fh=`expr $hr + 0`
          if [ $fh -ge 3 ];then
          $DISKUSR/nclscripts/plot_panel6_each.sh ${IRES} ${fh} || exit 15
          fi
        fi
        hr=`expr $hr + $PRTHOUR`
      done
#
      if [ $hx -eq $ENDHOUR ]; then
        if [ $hx -lt 10 ];then hx=0$hx;fi
        if [ $IOUTNHR -eq 1 ]; then
        mv r_sigf$hx   r_sig.f$hx
        mv r_sfcf$hx   r_sfc.f$hx
        mv r_flxf$hx   r_flx.f$hx
        else
        mv r_sigf${hx}:00:00   r_sig.f$hx
        mv r_sfcf${hx}:00:00   r_sfc.f$hx
        mv r_flxf${hx}:00:00   r_flx.f$hx
        fi
        $USHDIR/rpgb_post.sh $hx || exit 16
      fi

    fi # r_pgb dosync
###
  fi # end of RUNFCST 

  h=$hx
done  ###### end of while forecast loop
mem=`expr 1 + $mem`
done  ###### end of while member loop
# ensemble mean and spread
if [ do$RUNFCST = doyes ] && [ do$ENSMSPR = doyes ] && [ $MEMBER -gt 0 ]; then
  $USHDIR/rensmspr.sh $head || exit 17
fi
# save ensemble member or not
if [ do$SAVEENS = dono ] && [ $MEMBER -gt 0 ]; then
  mem=1
  while [ $mem -le $MEMBER ];do
    pmem=$mem
    if [ $pmem -lt 10 ]; then
      pmem=00$pmem
    else
      pmem=0$pmem
    fi
    if [ do$BGM = doyes ] && [ do$PSUB = doyes ]; then
      pmem=m$pmem
    fi
    cd $RUNDIR/${head}${pmem}
    hs=$PRTHOUR
    while [ $hs -lt $ENDHOUR ];do
      if [ $hs -lt 10 ];then hs=0$hs;fi
      if [ $IOUTNHR -eq 1 ]; then
      rm r_sig.f$hs
      rm r_sfc.f$hs
      rm r_flx.f$hs
      else
      rm r_sigf${hs}:*
      rm r_sfcf${hs}:*
      rm r_flxf${hs}:*
      fi
      hs=`expr $hs + $PRTHOUR`
    done
    cd -
    mem=`expr 1 + $mem`
  done
fi
# -------- schedule job submit  ----------------
if [ do$RUNFCST = doyes ] ; then

  if [ $MACHINE = ibm_xlf ] ; then
    if [ $FEND -lt $ENDHOUR ]; then
      llsubmit $EXPDIR/llrun.q
    fi

    if [ $FEND -ge $ENDHOUR ]; then
      llsubmit $EXPDIR/llrun_avrg.q
    fi
  fi

fi # end of schedule job submit
#
CYCLE=`expr 1 + $CYCLE`
done #while [ CYCLE -le CYCLEMAX ]
