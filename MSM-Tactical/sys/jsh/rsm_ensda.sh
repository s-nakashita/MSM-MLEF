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
INCCYCLE=${INCCYCLE:-$ENDHOUR}
EXTEND=${EXTEND:-0}
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
CYCLE=${CYCLESTART:-1}
CYCLEDA=`expr $CYCLE - $DASTART`
if [ $CYCLEDA -lt 0 ];then
  CYCLEDA=0
fi
SDATE0=$SDATE
while [ $CYCLE -le $CYCLEMAX ];do
  if [ $CYCLE -gt 1 ];then
    PCYCLE=`expr $CYCLE - 1`
    inch=`expr $INCCYCLE \* $PCYCLE`
    SDATE=`${UTLDIR}/ndate $inch ${SDATE0}`
    export SDATE
    HZ=`echo $SDATE | cut -c9-10`
    HZ=`expr $HZ`
#    if [ $EXTEND -eq 1 ];then
#      if [ $HZ -ne 0 ] && [ $HZ -ne 12 ]; then
#        export ENDHOUR=$INCCYCLE
#      fi
#    fi
#  else
#    export ENDHOUR=$INCCYCLE
  fi
  if [ $CYCLE -lt $DASTART ];then
    BGM=yes
    DA=no
    head=${HEAD:-bv}
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
if [ ! -d $base_dir ];then
base_dir=$BASEDIR1/$SDATE
if [ ! -d $base_dir ];then
echo 'Cannot find boundary data '$base_dir
exit 1
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
#  if [ -d ${head}000 ]; then
#    echo 'DA already done'
#  else
    echo 'ensemble DA : '$SDATE' cycle='$CYCLEDA
    #
    #   Regional mountain
    #
    if [ do$RUNRMTN = doyes ] ; then
      $USHDIR/rmtn.sh $MTNRES || exit 3
    fi
    #
    #   First guess ensemble
    #
    if [ $CYCLE -eq $CYCLEDA ]&&[ $CYCLEDA -eq 1 ]; then
      if [ $IRES -eq 27 ];then
	echo 'First guess ensemble at the same resolution required for IRES='$IRES
	exit 6
      else
    fh=${INCCYCLE}
    PDATE=`${UTLDIR}/ndate -$fh ${SDATE}`
    if [ $fh -lt 10 ];then fh=0$fh; fi
    mem=0
    while [ $mem -le $MEMBER ]; do
	if [ $mem -gt 0 ];then
        if [ $mem -lt 10 ]; then
          mem=00$mem
        else
          mem=0$mem
        fi
	GBASEDIR=${BASEDIR0}/${PDATE}/${HEAD}${mem}
	GBASESFCDIR=${GBASEDIR}
        GUESDIR=${RUNDIR0}/${PDATE}/${HEAD}${mem}
        else
	GBASEDIR=${BASEDIR0}/${PDATE}
	GBASESFCDIR=${GBASEDIR}
        GUESDIR=${RUNDIR0}/${PDATE}
	fi
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
       ln -fs $GBASEDIR/sigf$fh rb_sigf$fh
       ln -fs $GBASEDIR/sfcf$fh rb_sfcf$fh
     fi
     if [ do$P2R = doyes ] ; then
       if [ do$CWBGFS = doyes ] ; then
         ln -fs $GBASEDIR/otgb2_0$fh rb_pgbf$fh
       else
         ln -fs $GBASEDIR/pgbf$fh rb_pgbf$fh
       fi
     else
       if [ do$C2R = doyes ] ; then
         ln -fs $GBASEDIR/r_sig.f$fh rb_sigf$fh
         ln -fs $GBASESFCDIR/r_sfc.f$fh rb_sfcf$fh
       fi
     fi
     if [ do$NEWSST = do.TRUE. ] ; then
       #ln -fs $BASEDIR/sstf$fh rb_sstf$fh
       slag=`expr $fh + $SSTLAG`
       cymdh=`${UTLDIR}/ndate $slag ${SDATE}`
       cyyyy=`echo ${cymdh} | cut -c1-4`
       cymd=`echo ${cymdh} | cut -c1-8`
       if [ ! -f ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt ] ; then
         exit 99
       fi
       ln -fs ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt himsst.txt
     fi
    $USHDIR/rinp.sh $NEST $fh || exit 4
    cp r_sigi  r_sig.f$fh
    cp r_sfci  r_sfc.f$fh
    cd $RUNDIR
    mem=`expr $mem + 1`
done #while [ $mem -le $MEMBER ]
      fi #IRES -eq 27
    fi #CYCLE -eq CYCLEDA
    #
    #  DA
    #
    $USHDIR/rensda.sh $CYCLE $CYCLEDA || exit 7
    if [ do$POSTTYPE = dosync ]; then
#      $USHDIR/rpgb_post.sh 00 || exit 5
      mem=0
      while [ $mem -le $MEMBER ];do
        if [ $mem -lt 10 ]; then
          mem=00$mem
        else
          mem=0$mem
        fi
        cd ${head}${mem}
        $USHDIR/rpgb_post.sh 00 || exit 5
        cd ..
        mem=`expr $mem + 1`
      done
    fi
#  fi # -d ${head}000
else
#
# control
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
#     ln -fs $BASEDIR/sigf$CDATE$CHOUR rb_sigf00
#     ln -fs $BASEDIR/sfcf$CDATE$CHOUR rb_sfcf00
     if [ do$G2R = doyes ] ; then
       ln -fs $BASEDIR/sigf00 rb_sigf00
       ln -fs $BASEDIR/sfcf00 rb_sfcf00
     fi
     if [ do$P2R = doyes ] ; then
       if [ do$CWBGFS = doyes ] ; then
         ln -fs $BASEDIR/otgb2_000 rb_pgbf00
       else
         ln -fs $BASEDIR/pgbf00 rb_pgbf00
       fi
     else
       if [ do$C2R = doyes ] ; then
         ln -fs $BASEDIR/r_sig.f00 rb_sigf00
         ln -fs $BASESFCDIR/r_sfc.f00 rb_sfcf00
       fi
     fi
     if [ do$NEWSST = do.TRUE. ] ; then
       #ln -fs $BASEDIR/sstf00 rb_sstf00
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
    $USHDIR/rinp.sh $NEST 00 || exit 4
    cp r_sigi  r_sig.f00
    cp r_sfci  r_sfc.f00
fi
if [ do$POSTTYPE = dosync ]; then
    $USHDIR/rpgb_post.sh 00 || exit 5
fi
#
fi #restart
#
# BGM rescaling (ensemble)
#
if [ do$BGM = doyes ]; then
  if [ $GLOBAL = GFS ] && [ $CYCLE -eq 1 ] && [ $IRES -eq 27 ]; then
    cp $DISKUSR/exp/$EXPN/$SAMPLETXT .
    NSAMPLE=`expr $MEMBER \* 2`
    $PYENV $UTLDIR/random_sample.py $SAMPLETXT $NSAMPLE > pdate.txt || exit 6
  fi
mem=1
while [ $mem -le $MEMBER ];do
  if [ $GLOBAL = GFS ] && [ $CYCLE -eq 1 ] && [ $IRES -eq 27 ]; then
    #cp $DISKUSR/exp/$EXPN/pdate.txt pdate.txt
    irow=`expr 2 \* $mem - 1`
    PDATE1=`cat pdate.txt | awk '{if(NR == '$irow'){print $1}}'`
    irow=`expr $irow + 1`
    PDATE2=`cat pdate.txt | awk '{if(NR == '$irow'){print $1}}'`
    export PDATE1 PDATE2
  fi
  if [ $mem -lt 10 ]; then
    mem=00$mem
  else
    mem=0$mem
  fi
if [ -s ${head}${mem}/r_sigi -a -s ${head}${mem}/r_sigitdt -a -s ${head}${mem}/r_sfci ] ; then
  echo 'Restart file exists'
else
  if [ $IRES -eq 27 ];then #rescaling
    $USHDIR/raddprtb.sh $CYCLE $mem || exit 7
  else #downscaling
    GBASEDIR=${BASEDIR0}/${SDATE}/${head}${mem}
    GBASESFCDIR=${GBASEDIR}
    GUESDIR=${RUNDIR0}/${SDATE}/${head}${mem}
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
      ln -fs $GBASEDIR/sigf00 rb_sigf00
      ln -fs $GBASEDIR/sfcf00 rb_sfcf00
    fi
    if [ do$P2R = doyes ] ; then
      if [ do$CWBGFS = doyes ] ; then
        ln -fs $GBASEDIR/otgb2_000 rb_pgbf00
      else
        ln -fs $GBASEDIR/pgbf00 rb_pgbf00
      fi
    else
      if [ do$C2R = doyes ] ; then
        ln -fs $GBASEDIR/r_sig.f00 rb_sigf00
        ln -fs $GBASESFCDIR/r_sfc.f00 rb_sfcf00
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
    $USHDIR/rinp.sh $NEST 00 || exit 4
    cp r_sigi  r_sig.f00
    cp r_sfci  r_sfc.f00
    cd $RUNDIR
  fi #IRES -eq 27
  if [ do$POSTTYPE = dosync ]; then
    cd ${head}${mem}
    $USHDIR/rpgb_post.sh 00 || exit 5
    cd ..
  fi
fi #restart
  mem=`expr $mem + 1`
done #while [ $mem -le $MEMBER ]
fi #doBGM=doyes
#
fi #doDA=doyes
#######################################
# Ensemble Forecast loop
########################################
if [ do$BP = dowbp ] && [ $GLOBAL = GFS ] && [ $IRES -eq 27 ]; then
  # Base field perturbation
  cp $DISKUSR/exp/$EXPN/$SAMPLETXT .
  NSAMPLE=`expr $MEMBER \* 2`
  $PYENV $UTLDIR/random_sample.py $SAMPLETXT $NSAMPLE > pdatebase.txt
  $USHDIR/raddprtbbase.sh || exit 5
fi
mem=0
while [ $mem -le $MEMBER ];do
  if [ $mem -eq 0 ]; then ## control
    if [ do$DA = doyes ]; then
    cd $RUNDIR/${head}000
    else
    cd $RUNDIR
    fi
    if [ $IRES -lt 27 ];then
    export BASEDIR=$base_dir
    export BASESFCDIR=$BASEDIR
    fi
  else ## member
    if [ $mem -lt 10 ]; then
      mem=00$mem
    else
      mem=0$mem
    fi
    cd $RUNDIR/${head}${mem}
    if [ do$BP = dowbp ]; then
      export BASEDIR=${base_dir}/${mem}
      export BASESFCDIR=$BASEDIR
    fi
    if [ $IRES -lt 27 ];then
    export BASEDIR=${base_dir}/${HEAD}${mem}
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
  hh=$hx
  if [ $hx -lt 10 ];then hx=0$hx;fi
  hhr=`expr $h + 0`
  while [ $hhr -le $hx ]; do
       if [ $hhr -lt 10 ]; then hhr=0$hhr; fi
         rfti=`$UTLDIR/ndate $hhr $CDATE$CHOUR`
       if [ do$G2R = doyes ] ; then
         ln -fs $BASEDIR/sigf$hhr rb_sigf$hhr
         ln -fs $BASEDIR/sfcf$hhr rb_sfcf$hhr
       fi
       if [ do$P2R = doyes ] ; then
         if [ do$CWBGFS = doyes ] ; then
           if [ $hhr -lt 100 ] ; then hhrr=0$hhr ; fi
           ln -fs $BASEDIR/otgb2_$hhrr rb_pgbf$hhr
         else
           ln -fs $BASEDIR/pgbf$hhr rb_pgbf$hhr
         fi
       else
         if [ do$C2R = doyes ] ; then
           ln -fs $BASEDIR/r_sig.f$hhr rb_sigf$hhr
           ln -fs $BASESFCDIR/r_sfc.f$hhr rb_sfcf$hhr
         fi
       fi
       if [ do$NEWSST = do.TRUE. ] ; then
         #ln -fs $BASEDIR/sstf$hhr rb_sstf$hhr
         slag=`expr $hhr + $SSTLAG`
         cymdh=`${UTLDIR}/ndate $slag ${SDATE}`
         cyyyy=`echo ${cymdh} | cut -c1-4`
         cymd=`echo ${cymdh} | cut -c1-8`
         if [ ! -f ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt ] ; then
           exit 99
         fi
         ln -fs ${WORKUSR}/DATA/himsst/${cyyyy}/him_sst_pac_D${cymd}.txt himsst.txt
       fi
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
    #timeout 1800 $USHDIR/rfcst.sh $hx || exit 10
    $USHDIR/rfcst.sh $hx || exit 10

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
mem=`expr $mem + 1`
done  ###### end of while member loop

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
CYCLE=`expr $CYCLE + 1`
done #while [ CYCLE -le CYCLEMAX ]
