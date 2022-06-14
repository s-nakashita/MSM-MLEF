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

#-----------------------------------------------
# set running space
#-----------------------------------------------
RUNDIR=${RUNDIR:-$TEMP/$ryyyy$rmm/r${SDATE}.$CASE}
base_dir=$TEMP/$ryyyy$rmm/g${SDATE}.$CASE
BASEDIR=${BASEDIR:-$base_dir}
mkdir -p $RUNDIR
cd $RUNDIR || exit 1
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
 
 &END
EOF
#
  cat rsmparm rsmlocation > rfcstparm
  cp rsmlocation $RUNDIR
  cp $STTPRM $RUNDIR/station.parm
#
##########################################################
#
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
# RSM INITIAL forecast
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
         ln -fs $BASEDIR/r_sfc.f00 rb_sfcf00
       fi
     fi
     if [ do$NEWSST = do.TRUE. ] ; then
       ln -fs $BASEDIR/sstf00 rb_sstf00
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
fi
#######################################
# Forecast loop
########################################
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
           ln -fs $BASEDIR/r_sfc.f$hhr rb_sfcf$hhr
         fi
       fi
       if [ do$NEWSST = do.TRUE. ] ; then
         ln -fs $BASEDIR/sstf$hhr rb_sstf$hhr
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
    echo "Forecast starting from hour $h..." >>stdout
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
      mv r_sigf$hr   r_sig.f$hr
      mv r_sfcf$hr   r_sfc.f$hr
      mv r_flxf$hr   r_flx.f$hr
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
#
# panel plot (hr>=3)
#
        fh=`expr $hr + 0`
        if [ $fh -ge 3 ];then
        $DISKUSR/nclscripts/plot_panel6_each.sh ${IRES} ${fh} || exit 15
        fi
        hr=`expr $hr + $PRTHOUR`
      done
#
      if [ $hx -eq $ENDHOUR ]; then
        if [ $hx -lt 10 ];then hx=0$hx;fi
        mv r_sigf$hx   r_sig.f$hx
        mv r_sfcf$hx   r_sfc.f$hx
        mv r_flxf$hx   r_flx.f$hx
        $USHDIR/rpgb_post.sh $hx || exit 16
      fi

    fi # r_pgb dosync
###
  fi # end of RUNFCST 

  h=$hx
done  ###### end of while forecast loop

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

