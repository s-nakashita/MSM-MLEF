#!/bin/sh
#
syear=`echo $SDATE |cut -c1-4`               ## starting year
smonth=`echo $SDATE |cut -c5-6`                ## starting month
sday=`echo $SDATE |cut -c7-8`                  ## starting day
shour=`echo $SDATE |cut -c9-10`               ## starting hour
CDATE=$syear$smonth$sday
CHOUR=$shour
cycle=00Z
dayend=01
hourend=00
#-----------------------------------------------
#define directories
rmm=$smonth
ryyyy=$syear
if [ $sday -gt 15 ]; then
  rmm=`expr $smonth + 1`
  if [ $rmm -gt 12 ]; then
     rmm=`expr $rmm - 12`
     ryyyy=`expr $ryyyy + 1`
  fi
  if [ $rmm -lt 10 ]; then
     rmm=0$rmm
  fi
fi
#
RUNDIR=${RUNDIR:-$TEMP/$ryyyy$rmm/r${SDATE}.$CASE}
cd $RUNDIR || exit
#
RESVER=${RESVER:-$levs}
RSFC_MERGE=${RSFC_MERGE:-yes}
INCBASE=${INCBASE:-6}
PRTHOUR=${PRTHOUR:-6}
yearend=$ryyyy
monthend=`expr $rmm + $LENMON`
if [ $monthend -gt 12 ]; then
    monthend=`expr $monthend - 12`
    yearend=`expr $yearend + 1`
fi
if [ $monthend -lt 10 ]; then
     monthend=0$monthend
fi
eidate=$yearend$monthend$dayend$hourend
end_hr=`$UTLDIR/nhour $eidate $CDATE$CHOUR`
ENDHOUR=${ENDHOUR:-$END_HR}
#### define fcst parameters ######

# rsm_pgb
if [ $POSTTYPE != sync ] ; then
  FH=0
  FEND=$ENDHOUR
#
  while [ $FH -le $FEND ]; do
       if [ $FH -lt 10 ]; then FH=0$FH; fi
       $USHDIR/rpgb_post.sh ${FH} || exit 8
       FH=`expr $FH + $PRTHOUR`
  done
fi

########################################
#------------------------------------------------------
# Background fields: global file to regional input
#------------------------------------------------------
  #
  FH=00
########################################
# Forecast loop
########################################
h=$FH

#
#  r_pgb average
#
mc=0
mysdate=`$UTLDIR/ndate 24 $CDATE$CHOUR`
if [ $sday -gt 15 ]; then
   mysdate=${ryyyy}${rmm}0100
fi  
fhs=`$UTLDIR/nhour $mysdate $CDATE$CHOUR`
while [ $mc -lt $LENMON ] 
do
  yend=$ryyyy
  mend=`expr $rmm + $mc + 1`
  if [ $mend -gt 12 ]; then
      mend=`expr $mend - 12`
      yend=`expr $yend + 1`
  fi
  if [ $mend -lt 10 ]; then
       mend=0$mend
  fi
  dend=01
  hend=00
  myedate=$yend$mend$dend$hend
  myyyyy=`echo $mysdate |cut -c1-4`
  mymm=`echo $mysdate |cut -c5-6`
  fhe1=`$UTLDIR/nhour $myedate $CDATE$CHOUR`
  if [ $PRTHOUR -lt 10 ] ; then PRTHOUR=0$PRTHOUR; fi
  fhe=`expr $fhe1 - $PRTHOUR`
  $USHDIR/rpgb_avrg.sh r_pgb $fhs $fhe $PRTHOUR r_pgb.$myyyyy$mymm.avrg.grib|| exit 8
  $UTLDIR/force_grib_date_mon ./r_pgb.$myyyyy$mymm.avrg.grib $myyyyy${mymm}0100

  cycle=00
  DAYHOUR=24
  while [ $cycle -lt 24 ]; do
        fhs00=`expr $fhs + $cycle`
        fhe00=`expr $fhe1 + $cycle - $DAYHOUR`
        $USHDIR/rpgb_avrg.sh r_pgb $fhs00 $fhe00 $DAYHOUR r_pgb.$myyyyy${mymm}_${cycle}Z.avrg.grib|| exit 8
        $UTLDIR/force_grib_date_mon ./r_pgb.$myyyyy${mymm}_${cycle}Z.avrg.grib $myyyyy${mymm}01${cycle}
        cycle=$((cycle+$PRTHOUR))
        if [ $cycle -lt 10 ] ; then cycle=0$cycle; fi
  done
  mysdate=$myedate
  fhs=$fhe1
  mc=`expr $mc + 1`
done
lastavrg=r_pgb.$myyyyy$mymm.avrg.grib
#cp $RUNDIR/*.grib $AVGDIR/.

#$USHDIR/extract_rpgb.sh $RUNDIR $ENDHOUR subset.grb
#cp $RUNDIR/subset.grb $AVGDIR
#cd $AVGDIR

#tarfile
if [ $tar2hpss -eq 1 ]; then

  /u/wx20wa/bin/hpsstar mkd /hpssuser/g01/wx20wa/rsm2004/
  /u/wx20wa/bin/hpsstar mkd /hpssuser/g01/wx20wa/rsm2004/$ryyyy$rmm
  /u/wx20wa/bin/hpsstar mkd /hpssuser/g01/wx20wa/rsm2004/$ryyyy$rmm/$syear$smonth$sday$shour
  tardir=/hpssuser/g01/wx20wa/rsm2004/$ryyyy$rmm/$syear$smonth$sday$shour

  if [ $tarrsmfl -eq 1 ]; then
   cd $RUNDIR
     for f in r_pgb  avrg subset
     do
       tarfile=${syear}${smonth}${sday}${shour}_${f}.tar
       if [ $f = r_pgb ]; then
         tar -cvf $tarfile ${f}.f?? ${f}.f??? ${f}.f????
         oldsize=`ls -l $tarfile |awk '{print $5}'`
         filedir=$RUNDIR/filedir
                                                                                
pftp hpsscore 4021 >$filedir<<EOF
cd $tardir
site setcos 231
put $tarfile
dir $tarfile
bye
EOF
         hpsssize=`cat $filedir | grep "wx20wa" | grep $tarfile |awk '{print $5}'`
         if [ $hpsssize -ge $oldsize ]; then
           echo "rm $tarfile"
           rm $tarfile
         fi

       else
         tar -cvf $tarfile *${f}*
         oldsize=`ls -l $tarfile |awk '{print $5}'`
         /u/wx20wa/bin/hpsstar put $tardir/$tarfile  $tarfile
         filedir=`/u/wx20wa/bin/hpsstar dir $tardir/$tarfile`
         hpsssize=`echo $filedir | awk '{print $8}'`
         if [ $hpsssize -ge $oldsize ]; then
           echo "rm $tarfile"
           rm $tarfile
         fi
       fi
     done
  fi
fi
#
echo "average done!"

# send subset to NOMAD1
#remotedir=/home/ftp/pub/rsm/7monthrun
#cd $RUNDIR
#cp $RUNDIR/subset.grb $AVGDIR
#prod_size=`ls -l subset.grb |awk '{print $5}'`
#ftplog=$RUNDIR/ftplog
#rm $ftplog
#ftp -i -v 172.16.11.34 >>$ftplog  <<EOF
#cd $remotedir
#bin
#put subset.grb subset_${syear}${smonth}${sday}${shour}.grb
#dir subset_${syear}${smonth}${sday}${shour}.grb
#bye
#EOF

#newsize1=`grep "subset_${syear}${smonth}${sday}${shour}.grb" $ftplog | tail -n 1 | awk '{print $5}'`
#if [ "$prod_size" != "$newsize1" ]; then
#   mail -s "172.16.11.34, rsm, subset.grb transfer data wrong" jun.wang@noaa.gov <$ftplog
#   exit 8
#else
                                                                                
# cleanup
################## close rm temporarily
#rm $ftplog
                                                                                
#echo "finish in ftplog"
#fi

#cd $AVGDIR
#output=$AVGDIR/output
#filenum=`ls -l |wc -l`
###if [ $filenum -eq 79 ]; then
### add GSMsubset.grb
#if [ $filenum -ge 78 ]; then
#  if [ -s $AVGDIR/$lastavrg ]; then
#    if [ -s $AVGDIR/avgpgb$myyyyy$mymm -a -s $AVGDIR/avgflx$myyyyy$mymm ]; then
#      /u/wx20wa/bin/hpsstar dir /hpssuser/g01/wx20wa/rsm2004/$ryyyy$rmm/$syear$smonth$sday$shour >$output
#       lines=`cat $output |wc -l`
#       if [ $lines -eq 26 ]; then
#           echo "rm -r $RUNDIR $BASEDIR"
#           rm -r $RUNDIR $BASEDIR
#           rm $EXPDIR/rsm_avrg_${SDATE}${CASE}.q
#           rm $EXPDIR/rsm_${SDATE}${CASE}.q
#       fi
#       rm $output
#    fi
#   fi
#fi
echo "rsm post completed"

exit
