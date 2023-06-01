#!/bin/sh
set -ex
pbdir=/zdata/grmsm/work/DATA/gdas
obsdir=${obsdir:-/zdata/grmsm/work/dpac/obs}
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/obs
utldir=${UTLDIR:-/home/nakashita/Development/grmsm/MSM-Tactical/sys/utl}
rundir=${RUNDIR}
adate=${1:-2022061812}
lmin=${2:--60}
rmin=${3:-60}
prep=${PREP}
if [ do$prep = do_prep ];then
lprep=T
iq=1
elif [ do$prep = do_preprh ];then
lprep=T
iq=2
else
lprep=F
iq=
fi
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
set +e
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
set -e

wdir=${obsdir}/${adate}
mkdir -p $wdir/tmp
cd $wdir/tmp

# get file date
set +e
if [ $ihh -ge 21 ] || [ $ihh -lt 3 ]; then #00UTC
  dhr=`expr 0 - $ihh`
elif [ $ihh -lt 9 ]; then #06UTC
  dhr=`expr 6 - $ihh`
elif [ $ihh -lt 15 ]; then #12UTC
  dhr=`expr 12 - $ihh`
else #18UTC
  dhr=`expr 18 - $ihh`
fi
if [ $dhr -lt -3 ]; then dhr=`expr 24 + $dhr`; fi
if [ $dhr -ge 0 ]; then dhr=+$dhr; fi
cdate=`date -j -f "%Y%m%d%H" -v${dhr}H +"%Y%m%d%H" "$adate"`
cyyyy=`echo ${cdate} | cut -c1-4`
cmm=`echo ${cdate} | cut -c5-6`
cdd=`echo ${cdate} | cut -c7-8`
chh=`echo ${cdate} | cut -c9-10`
cym=`echo ${cdate} | cut -c1-6`
cymd=`echo ${cdate} | cut -c1-8`
echo $cyyyy $cmm $cdd $chh
icmm=`expr $cmm + 0`
icdd=`expr $cdd + 0`
ichh=`expr $chh + 0`
set -e

infhead=prepbufr.637701
cat <<EOF >decode.nml
&param_decode
 infhead='${infhead}',
 atime=${yyyy},${imm},${idd},${ihh},0
 ctime=${cyyyy},${icmm},${icdd},${ichh},0
 lmin=${lmin},
 rmin=${rmin},
 lpreproc=${lprep},
 iwnd=,
 iq=${iq},
&end
EOF
cat decode.nml
ln -fs ${bindir}/decode_prepbufr_ascii .
for f in $(ls $pbdir/${cym}/${cymd}/${infhead}.${cymd}.t${chh}z.*);do
otype=${f##*.}
echo $otype
ln -fs $pbdir/${cym}/${cymd}/${infhead}.${cymd}.t${chh}z.${otype} ${infhead}.${otype}
done
./decode_prepbufr_ascii < decode.nml | tee decode_prepbufr_ascii.log
rm ${infhead}.*

cd ..
mv tmp/*.dat .
mv tmp/decode_prepbufr_ascii.log .
echo "END decode_prepbufr_ascii"
