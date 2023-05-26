#!/bin/sh
set -e
pbdir=/zdata/grmsm/work/DATA/gdas
obsdir=/zdata/grmsm/work/dpac/obs
bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/build/obs
#bindir=/home/nakashita/Development/grmsm/MSM-Tactical/dpac/builddev/obs
adate=${1:-2022061812}
lmin=-30
rmin=30
lprep=F
iq=
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
echo $cyyyy $cmm $cdd $chh
icmm=`expr $cmm + 0`
icdd=`expr $cdd + 0`
ichh=`expr $chh + 0`
set -e
cat <<EOF >decode.nml
&param_decode
 atime=${yyyy},${imm},${idd},${ihh},0
 ctime=${cyyyy},${icmm},${icdd},${ichh},0
 lmin=${lmin},
 rmin=${rmin},
 lpreproc=${lprep},
 iwnd=,
 iq=${iq},
 inf='prepbufr.in',
 config='config',
&end
EOF
cat decode.nml
cat <<EOF >config
LATS 42 23
LONS 113 135
PARM P Q T U V
TYPE ADPUPA ADPSFC SFCSHP
VTMP FALSE

EOF
cat config
ln -fs ${bindir}/decode_prepbufr .
ln -fs  $pbdir/${cyyyy}${cmm}/${cyyyy}${cmm}${cdd}/gdas.t${chh}z.prepbufr.nr prepbufr.in
./decode_prepbufr < decode.nml | tee decode_prepbufr.log
rm prepbufr.in

cd ..
mv tmp/*.dat .
mv tmp/decode_prepbufr.log .
#for f in $(ls *.dat);do
#  mv $f ../${f%.dat}.noship.dat
#done
#mv decode_prepbufr.log ../
echo "END"
