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
# get computation boundary
$PYENV ${utldir}/setlatlon.py $IGRD $JGRD $RTRUTH $RDELX $RDELY $RBTMGRD $RLFTGRD $RCENLAT $RCENLON
lats=`head -n 1 rlat.txt`
latn=`tail -n 1 rlat.txt`
lonw=`head -n 1 rlon.txt`
lone=`tail -n 1 rlon.txt`
cat <<EOF >config
LATS $latn $lats
LONS $lonw $lone
PARM P Q T U V
TYPE ADPUPA ADPSFC SFCSHP
VTMP FALSE

EOF
cat config

ln -fs ${bindir}/decode_prepbufr .
ln -fs  $pbdir/${cyyyy}${cmm}/${cyyyy}${cmm}${cdd}/gdas.t${chh}z.prepbufr.nr prepbufr.in
./decode_prepbufr < decode.nml | tee decode_prepbufr.log

cd ..
mv tmp/*.dat .
mv tmp/decode_prepbufr.log .
echo "END decode_prepbufr"
