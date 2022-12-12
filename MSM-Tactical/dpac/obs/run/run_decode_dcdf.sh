#!/bin/sh
set -e
dcddir=/zdata/grmsm/work/DATA/dcd
adate=2022061812
lmin=-60
rmin=60
yyyy=`echo ${adate} | cut -c1-4`
yy=`echo ${adate} | cut -c3-4`
mm=`echo ${adate} | cut -c5-6`
dd=`echo ${adate} | cut -c7-8`
hh=`echo ${adate} | cut -c9-10`
echo $yyyy $mm $dd $hh
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`

rm -rf tmp
mkdir -p tmp
cd tmp
cat <<EOF >decode.nml
&param_decode
 atime=${yyyy},${imm},${idd},${ihh},0
 lmin=${lmin},
 rmin=${rmin},
 lpreproc=.false.,
&end
EOF
cat decode.nml

tarf=dcd${yy}${mm}${dd}.tar.gz
cp ${dcddir}/${yyyy}/$tarf .
tar zxvf $tarf
../decode_dcdf < decode.nml

cd ..
mv tmp/*.dat .
echo "END"
