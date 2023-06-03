#!/bin/sh
set -e
decode() {
  infile=$1
  var=$2
  lev=$3
  ofile=$4
  wgrib2 ${infile} | grep "${var}" | grep "${lev}" | wgrib2 ${infile} -i -no_header -ieee ${ofile}
}

datadir="/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work"
init=${1:-2022082800} # initial date (input)
tcnum=${2:-11} # TC number
wdir=DATA/gfs/rda
igrd=560 # size(lon)-1
jgrd=380 # size(lat)-1
endhour=120 # forecast length (hour)
inchour=3  # output interval (hour)
delm=25.0 #horizontal resolution (km)

dlat=0.25
dlon=0.25
clat1=-25
clat2=70
clon1=60
clon2=200

rm -rf tmp/$init
mkdir -p tmp/$init
cd tmp/$init
### Step 1 : get lat & lon information
params="4 $dlat $dlon $clat1 $clat2 $clon1 $clon2"
echo $params
python ../../setlatlon.py $igrd $jgrd $params
echo "lat"
wc -l rlat.txt
cat rlat.txt | head -n 5
echo ":"
cat rlat.txt | tail -n 5
echo "lon"
wc -l rlon.txt
cat rlon.txt | head -n 5
echo ":"
cat rlon.txt | tail -n 5

### Step 2 : crop SLP and UV850 from r_pgb.fNN
for fh in $(seq 0 ${inchour} ${endhour});do
if [ $fh -lt 10 ];then fh=0$fh; fi
ln -s ${datadir}/${wdir}/${init}/pgbf${fh} .
decode pgbf${fh} ":PRMSL:" ":mean sea level:" msl.f${fh}.grd
decode pgbf${fh} ":UGRD:" ":10 m above ground:" u10.f${fh}.grd
decode pgbf${fh} ":VGRD:" ":10 m above ground:" v10.f${fh}.grd
decode pgbf${fh} ":HGT:" ":850 mb:" z850.f${fh}.grd
decode pgbf${fh} ":UGRD:" ":850 mb:" u850.f${fh}.grd
decode pgbf${fh} ":VGRD:" ":850 mb:" v850.f${fh}.grd
decode pgbf${fh} ":HGT:" ":700 mb:" z700.f${fh}.grd
decode pgbf${fh} ":UGRD:" ":700 mb:" u700.f${fh}.grd
decode pgbf${fh} ":VGRD:" ":700 mb:" v700.f${fh}.grd
cat \
msl.f${fh}.grd u10.f${fh}.grd v10.f${fh}.grd \
z850.f${fh}.grd u850.f${fh}.grd v850.f${fh}.grd \
z700.f${fh}.grd u700.f${fh}.grd v700.f${fh}.grd > r_pgb.f${fh}.grd
rm \
msl.f${fh}.grd u10.f${fh}.grd v10.f${fh}.grd \
z850.f${fh}.grd u850.f${fh}.grd v850.f${fh}.grd \
z700.f${fh}.grd u700.f${fh}.grd v700.f${fh}.grd
rm pgbf${fh}
done
ls -ltr

### Step 3 : tracking
nlon=`expr $igrd + 1`
nlat=`expr $jgrd + 1`
python ../../track.py $wdir $nlon $nlat $inchour $endhour $delm $init $tcnum
