#!/bin/sh
set -e
decode() {
  infile=$1
  var=$2
  lev=$3
  ofile=$4
  wgrib ${infile} | grep "${var}" | grep "${lev}" | wgrib ${infile} -i -ieee -nh -o ${ofile}
}

datadir="/Volumes/dandelion/GRMSMJob"
#datadir="/Users/nakashita/mnt/methane/grmsm/work"
init=${1:-2022082800} # initial date (input)
tcnum=${2:-11} # TC number
wdir=rsm2msm9_gfsz
#wdir=rsm2msm9_tparc
igrd=384 # size(lon)-1
jgrd=192 # size(lat)-1
#jgrd=324 # size(lat)-1
endhour=120 # forecast length (hour)
inchour=3  # output interval (hour)
delm=9.0 #horizontal resolution (km)

rm -rf tmp/$init
mkdir -p tmp/$init
cd tmp/$init
### Step 1 : get lat & lon information
cat <<EOF > getparm.awk
BEGIN{}
{
    if(\$1 == "RPROJ"){tmp=\$3;rproj=substr(tmp,0,length(tmp)-2)}
    if(\$1 == "RTRUTH"){tmp=\$3;rtruth=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RDELX"){tmp=\$3;delx=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RDELY"){tmp=\$3;dely=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RCENLAT"){tmp=\$3;rlatc=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RCENLON"){tmp=\$3;rlonc=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RBTMGRD"){tmp=\$3;rbtmgrd=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RLFTGRD"){tmp=\$3;rlftgrd=substr(tmp,0,length(tmp)-1)}
}
END{print rproj, rtruth, delx, dely, rbtmgrd, rlftgrd, rlatc, rlonc}
EOF
ln -fs ${datadir}/${wdir}/${init}/rsmlocation .
params=$(awk -f getparm.awk rsmlocation)
echo $params
python ../../setlatlon.py $igrd $jgrd $params
echo "lat"
cat rlat.txt | head -n 5
echo ":"
cat rlat.txt | tail -n 5
echo "lon"
cat rlon.txt | head -n 5
echo ":"
cat rlon.txt | tail -n 5

### Step 2 : crop SLP and UV850 from r_pgb.fNN
for fh in $(seq 0 ${inchour} ${endhour});do
if [ $fh -lt 10 ];then fh=0$fh; fi
ln -s ${datadir}/${wdir}/${init}/r_pgb.f${fh} .
decode r_pgb.f${fh} ":PRMSL:" ":MSL:" msl.f${fh}.grd
decode r_pgb.f${fh} ":UGRD:" ":10 m above gnd:" u10.f${fh}.grd
decode r_pgb.f${fh} ":VGRD:" ":10 m above gnd:" v10.f${fh}.grd
decode r_pgb.f${fh} ":HGT:" ":850 mb:" z850.f${fh}.grd
decode r_pgb.f${fh} ":UGRD:" ":850 mb:" u850.f${fh}.grd
decode r_pgb.f${fh} ":VGRD:" ":850 mb:" v850.f${fh}.grd
decode r_pgb.f${fh} ":HGT:" ":700 mb:" z700.f${fh}.grd
decode r_pgb.f${fh} ":UGRD:" ":700 mb:" u700.f${fh}.grd
decode r_pgb.f${fh} ":VGRD:" ":700 mb:" v700.f${fh}.grd
cat msl.f${fh}.grd u10.f${fh}.grd v10.f${fh}.grd \
z850.f${fh}.grd u850.f${fh}.grd v850.f${fh}.grd \
z700.f${fh}.grd u700.f${fh}.grd v700.f${fh}.grd > r_pgb.f${fh}.grd
rm msl.f${fh}.grd u10.f${fh}.grd v10.f${fh}.grd \
z850.f${fh}.grd u850.f${fh}.grd v850.f${fh}.grd \
z700.f${fh}.grd u700.f${fh}.grd v700.f${fh}.grd
rm r_pgb.f${fh}
done
ls -ltr

### Step 3 : tracking
nlon=`expr $igrd + 1`
nlat=`expr $jgrd + 1`
python ../../track.py $wdir $nlon $nlat $inchour $endhour $delm $init $tcnum
