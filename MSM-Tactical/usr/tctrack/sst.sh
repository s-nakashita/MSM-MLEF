#!/bin/sh
set -e
decode() {
  infile=$1
  var=$2
  lev=$3
  ofile=$4
  wgrib ${infile} | grep "${var}" | grep "${lev}" | wgrib ${infile} -i -ieee -nh -o ${ofile}
}

datadir="/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work"
init=${1:-2022082800} # initial date (input)
tcnum=${2:-11} # TC number
wdir=rsm2msm9
igrd=384 # size(lon)-1
jgrd=192 # size(lat)-1
endhour=120 # forecast length (hour)
inchour=24  # output interval (hour)
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
decode r_pgb.f${fh} ":TMP:" ":sfc:" tsfc.f${fh}.grd
decode r_pgb.f${fh} ":LAND:" ":sfc:" slmsk.f${fh}.grd
cat tsfc.f${fh}.grd slmsk.f${fh}.grd > r_sst.f${fh}.grd
rm tsfc.f${fh}.grd slmsk.f${fh}.grd
rm r_pgb.f${fh}
python ../../plot_sst.py $wdir $init $fh $tcnum
done
