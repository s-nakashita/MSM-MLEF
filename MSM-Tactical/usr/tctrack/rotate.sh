#!/bin/sh
decode() {
  infile=$1
  var=$2
  lev=$3
  ofile=$4
  wgrib ${infile} | grep "${var}" | grep "${lev}" | wgrib ${infile} -i -ieee -nh -o ${ofile}
}
ires=9 # resolution [km]
if [ $ires -eq 27 ];then
wdir=rsm2rsm27_bvgfs
igrd=324 # size(lon)-1
jgrd=288 # size(lat)-1
endhour=48 # forecast length [hour]
inchour=3  # output interval [hour]
elif [ $ires -eq 9 ];then
wdir=rsm2msm9_bvgfs
igrd=384 # size(lon)-1
jgrd=324 # size(lat)-1
endhour=48 # forecast length [hour]
inchour=1  # output interval [hour]
fi
datadir=/Users/nakashita/mnt/methane/grmsm/work/$wdir
init=${1:-2022083000} # initial date (input)
tetype=dry
cntl=0
mean=0
bv=1

outdir=/Volumes/dandelion/GRMSMJob/rsm2msm9_bvgfs/$init
mkdir -p $outdir/rotate

rm -rf tmp/$init
mkdir -p tmp/$init
cd tmp/$init
### Step 1 : get lat & lon information
cat <<EOF > getparm.awk
BEGIN{}
{
    if(\$1 == "RTRUTH"){tmp=\$3;rtruth=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RDELX"){tmp=\$3;delx=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RDELY"){tmp=\$3;dely=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RCENLAT"){tmp=\$3;rlatc=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RCENLON"){tmp=\$3;rlonc=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RBTMGRD"){tmp=\$3;rbtmgrd=substr(tmp,0,length(tmp)-1)}
    if(\$1 == "RLFTGRD"){tmp=\$3;rlftgrd=substr(tmp,0,length(tmp)-1)}
}
END{print rtruth, delx, dely, rbtmgrd, rlftgrd, rlatc, rlonc}
EOF
ln -fs ${datadir}/${init}/rsmlocation .
params=$(awk -f getparm.awk rsmlocation)
echo $params
python ../../../grid_mercator.py $igrd $jgrd $params
cat rlat.txt | head -n 5
cat rlon.txt | head -n 5
echo "850 700 500 300 200" > rlev.txt
cat rlev.txt
if [ $cntl -eq 1 ]; then
## control
trackf=${outdir}/track/track${init}.txt
### Step 2 : crop necessary data from r_pgb.fNN
for fh in $(seq 0 ${inchour} ${endhour});do
if [ $fh -lt 10 ];then fh=0$fh; fi
ln -s ${datadir}/${init}/r_pgb.f${fh} .
# scalar
decode r_pgb.f${fh} ":PRMSL:" ":MSL:" msl.f${fh}.grd
catfiles=msl.f${fh}.grd
for v in TMP RH;do
for plev in $(cat rlev.txt);do
decode r_pgb.f${fh} ":${v}:" ":${plev} mb:" ${v}${plev}.f${fh}.grd
catfiles="${catfiles} ${v}${plev}.f${fh}.grd"
done
done
echo $catfiles
cat $catfiles > r_pgb_scl.f${fh}.grd
rm $catfiles 
# vector
catfiles=
#sfc
for v in UGRD VGRD;do
decode r_pgb.f${fh} ":${v}:" ":10 m above gnd:" ${v}10m.f${fh}.grd
catfiles="${catfiles} ${v}10m.f${fh}.grd"
done
for v in UGRD VGRD;do
for plev in $(cat rlev.txt);do
decode r_pgb.f${fh} ":${v}:" ":${plev} mb:" ${v}${plev}.f${fh}.grd
catfiles="${catfiles} ${v}${plev}.f${fh}.grd"
done
done
echo $catfiles
cat $catfiles > r_pgb_uv.f${fh}.grd
rm $catfiles
rm r_pgb.f${fh}
done
ls -ltr

### Step 3 : rotate binary files
nolon=1440
nolat=33
latmax=8.0
python ../../rotate_scalar.py $init $trackf $nolon $nolat $latmax || exit 10
python ../../rotate_uv.py $init $trackf $nolon $nolat $latmax || exit 11
python ../../create_netcdf.py || exit 12
mv np.nc $outdir/rotate/np_cntl.nc
rm np_scl.nc np_uv.nc r_pgb_scl.f*.grd r_pgb_uv.f*.grd
fi

if [ $bv -eq 1 ];then
## ensemble
member=10
for m in $(seq 1 $member);do
mem=`printf '%0.3d' $m`
echo $mem
mkdir -p $mem
cd $mem
cp ../rlat.txt .
cp ../rlon.txt .
cp ../rlev.txt .
trackf=${outdir}/track/bv${tetype}/track${init}m${mem}.txt
### Step 2 : crop necessary data from r_pgb.fNN
for fh in $(seq 0 ${inchour} ${endhour});do
if [ $fh -lt 10 ];then fh=0$fh; fi
ln -s ${datadir}/${init}/bv${tetype}${mem}/r_pgb.f${fh} .
# scalar
decode r_pgb.f${fh} ":PRMSL:" ":MSL:" msl.f${fh}.grd
catfiles=msl.f${fh}.grd
for v in TMP RH;do
for plev in $(cat rlev.txt);do
decode r_pgb.f${fh} ":${v}:" ":${plev} mb:" ${v}${plev}.f${fh}.grd
catfiles="${catfiles} ${v}${plev}.f${fh}.grd"
done
done
echo $catfiles
cat $catfiles > r_pgb_scl.f${fh}.grd
rm $catfiles 
# vector
catfiles=
#sfc
for v in UGRD VGRD;do
decode r_pgb.f${fh} ":${v}:" ":10 m above gnd:" ${v}10m.f${fh}.grd
catfiles="${catfiles} ${v}10m.f${fh}.grd"
done
for v in UGRD VGRD;do
for plev in $(cat rlev.txt);do
decode r_pgb.f${fh} ":${v}:" ":${plev} mb:" ${v}${plev}.f${fh}.grd
catfiles="${catfiles} ${v}${plev}.f${fh}.grd"
done
done
echo $catfiles
cat $catfiles > r_pgb_uv.f${fh}.grd
rm $catfiles
rm r_pgb.f${fh}
done
ls -ltr

### Step 3 : rotate binary files
nolon=1440
nolat=33
latmax=8.0
python ../../../rotate_scalar.py $init $trackf $nolon $nolat $latmax || exit 10
python ../../../rotate_uv.py $init $trackf $nolon $nolat $latmax || exit 11
python ../../../create_netcdf.py || exit 12
mv np.nc $outdir/rotate/np_${mem}.nc
rm np_scl.nc np_uv.nc r_pgb_scl.f*.grd r_pgb_uv.f*.grd
cd -
done
fi