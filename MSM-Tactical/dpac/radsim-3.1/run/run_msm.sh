#!/bin/sh
RADSIM_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/radsim-3.1
RADSIM_SCDIR=${RADSIM_DIR}/src/scripts
RADSIM_BINDIR=${RADSIM_DIR}/run/bin
RTTOV_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/rttov132
MODELDIR=${HOME}/Development/grmsm/MSM-Tactical
#MODELDATADIR=${HOME}/mnt/methane/work/msm2msm3_dad3
#EXPN=daprepbufrall.scl.iter1.l100.v4.rp80
##EXPN=${EXPN}.spinup
##EXPN=${EXPN}.nhydupd
#EXPN=${EXPN}.no4ship
MODELDATADIR=${HOME}/mnt/methane/work/msm2msm3_jpn
EXPN=
#OBSDATADIR=${HOME}/Development/dpac/himawari/hmwr_nc
OBSDATADIR=${HOME}/mnt/ensens2/jaxa/jma/netcdf

wdir=`basename $MODELDATADIR`
echo $wdir

member=0
#for member in $(seq 0 40);do
sdate=2022061800
edate=2022061800
while [ $sdate -le $edate ];do
fhour=3
#for fhour in $(seq 1 3); do
yyyy=`echo $sdate | cut -c1-4`
mm=`echo $sdate | cut -c5-6`
dd=`echo $sdate | cut -c7-8`
hh=`echo $sdate | cut -c9-10`
vdate=`date -j -f "%Y%m%d%H" -v+${fhour}H +"%Y%m%d%H" "$sdate"`
vyyyy=`echo $vdate | cut -c1-4`
vmm=`echo $vdate | cut -c5-6`
vdd=`echo $vdate | cut -c7-8`
vhh=`echo $vdate | cut -c9-10`
## OpenMP
num_threads=8
export OMP_NUM_THREADS=$num_threads
## create outdir
#outdir=${sdate}/$EXPN
outdir=${sdate}/${wdir}/$EXPN
if [ ! -d $outdir ]; then
mkdir -p $outdir
fi
cd $outdir
## model data
fh=`printf '%0.2d' $fhour`
if [ $member = mean ]; then
mem3="mean"
else
mem3=`printf '%0.3d' $member`
fi
if [ -z $EXPN ]; then
datadir=${MODELDATADIR}/${sdate}
else
datadir=${MODELDATADIR}/${sdate}/${EXPN}.${mem3}
fi
atmname=r_sig.f$fh
flxname=r_flx.f$fh
sfcname=r_sfc.f$fh
rm -f $atmname $flxname $sfcname fort.43
ln -s $datadir/$atmname .
ln -s $datadir/$flxname .
ln -s $datadir/$sfcname .
ln -s $MODELDIR/sys/fix/global_cldtune.f77 fort.43
model_filetype=9
## satellite information
platform=himawari
satid=8
inst=ahi
channel=13
options="_7gas"
COEFDIR=rttov13pred54L
#COEFDIR=rttov9pred54L
### prepare observation location file
#obsinfile=radsim_geo_obs_data_${platform}_${satid}_${inst}_5500x5500.nc
obsdir=${OBSDATADIR}/${vyyyy}${vmm}/${vdd}
#ext=_r14_FLDK.02701_02601 #Jpn, 2-km
#ext=_R21_FLDK.02401_02401 #Full disk, 5-km
ext=_R21_FLDK.06001_06001 #Full disk, 2-km
obsinfile=NC_H08_${vyyyy}${vmm}${vdd}_${vhh}00${ext}.nc
obsoutfile=radsim_geo_obs_data_${obsinfile%*.nc}.txt
if [ ! -f $obsoutfile ]; then
imm=`expr $vmm + 0`
idd=`expr $vdd + 0`
ihh=`expr $vhh + 0`
ln -s ${obsdir}/${obsinfile} .
$RADSIM_SCDIR/radsim_geo_obs.py \
    --data_file $obsinfile \
    --output_file $obsoutfile \
    --date_time $vyyyy $imm $idd $ihh 0 \
    --fixed_date_time T \
    --sat_lon 140.7 \
    --max_lat 33.0 --min_lat 30.0 \
    --min_lon 131.5 --max_lon 134.5 \
    || exit 1
    #--max_lat 33.8 --min_lat 29.1 \
    #--min_lon 124.8 --max_lon 132.1 \
    #--min_lat 5.0 --max_lat 55.0 \
    #--min_lon 110.0 --max_lon 160.0 \
    #--min_lat 27.0 --max_lat 37.0 \
    #--min_lon 130.0 --max_lon 140.0 \
    #--min_lat 30.0 --max_lat 31.5 \
    #--min_lon 130.0 --max_lon 132.0 \
    #--min_lat 23.5 --max_lat 41.8 \
    #--min_lon 113.3 --max_lon 135.3 \
    #--sat_lon 140.7 --scan_time 8.966 \
    #--sat_lon 140.7 --scan_time 1.732 \
rm $obsinfile
fi
## create config file and run
ch=`printf '%0.2d' $channel`
outfile=radsim-${model_filetype}-${platform}_${satid}_${inst}${options}_ch${ch}-${sdate}00+${fh}h_ircld.nc
if [ ! -z $EXPN ]; then
outfile=${outfile%.nc}.${mem3}.nc
#outfile=${outfile%.nc}_zoom.nc
fi
#if [ ! -f $outfile ]; then
rm $outfile radsim_test.nml
if [ -z $options ]; then
$RADSIM_SCDIR/radsim_run.py \
    --config_file radsim_test.nml \
    --write_config_only F \
    --radsim_bin_dir $RADSIM_BINDIR \
    --model_datafile $atmname \
    --model_ancil_datafile $flxname \
    --model_ancil2_datafile $sfcname \
    --model_filetype $model_filetype \
    --rttov_coeffs_dir $RTTOV_DIR/rtcoef_rttov13/$COEFDIR \
    --platform $platform --satid $satid \
    --inst $inst --channels $channel \
    --output_file ${outfile} \
    --obs_datafile $obsoutfile \
    --addsolar T \
    --ozone_data T \
    --ir_addclouds T \
    --write_latlon T \
    --write_profile T \
    --nthreads $num_threads \
    --output_mode 2 \
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
else
$RADSIM_SCDIR/radsim_run.py \
    --config_file radsim_test.nml \
    --write_config_only F \
    --radsim_bin_dir $RADSIM_BINDIR \
    --model_datafile $atmname \
    --model_ancil_datafile $flxname \
    --model_ancil2_datafile $sfcname \
    --model_filetype $model_filetype \
    --rttov_coeffs_dir $RTTOV_DIR/rtcoef_rttov13/$COEFDIR \
    --platform $platform --satid $satid \
    --inst $inst --channels $channel \
    --rttov_coeffs_options $options \
    --output_file ${outfile} \
    --obs_datafile $obsoutfile \
    --addsolar T \
    --ir_addclouds T \
    --ozone_data T \
    --write_latlon T \
    --write_profile T \
    --nthreads $num_threads \
    --output_mode 2 \
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
fi
#fi
ls -ltr
## create plot
$RADSIM_SCDIR/radsim_plot_example.py \
    --filename $outfile \
    --vmin 190 --vmax 300 --cmap gray_r --cltop T ## band13
#    --vmin 190 --vmax 260 --cmap gist_ncar_r  ## band10
#    --vmin 240 --vmax 320 --cmap gray_r ## band7
#    --vmin 190 --vmax 240 --cmap gray_r ## band8
#    --vmin 190 --vmax 310 --cmap gray_r  ## band14
#    --boxlonmin 131.5 --boxlonmax 134.5 --boxlatmin 30.0 --boxlatmax 33.0 \
#    --lonmin 113.3 --lonmax 135.3 --latmin 23.5 --latmax 41.8
### debug
#python plot_skewT.py $sdate $outfile
#mv ${outfile} data/
#mv ${outfile%.nc}.log log/
#mv ${outfile}*.png fig/
cd -
#done #fhour
sdate=`date -j -f "%Y%m%d%H" -v+1H +"%Y%m%d%H" "$sdate"`
done #[ sdate -le edate ]
#done #member
