#!/bin/sh
RADSIM_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/radsim-3.1
RADSIM_SCDIR=${RADSIM_DIR}/src/scripts
RADSIM_BINDIR=${RADSIM_DIR}/test/bin
RTTOV_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/rttov132
MODELDATADIR=${HOME}/mnt/methane/work
#OBSDATADIR=${HOME}/Development/dpac/himawari/hmwr_nc
OBSDATADIR=${HOME}/mnt/ensens2/jaxa/jma/netcdf

sdate=2022061900
fhour=6
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
## model data
fh=`printf '%0.2d' $fhour`
datadir=${MODELDATADIR}/msm2msm3_jpn/${sdate}
atmname=r_sig.f$fh
flxname=r_flx.f$fh
sfcname=r_sfc.f$fh
rm -f $atmname $flxname $sfcname
ln -s $datadir/$atmname .
ln -s $datadir/$flxname .
ln -s $datadir/$sfcname .
model_filetype=9
## satellite information
platform=himawari
satid=8
inst=ahi
channel=8
options="_7gas"
COEFDIR=rttov13pred54L
#COEFDIR=rttov9pred54L
### prepare observation location file
#obsinfile=radsim_geo_obs_data_${platform}_${satid}_${inst}_5500x5500.nc
obsdir=${OBSDATADIR}/${vyyyy}${vmm}/${vdd}
#ext=_r14_FLDK.02701_02601
ext=_R21_FLDK.02401_02401
#ext=_R21_FLDK.06001_06001
obsinfile=NC_H08_${vyyyy}${vmm}${vdd}_${vhh}00${ext}.nc
obsoutfile=radsim_geo_obs_data_${obsinfile%*.nc}.txt
rm $obsoutfile
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
    --min_lat 24.0 --max_lat 35.0 \
    --min_lon 120.0 --max_lon 135.0 \
    || exit 1
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
#outfile=${outfile%.nc}_zoom.nc
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
    --ir_addclouds T \
    --write_latlon T \
    --write_profile T \
    --nthreads $num_threads \
    --output_mode 2 \
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
#    --ozone_data T \
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
    --output_mode 3 \
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
fi
ls -ltr
## create plot
$RADSIM_SCDIR/radsim_plot_example.py \
    --filename $outfile \
    --vmin 190 --vmax 240 --cmap gray_r ## band8
#    --vmin 190 --vmax 310 --cmap gray_r  ## band14
#    --vmin 240 --vmax 320 --cmap gray_r ## band7
#    --lonmin 113.3 --lonmax 135.3 --latmin 23.5 --latmax 41.8
#fi
### debug
#python plot_skewT.py $sdate $outfile
#mv ${outfile} data/
#mv ${outfile%.nc}.log log/
#mv ${outfile}*.png fig/