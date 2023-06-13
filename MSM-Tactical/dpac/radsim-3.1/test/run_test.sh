#!/bin/sh
RADSIM_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/radsim-3.1
RADSIM_SCDIR=${RADSIM_DIR}/src/scripts
RADSIM_BINDIR=${RADSIM_DIR}/bin
RTTOV_DIR=${HOME}/Development/grmsm/MSM-Tactical/dpac/rttov132
MODELDATADIR=/Volumes/dandelion/grib
#OBSDATADIR=${HOME}/Development/dpac/himawari/hmwr_nc
OBSDATADIR=${HOME}/mnt/ensens2/jaxa/jma/netcdf

sdate=2022061906
yyyy=`echo $sdate | cut -c1-4`
mm=`echo $sdate | cut -c5-6`
dd=`echo $sdate | cut -c7-8`
hh=`echo $sdate | cut -c9-10`
## OpenMP
num_threads=8
export OMP_NUM_THREADS=$num_threads
## model data
### JMA MSM GRIB
#datadir=${MODELDATADIR}/${yyyy}/${mm}/${dd}
#prsname=Z__C_RJTD_${sdate}0000_MSM_GPV_Rjp_L-pall_FH00-15_grib2.bin
#sfcname=Z__C_RJTD_${sdate}0000_MSM_GPV_Rjp_Lsurf_FH00-15_grib2.bin
#filename=${sdate}0000_FH00-15.grb
#model_filetype=8
### ERA5 GRIB
#datadir=` pwd `
#atmname=ec_${sdate}_era5_T0_atm.grb
#sfcname=ec_${sdate}_era5_T0_sfc.grb
#model_filetype=1
## ERA5 netcdf
datadir=` pwd `
atmname=ec_${sdate}_era5_T0_atm.nc
sfcname=ec_${sdate}_era5_T0_sfc.nc
model_filetype=5
## satellite information
platform=himawari
satid=8
inst=ahi
channel=14
options="_7gas"
COEFDIR=rttov13pred54L
#COEFDIR=rttov9pred54L
### prepare observation location file
#obsinfile=radsim_geo_obs_data_${platform}_${satid}_${inst}_5500x5500.nc
obsdir=${OBSDATADIR}/${yyyy}${mm}/${dd}
#ext=_r14_FLDK.02701_02601
ext=_R21_FLDK.02401_02401
#ext=_R21_FLDK.06001_06001
obsinfile=NC_H08_${yyyy}${mm}${dd}_${hh}00${ext}.nc
obsoutfile=radsim_geo_obs_data_${obsinfile%*.nc}.txt
rm $obsoutfile
if [ ! -f $obsoutfile ]; then
imm=`expr $mm + 0`
idd=`expr $dd + 0`
ihh=`expr $hh + 0`
ln -s ${obsdir}/${obsinfile} .
$RADSIM_SCDIR/radsim_geo_obs.py \
    --data_file $obsinfile \
    --output_file $obsoutfile \
    --date_time $yyyy $imm $idd $ihh 0 \
    --fixed_date_time T \
    --sat_lon 140.7 \
    --min_lat 24.0 --max_lat 35.0 \
    --min_lon 120.0 --max_lon 135.0 \
    || exit 1
    #--sat_lon 140.7 --scan_time 8.966 \
    #--sat_lon 140.7 --scan_time 1.732 \
    #--min_lat 23.5 --max_lat 41.8 \
    #--min_lon 113.3 --max_lon 135.3 \
rm $obsinfile
fi
## create config file and run
ch=`printf '%0.2d' $channel`
outfile=radsim-${model_filetype}-${platform}_${satid}_${inst}${options}_ch${ch}-${sdate}00.nc
#outfile=${outfile%.nc}_ozn.nc
outfile=${outfile%.nc}_ircld.nc
if [ ! -f $outfile ]; then
#rm $outfile radsim_test.nml
if [ -z $options ]; then
$RADSIM_SCDIR/radsim_run.py \
    --config_file radsim_test.nml \
    --write_config_only F \
    --radsim_bin_dir $RADSIM_BINDIR \
    --model_datafile $atmname \
    --model_ancil_datafile $sfcname \
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
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
else
$RADSIM_SCDIR/radsim_run.py \
    --config_file radsim_test.nml \
    --write_config_only F \
    --radsim_bin_dir $RADSIM_BINDIR \
    --model_datafile $atmname \
    --model_ancil_datafile $sfcname \
    --model_filetype $model_filetype \
    --rttov_coeffs_dir $RTTOV_DIR/rtcoef_rttov13/$COEFDIR \
    --platform $platform --satid $satid \
    --inst $inst --channels $channel \
    --rttov_coeffs_options $options \
    --output_file ${outfile} \
    --obs_datafile $obsoutfile \
    --addsolar T \
    --write_latlon T \
    --write_profile T \
    --nthreads $num_threads \
    2>&1 | tee ${outfile%.nc}.log \
    || exit 2
#    --ozone_data T \
#    --ir_addclouds T \
fi
fi
ls -ltr
## create plot
$RADSIM_SCDIR/radsim_plot_example.py \
    --filename $outfile \
    --vmin 190 --vmax 310 --cmap gray_r  ## band14
#    --vmin 190 --vmax 240 --cmap gray_r ## band8
#    --vmin 240 --vmax 320 --cmap gray_r ## band7
#    --lonmin 113.3 --lonmax 135.3 --latmin 23.5 --latmax 41.8
### debug
#python plot_skewT.py $sdate $outfile
#mv ${outfile} data/
#mv ${outfile%.nc}.log log/
#mv ${outfile}*.png fig/