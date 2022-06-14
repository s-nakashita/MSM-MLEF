#!/bin/sh
set -e
SDATE=2022061312
echo $SDATE
set -x
export SDATE
CDIR=/home/nakashita/Development/grmsm/MSM-Tactical/usr/exp
# set timer
#cd /home/nakashita/Development/grmsm/MSM-Tactical/usr/work
cd /zdata/grmsm/work
mkdir -p timer_nest/$SDATE
cd timer_nest/$SDATE
TIMERDIR=`pwd`
rm -f ${TIMERDIR}/timer
touch ${TIMERDIR}/timer
cd $CDIR
#GFS downscaling 
echo "downscaling" >> ${TIMERDIR}/timer
WDIR=${CDIR}/gfsp2rsm27
export WDIR
cd ${WDIR}
${WDIR}/clean 
${WDIR}/compile > compile.log 2>&1
start_time=$(date +"%s")
time ${WDIR}/run > run.log 2>&1 
end_time=$(date +"%s")
dt=$(echo "${end_time}-${start_time}" | bc)
echo "${dt} s" >> ${TIMERDIR}/timer
cd $CDIR
## RSM 27km
#echo "RSM 27km" >> ${TIMERDIR}/timer
#WDIR=${CDIR}/rsm2rsm27
#export WDIR
#cd ${WDIR}
#${WDIR}/clean 
#${WDIR}/compile > compile.log 2>&1
#start_time=$(date +"%s")
#time ${WDIR}/run > run.log 2>&1
#end_time=$(date +"%s")
#dt=$(echo "${end_time}-${start_time}" | bc)
#echo "${dt} s" >> ${TIMERDIR}/timer
#cd $CDIR
# MSM 9km
echo "MSM 9km" >> ${TIMERDIR}/timer
WDIR=${CDIR}/rsm2msm9
export WDIR
cd ${WDIR}
${WDIR}/clean 
${WDIR}/compile > compile.log 2>&1
start_time=$(date +"%s")
time ${WDIR}/run > run.log 2>&1
end_time=$(date +"%s")
dt=$(echo "${end_time}-${start_time}" | bc)
echo "${dt} s" >> ${TIMERDIR}/timer
# animation
ODIR=/zdata/grmsm/fig/rsm2msm9_jpn/${SDATE}
cd $ODIR
convert -delay 75 -loop 0 panel6_fh*.png panel6.gif
ls -ltr | tail -n 5
cd $CDIR
# MSM 3km
echo "MSM 3km" >> ${TIMERDIR}/timer
WDIR=${CDIR}/msm2msm3
export WDIR
cd ${WDIR}
${WDIR}/clean 
${WDIR}/compile > compile.log 2>&1
start_time=$(date +"%s")
time ${WDIR}/run > run.log 2>&1
end_time=$(date +"%s")
dt=$(echo "${end_time}-${start_time}" | bc)
echo "${dt} s" >> ${TIMERDIR}/timer
cd $CDIR
# total time
cat <<EOF > total.awk
BEGIN{sum=0}{
if(\$2~/s/){sum+=\$1}
}END{
hour=int(sum/3600);sum-=hour*3600
minu=int(sum/60);sum-=minu*60
sec=sum
print hour,"h",minu,"m",sec,"s"
}
EOF
awk -f total.awk ${TIMERDIR}/timer >> ${TIMERDIR}/timer
## panel plots
#${CDIR}/../nclscripts/plot_panel6.sh
# animation
ODIR=/zdata/grmsm/fig/msm2msm3_jpn/${SDATE}
cd $ODIR
convert -delay 75 -loop 0 panel6_fh*.png panel6.gif
ls -ltr | tail -n 5
cd $CDIR
# sync figure
${CDIR}/syncfig.sh
echo $?
