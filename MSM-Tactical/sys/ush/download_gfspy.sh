#!/bin/sh
set -x
#
DTOOL=${DTOOL:-curl}     ## DTOOL can be curl or wget
PGBRES=${PGBRES:-0.5}    ## can be 0.5 or 0.25
YMD=`echo $SDATE | cut -c1-8`
CYC=`echo $SDATE | cut -c9-10`
if [ $DTOOL = wget ] ; then OPN=O ; else OPN=o ; fi

DATADIR=${BASEDIR:-/home/hjuang/Model/DATA/gfs/$SDATE}
mkdir -p $DATADIR
cd $DATADIR
LONMIN=${CLON1:--100}
LONMAX=${CLON2:--50}
LATMIN=${CLAT1:-10}
LATMAX=${CLAT2:-60}
LONMIN2=$LONMIN
LONMAX2=$LONMAX
if [ $LONMIN -gt 180 ]; then
  LONMIN2=`expr $LONMIN - 360`
fi
if [ $LONMAX -gt 180 ]; then
  LONMAX2=`expr $LONMAX - 360`
fi
fh=0
fh2=00
fend=`expr $fh + $ENDHOUR`
#
while [ $fh -le $fend ]; do
   fh2=$fh
   if [ $fh2 -lt 10 ]; then fh2=0$fh2; fi
   if [ ! -f pgbf$fh2 ]
   then
   if [ $fh -eq 0 ]; then
   echo "Domain resolution "$PGBRES" degree " > $DATADIR/Domain_Info
   echo "Domain westbound longitude "$LONMIN  >>$DATADIR/Domain_Info
   echo "Domain eastbound longitude "$LONMAX  >>$DATADIR/Domain_Info
   echo "Domain southbound latitude "$LATMIN  >>$DATADIR/Domain_Info
   echo "Domain northbound latitude "$LATMAX  >>$DATADIR/Domain_Info
   PRODUCT=Analysis
   else
   PRODUCT=${PRODUCT}"/${fh}-hour Forecast"
   fi
   fi
   fh=`expr $fh + $INCBASE`
done
echo $PRODUCT
if [ $fend -lt 100 ]; then fend=0$fend; fi
tarf=gfs.0p25.${SDATE}.f000-${fend}.grib2.tar
#if [ ! -f $tarf ]
#then
cat << EOF > download.py
import sys
sys.path.append('${DISK}/rda-apps-clients/')
import rdams_client as rc
import time
# Checks if q request is ready.
def check_ready(rqst_id, wait_interval=120):
    for i in range(100):
        res = rc.get_status(rqst_id)
        request_status = res['result']['status']
        if request_status == 'Completed':
            return True
        print(request_status)
        print('Not yet available. Waiting ' + str(wait_interval) + ' seconds.' )
        time.sleep(wait_interval)
    return False
# Define control dictionary for subsetting
control = {
'dataset' : 'ds084.1',
'date':'${SDATE}00/to/${SDATE}00',
'datetype':'init',
'param':'CLWMR/HGT/O3MR/SPF H/TMP/U GRD/V GRD/LAND/TSOIL/T CDC/SOILW/ICEC/WEASD',
#'level':,
#'oformat':'WMO GRIB2',
'nlat':${LATMAX},
'slat':${LATMIN},
'elon':${LONMAX2},
'wlon':${LONMIN2},
'product':'${PRODUCT}'
}
# Submit a request
response = rc.submit_json(control)
assert response['status'] == 'ok'
rqst_id = response['result']['request_id']
print(response)
# Checks if a request is ready. If ready, start to download the files.
check_ready(rqst_id)
response = rc.download(rqst_id)
# After download successfully, purge request
assert response['status'] == 'ok'
rc.purge_request(rqst_id)

EOF
cat download.py
cp ${DISK}/rda-apps-clients/rdamspw.txt .
${HOME}/.local/bin/python download.py || exit 99
ls
rm rdamspw.txt
#fi
#
fh=000
fh2=00
fend=`expr $fh + $ENDHOUR`
if [ -f $tarf ]; then
  tar -xvf $tarf
fi
while [ $fh -le $fend ]; do
   fh2=$fh
   if [ $fh -lt 100 ]; then fh2=`echo $fh | cut -c2-3`; fi
   if [ ! -f pgbf$fh2 ]
   then
   ln -s gfs.0p25.${SDATE}.f${fh}.grib2 pgbf$fh2
   fi
   fh=`expr $fh + $INCBASE`
   if [ $fh -lt 100 ]; then fh=0$fh; fi
   if [ $fh -lt 10  ]; then fh=0$fh; fi
done
cd -
