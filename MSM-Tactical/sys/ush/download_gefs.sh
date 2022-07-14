#!/bin/sh
set -x
#
DTOOL=${DTOOL:-curl}     ## DTOOL can be curl or wget
PGBRES=${PGBRES:-0.5}    ## can be 0.5 or 0.25
YMD=`echo $SDATE | cut -c1-8`
CYC=`echo $SDATE | cut -c9-10`
if [ $DTOOL = wget ] ; then OPN=O ; else OPN=o ; fi

DATADIR=${BASEDIR:-/home/hjuang/Model/DATA/gefs/$SDATE/$MEM}
mkdir -p $DATADIR
cd $DATADIR
LONMIN=${CLON1:--100}
LONMAX=${CLON2:--50}
LATMIN=${CLAT1:-10}
LATMAX=${CLAT2:-60}

mem=`expr $MEM + 0`
if [ $mem -eq 0 ]; then
  mem=c00
else
  if [ $mem -lt 10 ]; then
    mem=p0$mem
  else
    mem=p$mem
  fi
fi

fh=000
fh2=00
fend=`expr $fh + $ENDHOUR`
#
while [ $fh -le $fend ]; do
   fh2=$fh
   if [ $fh -lt 100 ]; then fh2=`echo $fh | cut -c2-3`; fi
   if [ ! -f pgbf$fh2 ]
   then
   if [ $fh -eq 0 ]; then
echo "Domain resolution "$PGBRES" degree " > $DATADIR/Domain_Info
echo "Domain westbound longitude "$LONMIN  >>$DATADIR/Domain_Info
echo "Domain eastbound longitude "$LONMAX  >>$DATADIR/Domain_Info
echo "Domain southbound latitude "$LATMIN  >>$DATADIR/Domain_Info
echo "Domain northbound latitude "$LATMAX  >>$DATADIR/Domain_Info
   fi
      if [ $PGBRES = 0.25 ]
      then
         PGBNAME=pgrb2s.0p25
         FILTER=filter_gefs_atmos_0p25s
         $DTOOL "https://nomads.ncep.noaa.gov/cgi-bin/${FILTER}.pl?file=ge${mem}.t${CYC}z.${PGBNAME}.f${fh}&all_lev=on&var_CLWMR=on&var_HGT=on&var_ICEC=on&var_LAND=on&var_O3MR=on&var_SPFH=on&var_SOILW=on&var_TSOIL=on&var_TCDC=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_WEASD=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2Fgefs.${YMD}%2F${CYC}%2Fatmos%2Fpgrb2sp25" -${OPN} pgbf$fh2
      else
         FILTER=filter_gefs_atmos_0p50a
         PGBNAME=pgrb2a.0p50
         $DTOOL "https://nomads.ncep.noaa.gov/cgi-bin/${FILTER}.pl?file=ge${mem}.t${CYC}z.${PGBNAME}.f${fh}&all_lev=on&var_HGT=on&var_SOILW=on&var_TSOIL=on&var_TCDC=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_WEASD=on&var_RH=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2Fgefs.${YMD}%2F${CYC}%2Fatmos%2Fpgrb2ap5" -${OPN} pgbf$fh2
         PGBNAME=pgrb2b.0p50
         FILTER=filter_gefs_atmos_0p50b
         $DTOOL "https://nomads.ncep.noaa.gov/cgi-bin/${FILTER}.pl?file=ge${mem}.t${CYC}z.${PGBNAME}.f${fh}&all_lev=on&var_CLWMR=on&var_HGT=on&var_O3MR=on&var_SPFH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_RH=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2Fgefs.${YMD}%2F${CYC}%2Fatmos%2Fpgrb2bp5" -${OPN} pgbbf$fh2
         cat pgbbf$fh2 >>pgbf$fh2 && rm pgbbf$fh2
      fi
   fi
   fh=`expr $fh + $INCBASE`
   if [ $fh -lt 100 ]; then fh=0$fh; fi
   if [ $fh -lt 10  ]; then fh=0$fh; fi
done
cd -
