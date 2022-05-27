#!/bin/sh
set -x
#
UTLDIR=${UTLDIR:-` pwd `/../utl}
ENDHOUR=${ENDHOUR:-72}
DTOOL=${DTOOL:-curl}
SDATE=${SDATE:-2011112900}
CYC=${CYC:-00}
LONMIN=${LONMIN:-100}
LONMAX=${LONMAX:-180}
DLON=${DLON:-1}
LONDIM=${LONDIM:-` expr $LONMAX - $LONMIN + 1 `}
LATMIN=${LATMIN:-0}
LATMAX=${LATMAX:-40}
DLAT=${DLAT:-1}
LATDIM=${LATDIM:-` expr $LATMAX - $LATMIN + 1 `}
DATADIR=${DATADIR:-~/TMPDIR/${SDATE}}
INCBASE=${INCBASE:-06}
SRVNAME=${SRVNAME:-nomads.ncep.noaa.gov}
SRVPATH=${SRVPATH:-cfs.${SDATE:0:8}/${CYC}/6hrly_grib_01}

mkdir -p $DATADIR
cd $DATADIR
FH=00
FEND=`expr $FH + $ENDHOUR`
#
while [ $FH -le $FEND ]; do
   if [ ! -f pgbf$FH ]
   then
      CDATE=` echo $SDATE | cut -c1-8 `
      FDATE=` ${UTLDIR}/validate.s $SDATE $FH `
      if [ $DTOOL = wget ]
      then
         wget "http://${SRVNAME}/cgi-bin/filter_cfs_pgb.pl?file=pgbf${FDATE}.01.${CDATE}${CYC}.grb2&all_lev=on&var_CLWMR=on&var_HGT=on&var_O3MR=on&var_SPFH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2F${SRVPATH}" -O pgbf$FH
         wget "http://${SRVNAME}/cgi-bin/filter_cfs_flx.pl?file=flxf${FDATE}.01.${CDATE}${CYC}.grb2&all_lev=on&var_ICEC=on&var_LAND=on&var_SOILW=on&var_TCDC=on&var_HGT=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_SPFH=on&var_WEASD=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2F${SRVPATH}" -o flxf$FH
         cat flxf$FH >> pgbf$FH
      else
         curl "http://${SRVNAME}/cgi-bin/filter_cfs_pgb.pl?file=pgbf${FDATE}.01.${CDATE}${CYC}.grb2&all_lev=on&var_CLWMR=on&var_HGT=on&var_O3MR=on&var_SPFH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2F${SRVPATH}" -o pgbf$FH
         curl "http://${SRVNAME}/cgi-bin/filter_cfs_flx.pl?file=flxf${FDATE}.01.${CDATE}${CYC}.grb2&all_lev=on&var_ICEC=on&var_LAND=on&var_SOILW=on&var_TCDC=on&var_HGT=on&var_TMP=on&var_UGRD=on&var_VGRD=on&var_SPFH=on&var_WEASD=on&subregion=&leftlon=${LONMIN}&rightlon=${LONMAX}&toplat=${LATMAX}&bottomlat=${LATMIN}&dir=%2F${SRVPATH}" -o flxf$FH
         cat flxf$FH >> pgbf$FH
      fi
   fi
   FH=`expr $FH + $INCBASE`
   if [ $FH -lt 10 ]; then FH=0$FH; fi
done
cd -
