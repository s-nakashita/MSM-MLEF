#!/bin/sh
#
#WGRIB="/climate/save/wx20wa/wgrib/old/wgrib"
WGRIB=${WGRIB:-"/usrx/local/grads/bin/wgrib"}
filelist="r_pgb.f?? r_pgb.f??? r_pgb.f????"
dir=$1
endhour=$2
subset=$3

cd $dir
#for fl in $filelist
fh=00
while [ $fh -le $endhour ]
do
  fl=r_pgb.f$fh
  echo "file=$fl"

  if [ $fh -eq 0 ]; then
  echo "$WGRIB -4yr -s $fl |egrep \":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN):\" |$WGRIB $fl -i -grib -s -o $subset"
  $WGRIB -4yr -s $fl |egrep ":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN|SOILW):"  |$WGRIB $fl -i -grib -s -o $subset
  else
  echo "$WGRIB -4yr -s $fl |egrep \":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN):\" |$WGRIB $fl -i -append -s -o $subset"
  $WGRIB -4yr -s $fl |egrep ":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN|SOILW):"  |$WGRIB $fl -i -grib -append -s -o $subset
  fi

   echo "get PRES|TMP|DLWRF|ULWRF|DSWRF|USWRF"
  $WGRIB -4yr -s $fl |egrep ":(PRES|TMP|DLWRF|ULWRF|DSWRF|USWRF):" |egrep ":(sfc):" |$WGRIB $fl -i -grib -append -s -o $subset

   echo "get UGRD|VGRD"
  $WGRIB -4yr -s $fl |egrep ":(UGRD|VGRD):" |egrep ":(10 m above gnd):" |$WGRIB $fl -i -grib -append -s -o $subset

   echo "get TMP"
  $WGRIB -4yr -s $fl |egrep ":(TMP):" |egrep ":(2 m above gnd):" |$WGRIB $fl -i -grib -append -s -o $subset

   echo "get TCDC"
  $WGRIB -4yr -s $fl |egrep ":(TCDC):" |egrep ":(atmos col):" |$WGRIB $fl -i -grib -append -s -o $subset

# for ieee file
# $WGRIB -4yr -s $subset |egrep ":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN|SOILW|PRES|TMP|DLWRF|ULWRF|DSWRF|USWRF|UGRD|VGRD|TCDC):" |$WGRIB $fl -i -nh -ieee  -s -o subset.ieee

# $WGRIB -4yr -s $subset |egrep ":(APCP|SPFH|LHTFL|SHTFL|UFLX|VFLX|TMAX|TMIN|QMAX|QMIN|SOILW|PRES|TMP|DLWRF|ULWRF|DSWRF|USWRF|UGRD|VGRD|TCDC):" |$WGRIB $fl -i -nh -text  -s -o subset.text

  fh=`expr $fh + 6`
  if [ $fh -lt 10 ]; then fh=0$fh; fi
  
done

