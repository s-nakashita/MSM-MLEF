#!/bin/sh

set -x

. ./configure

#if [ -s /ptmp ]; then
#  mkdir -p /ptmp/${USER}/rsmmap
#  cd /ptmp/${USER}/rsmmap && rm -f r*
#else
#  mkdir -p /tmp/${USER}/rsmmap
#  cd /tmp/${USER}/rsmmap && rm -f r*
#fi
rm -rf ${TEMP}/rsmmap && mkdir -p ${TEMP}/rsmmap
cd ${TEMP}/rsmmap && rm -f r*

echo " &NAMLOC			" >rmap.inp
echo " IGRD     = $IGRD,        ">>rmap.inp
echo " JGRD     = $JGRD,        ">>rmap.inp
echo " RPROJ    = $RPROJ,       ">>rmap.inp
echo " RTRUTH   = $RTRUTH,      ">>rmap.inp
echo " RORIENT  = $RORIENT,     ">>rmap.inp
echo " RDELX    = $RDELX,       ">>rmap.inp
echo " RDELY    = $RDELY,       ">>rmap.inp
echo " RCENLAT  = $RCENLAT,     ">>rmap.inp
echo " RCENLON  = $RCENLON,     ">>rmap.inp
echo " RLFTGRD  = $RLFTGRD,     ">>rmap.inp
echo " RBTMGRD  = $RBTMGRD,     ">>rmap.inp
echo " &END                     ">>rmap.inp

$UTLDIR/rsmmap.x <rmap.inp 

read im jm proj lon1 lon2 lat1 lat2 <rmap.parm
echo $im $jm $proj $lon1 $lon2 $lat1 $lat2

echo "'open rmap.ctl'"						 >rmap.gs
echo "'set display color white'"	 			>>rmap.gs
echo "'clear'" 							>>rmap.gs
echo "'set mpdset hires'" 					>>rmap.gs
if [ $proj = 1 ]
then
    echo "'set mproj nps'" 					>>rmap.gs
    echo "'"set mpvals $lon1 $lon2 $lat1 $lat2"'" 		>>rmap.gs
    echo "'set gxout shaded'" 					>>rmap.gs
    echo "'"set cint $im"'" 					>>rmap.gs
    echo "'d fi'" 						>>rmap.gs
fi
if [ $proj = -1 ]
then
    echo "'set mproj sps'" 					>>rmap.gs
    echo "'"set mpvals $lon1 $lon2 $lat1 $lat2"'" 		>>rmap.gs
    echo "'set gxout shaded'" 					>>rmap.gs
    echo "'"set cint $im"'" 					>>rmap.gs
    echo "'d fi'" 						>>rmap.gs
fi
echo "'define mask=20-sqrt(sqrt(fi*fi*fi*fi+fj*fj*fj*fj))'" 	>>rmap.gs
echo "'set grads off'" 						>>rmap.gs
echo "'set grid off'" 						>>rmap.gs
echo "'set gxout contour'" 					>>rmap.gs
echo "'set ccolor 3'" 						>>rmap.gs
echo "'set cint 1'" 						>>rmap.gs
echo "'d maskout(fi,mask)'" 					>>rmap.gs
echo "'set ccolor 3'" 						>>rmap.gs
echo "'set cint 1'" 						>>rmap.gs
echo "'d maskout(fj,mask)'" 					>>rmap.gs
echo "'draw title RSM MODEL DOMAIN WITH GRID LOCATION'" 	>>rmap.gs
echo "'gxprint domain.png x1280 y1024 white'"     		>>rmap.gs

grads -c "run rmap.gs"


