#!/bin/sh

# this script to generate rsmlocation with external defined variables

cat >rsmlocation <<EOF
 &NAMLOC
 RPROJ    = $RPROJ,
 RTRUTH   = $RTRUTH,
 RORIENT  = $RORIENT,
 RDELX    = $RDELX,
 RDELY    = $RDELY,
 RCENLAT  = $RCENLAT,
 RCENLON  = $RCENLON,
 RLFTGRD  = $RLFTGRD,
 RBTMGRD  = $RBTMGRD,
 CPROJ    = $CPROJ,
 CTRUTH   = $CTRUTH,
 CORIENT  = $CORIENT,
 CDELX    = $CDELX,
 CDELY    = $CDELY,
 CCENLAT  = $CCENLAT,
 CCENLON  = $CCENLON,
 CLFTGRD  = $CLFTGRD,
 CBTMGRD  = $CBTMGRD,
 CLAT2    = $CLAT2,
 CLAT1    = $CLAT1,
 CLON2    = $CLON2,
 CLON1    = $CLON1,
 &END
EOF

