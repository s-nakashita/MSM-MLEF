#! /bin/sh
set -x 
#FTN=${FTN:-pgf90}
#FTN=${FTN:-gfortran}
#FORT_OPTNS=${FORT_OPTNS:-'-r8 -byteswapio'}
#FORT_OPTNS=${FORT_OPTNS:-'-fdefault-real-8 -fconvert=big-endian'}
export MACHINE=mac_gfortran
. ../opt/compile.option
FILEENV=${FILEENV:-'/bin/rm -rf fort.*'}
ASSIGN=${ASSIGN:-'ln -s'}
FILEFORM=${FILEFORM:-' '}
UNIT=${UNIT:-'fort.'}

  if [ ! -s fhour.x ]
  then
      cat >date.f <<'EOF'
      program datech
      CHARACTER*8 label(4)
      REAL*4 fhour 
      dimension idate(4)
      read(11) label
      read(11) fhour,idate
      ihour=nint(fhour)
      write(6,100) idate(1),idate(2),idate(3),idate(4),ihour
100   format(4i6,i10)
      stop
      end
EOF
      $F_COMP $F_FLAG_r8 -o fhour.x date.f
#      rm date.f
  fi

#  $FILEENV
#  $ASSIGN $1 $FILEFORM ${UNIT}11
#  ./DATE.x  >dte.out
#  read hour month day year fhour <dte.out
#  echo $fhour
#  rm dte.out
