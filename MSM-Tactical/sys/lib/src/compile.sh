#!/bin/sh

# compile options for ibm_xlf, linux_intel, linux_pgi, linux_gfortran, mac_intel
#                     mac_gfortran
#export MACHINE=linux_intel
export MACHINE=${MACHINE:-linux_gfortran}
export LIBPATH=`cd .. ; pwd `
export INCDIR=$LIBPATH/incmod
. ./compile.option

if [ $MACHINE = ibm_xlf ]; then
  mach_name=`uname -a | cut -c5`
  if [ $mach_name = c ]; then
    cflag=0
  else
    cflag=1
  fi
else
  cflag=1
fi

if [ $cflag = 1 ]; then
  cd $LIBPATH/src/zlib-1.2.3
  ./configure --prefix=$LIBPATH 
  make
  make install

  cp -r $LIBPATH/include/* $LIBPATH/incmod
  cp -r $LIBPATH/lib/* $LIBPATH/

  export LDFLAGS="-L$LIBPATH -lz"
  export CPPFLAGS=-I$LIBPATH/include
  cd $LIBPATH/src/libpng-1.2.40
  make clean
  ./configure --prefix=$LIBPATH --disable-shared
# ./configure --prefix=$LIBPATH --enable-shared --enable-static
  make
  make install
  unset LDFLAGS
  unset CPPFLAGS

  cp -r $LIBPATH/include/* $LIBPATH/incmod
  cp -r $LIBPATH/lib/* $LIBPATH/

  export LDFLAGS="-L$LIBPATH -lpng -lz"
  export CPPFLAGS="-I$LIBPATH/include -I/usrx/local/jpeg.6b/include"
  if [ $MACHINE = ibm_xlf ]; then
    export CPPFLAGS="-I/usrx/local/jpeg.6b/include"
  fi
  cd $LIBPATH/src/jasper-1.900.1
  make clean
  ./configure --prefix=$LIBPATH --disable-shared
# ./configure --prefix=$LIBPATH --enable-shared --enable_static
# ./configure --prefix=$LIBPATH --enable-shared 
  make
  make install
  unset LDFLAGS
  unset CPPFLAGS

  cp -r $LIBPATH/include/* $LIBPATH/incmod
  cp -r $LIBPATH/lib/* $LIBPATH/
  rm -rf $LIBPATH/bin
  rm -rf $LIBPATH/lib
  rm -rf $LIBPATH/man
  rm -rf $LIBPATH/share
  rm -rf $LIBPATH/pkgconfig
  rm -rf $LIBPATH/include
fi

cd $LIBPATH/src
. ./compile.option

cd bacio
./makebacio.sh

cd ..

mkdir -p $LIBPATH/incmod/w3_4
mkdir -p $LIBPATH/incmod/w3_8
mkdir -p $LIBPATH/incmod/w3_d
cd w3lib-1.7
./makelibw3.sh
cd ..

if [ $MACHINE = ibm_xlf ]; then
  export CFLAGS_R=" -q64 -O3 "
fi

if [ $cflag = 1 ]; then
  mkdir -p $LIBPATH/incmod/g2_4
  cd g2
  gmake
  gmake install
  cd ..
fi

# fix problem for prefix error in configure and install
cd $LIBPATH && ln -sf $LIBPATH lib 
