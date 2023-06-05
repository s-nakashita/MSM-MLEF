#!/bin/bash

echo
echo '==============================================================================='
echo ' RTTOV compilation script'
echo '==============================================================================='
echo
echo "This script compiles RTTOV. It should be run from the src/ directory."
echo
echo "Much RTTOV functionality is available without HDF5: only those features listed"
echo "below require HDF5 to compile."
echo
echo "If compiling with HDF5 (recommended) you must first edit the file"
echo "build/Makefile.local with the location of the library."
echo
echo "RTTOV features which require the HDF5 library:"
echo "- Reading HDF5 coefficient files"
echo "- Emissivity/BRDF atlases"
echo "- RTTOV GUI"
echo
echo "In addition f2py must be installed for the Python interface and the RTTOV GUI."
echo
echo "The NetCDF v4 library is optional for running the HTFRTC PC-based model. You"
echo "can specify this library in build/Makefile.local before running this script."
echo
echo "RTTOV uses some LAPACK routines which are included in the package, but you"
echo "can optionally link against your own LAPACK library instead by specifying"
echo "this library in build/Makefile.local before running this script."
echo '==============================================================================='
echo

# Function for interpreting input

onezero () {
    if [ "$1" = "y" ]; then
        echo 1
    else
        echo 0
    fi
}


# Function for detecting if a given library is specified in Makefile.local

check_makefile_local () {
    fflags=$(grep -e "^\s*FFLAGS_$1\s*=" ../build/Makefile.local | cut -d"=" -f2)
    ldflags=$(grep -e "^\s*LDFLAGS_$1\s*=" ../build/Makefile.local | cut -d"=" -f2)
    if [[ $fflags = "" || $ldflags = "" ]]; then
        return 0
    else
        return 1
    fi
}


# Check we are in src/ directory; if not then try to find it

if [[ $(pwd) != *"src" || ! -d main/ ]]; then
    if [[ -d src/main/ ]]; then
        echo "Not in RTTOV src/ directory, changing to src/"
        cd src
    elif [[ -d ../src/main/ ]]; then
        echo "Not in RTTOV src/ directory, changing to src/"
        cd ../src
    elif [[ -d ../../src/main/ ]]; then
        echo "Not in RTTOV src/ directory, changing to src/"
        cd ../../src
    else
        echo "This script should be run from the RTTOV src/ directory."
        exit 1
    fi
    echo
fi


# Specify build flags and installation directory

echo "Compiler flag files available in build/arch/:"
echo
ls ../build/arch/
echo
echo "Specify required compiler flag file (leave blank for default: gfortran-openmp)"
read -p "> " myarch
if [[ $myarch = "" ]]; then
    myarch="gfortran-openmp"
elif [[ ! -f ../build/arch/$myarch ]]; then
    echo "Error: $myarch not found in build/arch/ directory."
    echo "Either specify an existing file or add your own using an existing file as a template."
    exit 1
fi

echo "Specify installation directory relative to top-level RTTOV directory (leave blank for default: ./)"
read -p "> " installdir
if [[ $installdir = "" ]]; then
    installdir="./"
fi


# Check HDF5, NetCDF and LAPACK libraries and f2py, determine options for regenerating Makefiles

echo ""
echo "Checking ../build/Makefile.local for user-specified HDF5 library..."
check_makefile_local "HDF5"
if [[ $? -eq 0 ]]; then
    hdf5="n"
    echo "...did not find FFLAGS_HDF5 or LDFLAGS_HDF5: compiling without HDF5 library"
    echo "If you want to compile with HDF5 you must specify this in ../build/Makefile.local first"
else
    hdf5="y"
    echo "...found FFLAGS_HDF5 and LDFLAGS_HDF5: compiling with HDF5 library"
fi

echo ""
echo "Checking ../build/Makefile.local for user-specified NetCDF library..."
check_makefile_local "NETCDF"
if [[ $? -eq 0 ]]; then
    netcdf="n"
    echo "...did not find FFLAGS_NETCDF or LDFLAGS_NETCDF: compiling without NETCDF library"
    echo "If you want to compile with NETCDF you must specify this in ../build/Makefile.local first"
else
    netcdf="y"
    echo "...found FFLAGS_NETCDF and LDFLAGS_NETCDF: compiling with NETCDF library"
fi

echo ""
echo "Checking ../build/Makefile.local for user-specified LAPACK library..."
check_makefile_local "LAPACK"
if [[ $? -eq 0 ]]; then
    lapack="n"
    echo "...did not find FFLAGS_LAPACK or LDFLAGS_LAPACK: compiling with lapack.f included in RTTOV"
else
    lapack="y"
    echo "...found FFLAGS_LAPACK and LDFLAGS_LAPACK: compiling with user-specified LAPACK library"
fi

f2py="n"
gui="n"
echo ""
f2pycmd=$(grep -e "^\s*F2PY\s*=" ../build/arch/$myarch | sed -e "s/^\s*F2PY\s*=//")
if [[ ! $f2pycmd = "" ]]; then
    echo "Testing for f2py using '$f2pycmd'..."

    TMPF90=__rttov_foo__.F90
    TMPPYLIB=__rttov_foo_f2py__
    echo "SUBROUTINE foo; PRINT *,'Hello world'; END SUBROUTINE" > $TMPF90
    $f2pycmd -c -m $TMPPYLIB $TMPF90 1> /dev/null 2> /dev/null
    result=$?
    rm -f ${TMPPYLIB}.so $TMPF90

    if [[ $result -eq 0 ]]; then
        if [[ $hdf5 = "y" ]]; then
            echo "...f2py detected: do you want to compile the Python wrapper and RTTOV GUI? (y/n)"
        else
            echo "...f2py detected: do you want to compile the Python wrapper? (y/n)"
        fi
        while true; do
            read -p "> " yn
            case $yn in
                [Yy] ) f2py="y"; break;;
                [Nn] ) f2py="n"; break;;
                * ) echo "Please answer y or n.";;
            esac
        done
        if [[ $hdf5 = "y" ]]; then
          gui=$f2py
        fi
    else
        echo "...f2py does not appear to be installed, Python interface and RTTOV GUI cannot be compiled"
    fi
else
  echo "Did not find F2PY in ../build/arch/$myarch: compiling without f2py"
fi

# Check for a previous build

clean="n"
if [[ -d ../$installdir/bin ]]; then
    echo
    echo "Previous build detected in $installdir: perform a clean compilation? (y/n)"
    echo "Choose y if you have changed the compilation options since the previous build and re-compilation fails."
    while true; do
        read -p "> " yn
        case $yn in
            [Yy] ) clean="y"; break;;
            [Nn] ) clean="n"; break;;
            * ) echo "Please answer y or n.";;
        esac
    done
    echo
fi


# Additional flags for make

makeflags=
echo
echo "Specify any additional flags to pass to make (e.g. -j); leave blank if unsure"
read -p "> " makeflags
echo


# Summarise the user inputs

echo
echo "==========================="
echo " RTTOV COMPILATION SUMMARY "
echo "==========================="
echo
echo "Compiling with flags           : $myarch"
echo "Compiling in directory         : $installdir"
echo
echo "RTTOV features available:"
echo "HDF5 coefficient I/O           : $hdf5"
echo "Emissivity/BRDF atlases        : $hdf5"
echo "C/C++ wrapper                  : y"
echo "Python wrapper                 : $f2py"
echo "RTTOV GUI                      : $gui"
echo "HTFRTC netCDF files            : $netcdf"
echo
echo "Compiling with user LAPACK lib : $lapack"
echo

cmd_mkfile="../build/Makefile.PL RTTOV_HDF=$(onezero $hdf5) RTTOV_F2PY=$(onezero $f2py) RTTOV_USER_LAPACK=$(onezero $lapack)"
cmd_clean="make ARCH=$myarch INSTALLDIR=$installdir clean $makeflags"
cmd_build="make ARCH=$myarch INSTALLDIR=$installdir $makeflags"

echo "Regenerating Makefiles using:"
echo "$ $cmd_mkfile"
echo
echo "Compiling RTTOV using:"
if [[ $clean = "y" ]]; then
    echo "$ $cmd_clean"
fi
echo "$ $cmd_build"

echo
echo "OK to continue and compile RTTOV? (y/n)"
while true; do
    read -p "> " yn
    case $yn in
        [Yy] ) break;;
        [Nn] ) exit;;
        * ) echo "Please answer y or n.";;
    esac
done


# Compile RTTOV

echo
echo "Regenerating Makefiles..."
$cmd_mkfile

echo
echo "Compiling RTTOV..."
if [[ $clean = "y" ]]; then
    $cmd_clean
fi
$cmd_build
if [[ $? -eq 0 ]]; then
    echo
    echo "RTTOV compiled successfully"
    echo
fi
