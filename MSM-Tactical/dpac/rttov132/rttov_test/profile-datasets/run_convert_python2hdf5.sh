#!/bin/sh

# Call this script from within profile-datasets after running
# run_convert_test2python.sh to convert the Python profile
# definitions to HDF files for use with the GUI.

# Environment variable PYTHONPATH must contain the path to the
# RTTOV gui/rttov directory
# Also need a link to rttov_gui_f2py.so in current directory

export PYTHONPATH=$PYTHONPATH:../../gui/rttov
ln -s ../../gui/rttov_gui_f2py.so 2> /dev/null

mkdir ../profile-datasets-hdf 2> /dev/null
rm -r ../profile-datasets-hdf/* 2> /dev/null

cd ../profile-datasets-py

for i in *//*.py
do
  echo "Converting $i..."
  j=`basename $i .py`
  o=../profile-datasets-hdf/`dirname $i`'.H5'
  python ../profile-datasets/convert_python2hdf5.py -i $i -o $o -g "/PROFILES/0$j"
done

cd ../profile-datasets
