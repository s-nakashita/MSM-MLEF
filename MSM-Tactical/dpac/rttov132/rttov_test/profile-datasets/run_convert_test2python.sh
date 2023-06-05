#!/bin/sh

# Call this script from within profile-datasets to convert
# all profile directories to Python scripts in preparation
# for conversion to HDF5 format for the GUI.

mkdir ../profile-datasets-py 2> /dev/null
rm -r ../profile-datasets-py/* 2> /dev/null

for i in *
do
    if [ -d $i ]; then
        echo "Converting $i..."
        python convert_test2python.py -d $i -D ../profile-datasets-py
    fi
done
