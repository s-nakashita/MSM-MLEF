#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
   Import an ASCII file which contains Python definitions of a RTTOV
     profile structure and convert it to HDF5 format for use with the RTTOV GUI.
   Mandatory input is the input file name
   If not provided, output file name is the input filename with new .H5 extension
   Default HDF5 group is  /PROFILES/0001/ which is appropriate for RTTOV GUI

   May 2014
"""
import rttov.profile as profile
import h5py
import argparse
import os

def parse_args():
    parser = argparse.ArgumentParser(description="Import ASCII profile to HDF5 for RTTOV GUI", conflict_handler='resolve',formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-i', '--input-file',  dest="inputf",  help="input file name",    type=str, required=True)
    parser.add_argument('-o', '--output-file', dest="outputf", help="output file name",   type=str)
    parser.add_argument('-g', '--group',       dest="group",   default="/PROFILES/0001",  help="internal HDF5 path", type=str)
    parser.add_argument("-v", "--verbose",     dest="verbose", action="store_true",       help="display profile variables")
    return parser.parse_args()

args = parse_args()

p = profile.Profile()

erreur = p.loadProfileAscii(args.inputf)

if( erreur != 1 ):
    if( args.verbose ):
        p.display()

    if( args.outputf is None ):
        iName, iExtension = os.path.splitext(args.inputf)
        args.outputf = iName + ".H5"

    of_option = 'w'
    if "PROFILES/" in args.group:
        ip = os.path.basename(os.path.normpath(args.group))
        if ip.isdigit():
            if int(ip) > 1:
                of_option = 'a'
    of = h5py.File(args.outputf, of_option)
    p.saveh5(of, args.group)
    of.close()
