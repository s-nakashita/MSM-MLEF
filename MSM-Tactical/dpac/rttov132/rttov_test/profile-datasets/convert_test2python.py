#!/usr/bin/env python
#
# This script reads one rttov test profile (ASCII files for rttov_test.pl)
# and writes a Python file which contains the transcription of the profile
# variables for the use in the RTTOV GUI frame work
#
# ============================================================================

import numpy as np
import os
import argparse
import pprint

# ----------------------------------------------------------------------------
# Function to create the file for a new profile 
# ----------------------------------------------------------------------------

def make_fo(fo, dr):
    """Create the output file, will later be opened in append mode"""
    if not os.path.exists(os.path.dirname(fo)):
        os.makedirs(os.path.dirname(fo))
    with open(fo, 'w') as f:
        f.write('"""\n    Profile {}\n'.format(fo))
        f.write('        file automaticaly created by prof_gen.py script\n')
        f.write('""" \n\n')
        d = dr + '/prof_id.txt'
        if os.path.exists(d):
            with open(d, 'r') as fin:
                f.write('self["ID"] = "{}"'.format(fin.readline().strip()))
        else:
            f.write('self["ID"] = "{}"'.format(fo))

# ----------------------------------------------------------------------------
# The following functions read in and write out different kinds of profile data
# ----------------------------------------------------------------------------
def get_array(fn, dtype=float):
    """Get an array from a text file"""
    if os.access(fn, os.R_OK):
        data=np.loadtxt(fn, dtype=dtype)
    else:
        data=None
    return data

def write_list(fn, data, gui=False, name=""):
    """Write a 1-D profile to the given filename"""
    if gui:
        with open(fn, 'a') as f:
            f.write('\nself["{}"] = numpy.'.format(name))
            pprint.pprint(data, stream=f)
    else:
        with open(fn, 'w') as f:
            for v in data:
                f.write('{:12.6E}'.format(v) + '\n')

def do_gases(dr, fo):
    """Gases and Pressure, also liquid water (MW)"""
    LIST = {'P':'p.txt','T':'t.txt', 'Q':'q.txt', 'O3':'o3.txt', 'CO2':'co2.txt', 'CO':'co.txt', 'N2O':'n2o.txt', 'CH4':'ch4.txt', 'SO2':'so2.txt', 'CLW':'clw.txt'}
    d = dr + '/atm/'
    for key in LIST.keys():
        data=get_array(d + LIST[key])
        if data is not None:
            write_list(fo, data, True, key)    

def do_aerosol(dr, fo):  
    """Read aerosl.txt file and write output"""
    d = dr + '/atm/'
    aer=get_array(d + 'aerosl.txt')
    if aer is not None:
        if (aer.shape[1]) == 13:
            aerosol_list = ['INSO','WASO','SOOT','SSAM','SSCM','MINM','MIAM','MICM','MITR',  \
                            'SUSO','VOLA','VAPO','ASDU']
        else:
            aerosol_list = ['BCAR','DUS1','DUS2','DUS3','SULP','SSA1','SSA2','SSA3','OMAT']
        for ityp in range(aer.shape[1]):
            if( any(aer[:,ityp] > 0) ):
                write_list(fo, aer[:,ityp], True, aerosol_list[ityp] )

def do_cloud(dr, fo):
    """Read cloud.txt file and write output"""
    cloud_list = ['STCO', 'STMA', 'CUCC', 'CUCP', 'CUMA', 'CIRR']
    d = dr + '/atm/'
    cld=get_array(d + 'cloud.txt')
    if cld is not None:
        for ityp in range(cld.shape[1]):
            if( any(cld[:,ityp] > 0) ):
                write_list(fo, cld[:,ityp], True, cloud_list[ityp] )
    cfrac=get_array(d + 'cfrac.txt')
    if cfrac is not None:
        write_list(fo, cfrac, True, "CFRAC")  
    icede=get_array(d + 'icede.txt')
    if icede is not None:
        write_list(fo, icede, True, "ICEDE")    
    clwde=get_array(d + 'clwde.txt')
    if clwde is not None:
        write_list(fo, clwde, True, "CLWDE")

def do_common(fi, fo, strep="", kname=None):
    """Manage a file with Fortan namelists"""
    fastem=np.zeros(5)
    with open(fo, 'a') as f:
        with open(fi, 'r') as v:
            for line in v:
                if "=" in line:
                    l=line.replace(strep,"").upper()
                    indx=l.find("=")
                    name=l[0:indx].strip()
                    val=l[indx+1:].strip()
                    if "FASTEM" in name:
                        ifor=int(name.replace("FASTEM(","").replace(")",""))
                        fastem[ifor-1] = val
                        if ifor == 5:
                            f.write('\nself["{}"]["{}"] = numpy.'.format(kname,"FASTEM"))
                            pprint.pprint(fastem, stream=f)
                    elif "MMR_CLDAER" in name:
                        val = (1 if val.upper() == 'T' else 0)
                        f.write('\nself["{}"] = {}'.format(name, val))
                    else:
                        if kname is not None:
                            f.write('\nself["{}"]["{}"] = {}'.format(kname,name, val))
                        else:
                            f.write('\nself["{}"] = {}'.format(name, val))

def do_commons(dr, fo):
    """Consider all common files"""
    d = dr + '/atm/simple_cloud.txt'
    if os.path.exists(d):
        do_common(d, fo)

    d = dr + '/atm/mmr_cldaer.txt'
    if os.path.exists(d):
        do_common(d, fo)

    d = dr + '/atm/ice_scheme.txt'
    if os.path.exists(d):
        do_common(d, fo)

    d = dr + '/atm/clw_scheme.txt'
    if os.path.exists(d):
        do_common(d, fo)

    d = dr + '/ground/s2m.txt'
    do_common(d, fo, "s0%", "S2M")

    d = dr + '/ground/skin.txt'
    do_common(d, fo, "k0%", "SKIN")

    d = dr + '/angles.txt'
    do_common(d, fo)

    d = dr + '/gas_units.txt'
    if os.path.exists(d):
        do_common(d, fo)

    d = dr + '/be.txt'
    if os.path.exists(d):
        data=get_array(d)
        with open(fo, 'a') as f:
            f.write('\nself["BE"] = {}'.format(data[0]))
            f.write('\nself["COSBK"] = {}\n'.format(data[1]))

    d = dr + '/datetime.txt'
    if os.path.exists(d):
        data=get_array(d, dtype=int)
        if data is None:
            pass
        else:
            write_list(fo, data[0:3], True, "DATE")
            write_list(fo, data[3:6], True, "TIME")


def parse_args():
    parser = argparse.ArgumentParser(description="Import ASCII profile to Python ASCII for RTTOV GUI", conflict_handler='resolve',formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-d', '--dir-in',  dest="inputd",                      help="input directory",  type=str, required=True)
    parser.add_argument('-D', '--dir-out', dest='outputd', default=".",        help='output directory', type=str)
    parser.add_argument("-v", "--verbose", dest="verbose", action="store_true",help="list output files")
    return parser.parse_args()

args = parse_args()

if os.path.isdir(args.inputd):
    for i in range(1,999):
        fn=str('{:0>3}'.format(i))
        d=args.inputd+"/"+fn
        fo=args.outputd+"/"+args.inputd+"/"+fn+".py"
        if args.verbose :
            print(fo)
        if os.path.exists(d):
            make_fo(fo, d)
            do_gases(d, fo)
            do_cloud(d, fo)
            do_aerosol(d, fo)
            do_commons(d, fo)
        else:
            break
else:
    print(args.inputd,"is not a valid directory")

