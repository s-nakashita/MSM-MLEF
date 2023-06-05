#!/usr/bin/env python

# This script generates profile sets for the RTTOV test suite based on
# the diverse profile datasets.

# A function is required to read the profile data from the particular
# dataset and then common routines are used to write the profiles out
# possibly with modifications to certain profile parameters.

# Various additional functions are provided to interpolate profile
# sets to different levels and change specific parameters.

# The gas profiles are assumed to be ppmv over dry air (83) or
# kg/kg over moist air (52) and are converted to ppmv over moist air
# for the test suite.

import os
import argparse
import numpy as np

from prof_gen import make_dirs, write_list, \
                     ppmvdry2kgkgwet, ppmvdry2ppmvwet, kgkgdry2ppmvdry, \
                     DRYAIRMASS, GASMASSLIST, \
                     UNIT_PPMVDRY, UNIT_KGKGWET, UNIT_PPMVWET
from prof_gen_data import p54, p44


SURFTYPE_LAND = 0
SURFTYPE_SEA = 1

PROF_LIST = ['t', 'q', 'o3', 'co2', 'co', 'n2o', 'ch4', 'so2']

# -----------------------------------------------------------------------------
# Subroutines for reading the diverse dataset ASCII files
# -----------------------------------------------------------------------------

def skip_comment(f):
    line = f.readline()
    while line[0] == '!':
        line = f.readline()
    return line

def read_div52(fname_prof, fname_surf, fname_co2='', nprof=52, nlev=101, gas_unit=UNIT_PPMVWET):
    """Reads the diverse 52 profile set as provided on the NWP SAF web site.
       The profile and surface data are held in separate files.
       Units for gases are kg/kg dry so they must be converted to the chosen units.
       The CO2 data is in a separate file: it was not part of the original
       diverse 52 profile set (CO2 units assumed to be ppmv dry)."""

    if not os.path.exists(fname_prof):
        print('Profile file not found')
        return
    if not os.path.exists(fname_surf):
        print('Surface file not found')
        return

    profs = {}
    profs['nprof'] = nprof
    profs['nlev'] = nlev
    profsq = {} # Water vapour profiles in ppmv dry for CO2 unit conversion

    # Read profile data (gases in kg/kg wet)

    with open(fname_prof, 'r') as f:
        for p in range(nprof):
            if p == 0:
                line = skip_comment(f)
            else:
                line = f.readline()

            profs[p] = {}

            data = line.split()

            if int(data[1]) < 0:
                # Take care of the min/max/mean profiles
                profs[p]['datetime'] = [1950, 1, 1, 0, 0, 0]
                profs[p]['lat'] = 0.
                profs[p]['lon'] = 0.
                profs[p]['surftype'] = 1
            else:
                profs[p]['datetime'] = [int(data[1][0:4]), int(data[1][4:6]), int(data[1][6:8]), int(data[1][8:10]), 0, 0]
                profs[p]['lat'] = float(data[2])
                profs[p]['lon'] = float(data[3])
                profs[p]['surftype'] = SURFTYPE_LAND if float(data[5]) > 0. else SURFTYPE_SEA

            profs[p]['p2m'] = float(data[4])

            if p == 0:
                f.readline()

            profs[p]['gas_units'] = gas_unit

            keys = ['p', 't', 'q', 'o3'] # q must be first gas as it is used in unit conversion
            for k in keys:
                profs[p][k] = []
            profsq[p] = []
            for l in range(nlev):
                data = list(map(float, f.readline().split()))
                for i, k in enumerate(keys):
                    if k == 'q':
                        profsq[p].append(kgkgdry2ppmvdry(data[i], 'q')) # Store q in ppmv dry for CO2 unit conversion
                    # Conversion from kg/kg dry to selected units
                    if (k not in GASMASSLIST):
                        profs[p][k].append(data[i])
                    elif gas_unit == UNIT_KGKGWET:
                        profs[p][k].append(ppmvdry2kgkgwet(kgkgdry2ppmvdry(data[i], k), k, profsq[p][-1]))
                    elif gas_unit == UNIT_PPMVWET:
                        profs[p][k].append(ppmvdry2ppmvwet(kgkgdry2ppmvdry(data[i], k), k, profsq[p][-1]))
                    else: # gas_unit == UNIT_PPMVDRY:
                        profs[p][k].append(kgkgdry2ppmvdry(data[i], k))


    # Read surface variable data

    with open(fname_surf, 'r') as f:
        f.readline()

        keys = ['t2m', 'tskin', 'q2m', 'o32m', 'windu', 'windv']
        for p in range(nprof):
            data = list(map(float, f.readline().split()))
            for i, k in enumerate(keys):
                if k[:1] == 'q': qppmvdry = kgkgdry2ppmvdry(data[i], 'q')
                # Conversion from kg/kg dry to selected units
                if (k[:-2] not in GASMASSLIST):
                    profs[p][k] = data[i]
                elif gas_unit == UNIT_KGKGWET:
                    profs[p][k] = ppmvdry2kgkgwet(kgkgdry2ppmvdry(data[i], k[:-2]), k[:-2], qppmvdry)
                elif gas_unit == UNIT_PPMVWET:
                    profs[p][k] = ppmvdry2ppmvwet(kgkgdry2ppmvdry(data[i], k[:-2]), k[:-2], qppmvdry)
                else: # gas_unit == UNIT_PPMVDRY:
                    profs[p][k] = kgkgdry2ppmvdry(data[i], k[:-2])


    # Read CO2 data if supplied (ppmv dry)

    try:
        with open(fname_co2, 'r') as f:
            for p in range(nprof):
                if p == 0:
                    line = skip_comment(f)
                else:
                    line = f.readline()
                profs[p]['co2'] = []
                for l in range(nlev):
                    line = f.readline()
                    # Conversion from ppmv dry to selected units
                    if gas_unit == UNIT_PPMVWET:
                        profs[p]['co2'].append(ppmvdry2ppmvwet(float(line), 'co2', profsq[p][l]))
                    elif gas_unit == UNIT_KGKGWET:
                        profs[p]['co2'].append(ppmvdry2kgkgwet(float(line), 'co2'))
                    else: #  gas_unit == UNIT_PPMVDRY
                        profs[p]['co2'].append(float(line))
    except:
        print('No CO2 file found for div52, continuing without CO2 profiles')

    # Fill in remaining profile values with defaults (these may be overwritten later)

    for p in range(nprof):
        profs[p]['zenangle'] = (p+1) * 1.5 % 60
        profs[p]['azangle'] = 0.
        profs[p]['sunzenangle'] = (p+1) * 2.5 % 80
        profs[p]['sunazangle'] = (p+1) * 7 % 360
        profs[p]['elevation'] = 0.
        profs[p]['be'] = 0.
        profs[p]['cosbk'] = 0.
        profs[p]['ctp'] = 500.
        profs[p]['cfraction'] = 0.
        profs[p]['clwde_param'] = 0
        profs[p]['icede_param'] = 0
        profs[p]['clw_scheme'] = 0
        profs[p]['ice_scheme'] = 0
        profs[p]['watertype'] = 1
        profs[p]['salinity'] = 35.
        profs[p]['foam_fraction'] = 0.
        profs[p]['fastem'] = [3., 5., 15., 0.1, 0.3]
        profs[p]['windfetch'] = 100000.

    return profs


def read_div83(fname, fname_co2n2och4_2016='', fname_so2='', nprof=83, nlev=101, gas_unit=UNIT_PPMVWET):
    """Reads the diverse 83 profile set as provided on the NWP SAF web site.
       Data for skin T and 2m T and q are not present, so these are taken
       from the bottom level of the respective atmospheric profiles.
       Units for gases are ppmv dry."""

    if not os.path.exists(fname):
        print('Profile file not found')
        return

    profs = {}
    profs['nprof'] = nprof
    profs['nlev'] = nlev
    qdry = np.zeros((nprof,nlev), dtype=np.float64)

    with open(fname, 'r') as f:
        line = skip_comment(f)
        for p in range(nprof):
            if p > 0:
                f.readline()
            line = f.readline()

            profs[p] = {}

            data = line.split()

            if int(data[1]) == 1000:
                # Take care of the min/max/mean profiles
                profs[p]['datetime'] = [1950, 1, 1, 0, 0, 0]
                profs[p]['lat'] = 0.
                profs[p]['lon'] = 0.
                profs[p]['surftype'] = 0
            else:
                profs[p]['datetime'] = [int(data[0]), int(data[1]), int(data[2]), 0, 0, 0]
                profs[p]['lat'] = float(data[3])
                profs[p]['lon'] = float(data[4])
                profs[p]['surftype'] = SURFTYPE_LAND if float(data[6]) >= 0.5 else SURFTYPE_SEA

            profs[p]['p2m'] = float(data[5]) # This is 1000 for min/max/mean, but that's reasonable

            profs[p]['gas_units'] = gas_unit

            keys = ['p', 't', 'q', 'co2', 'o3', 'n2o', 'co', 'ch4'] # q must be first gas as it is used in unit conversion
            for k in keys:
                profs[p][k] = []
            for l in range(nlev):
                data = list(map(float, f.readline().split()))
                for i, k in enumerate(keys):
                    # Conversion from ppmv dry to selected units
                    if k == 'q': qdry[p,l] = data[i]
                    if (k not in GASMASSLIST) or gas_unit == UNIT_PPMVDRY:
                        profs[p][k].append(data[i])
                    elif gas_unit == UNIT_KGKGWET:
                        profs[p][k].append(ppmvdry2kgkgwet(data[i], k, qdry[p,l]))
                    else: # gas_unit == UNIT_PPMVWET
                        profs[p][k].append(ppmvdry2ppmvwet(data[i], k, qdry[p,l]))

            # Take missing surface variables from the profiles

            profs[p]['t2m'] = profs[p]['t'][-1]
            profs[p]['tskin'] = profs[p]['t'][-1]
            profs[p]['q2m'] = profs[p]['q'][-1]
            profs[p]['o32m'] = profs[p]['o3'][-1]

            # Fill in remaining profile values with defaults (these may be overwritten later)

            profs[p]['windu'] = (p+1) * 2 % 20 - 10.
            profs[p]['windv'] = (p+1) * 3 % 20 - 10.

            profs[p]['zenangle'] = (p+1) * 1.5 % 60
            profs[p]['azangle'] = 0.
            profs[p]['sunzenangle'] = (p+1) * 2.5 % 80
            profs[p]['sunazangle'] = (p+1) * 7 % 360
            profs[p]['elevation'] = 0.
            profs[p]['be'] = 0.
            profs[p]['cosbk'] = 0.
            profs[p]['ctp'] = 500.
            profs[p]['cfraction'] = 0.
            profs[p]['clwde_param'] = 0
            profs[p]['clw_scheme'] = 0
            profs[p]['icede_param'] = 0
            profs[p]['ice_scheme'] = 0
            profs[p]['watertype'] = 1
            profs[p]['salinity'] = 35.
            profs[p]['foam_fraction'] = 0.
            profs[p]['fastem'] = [3., 5., 15., 0.1, 0.3]
            profs[p]['windfetch'] = 100000.

    # Read 2016 updated CO2, N2O, CH4 data if supplied (ppmv dry)
    # Existing profile data is overwritten

    try:
        with open(fname_co2n2och4_2016, 'r') as f:
            for k in ['co2', 'n2o', 'ch4']:
                d = np.zeros((nprof, nlev), dtype=np.float64)
                for l in range(2): f.readline()
                for l in range(nlev):
                    d[:,l] = list(map(np.float64, f.readline().split(',')[1:]))
                for p in range(nprof):
                    for l in range(nlev):
                        # Conversion from ppmv dry to selected units
                        if (k not in GASMASSLIST) or gas_unit == UNIT_PPMVDRY:
                            profs[p][k][l] = d[p,l]
                        elif gas_unit == UNIT_KGKGWET:
                            profs[p][k][l] = ppmvdry2kgkgwet(d[p,l], k, qdry[p,l])
                        else: # gas_unit == UNIT_PPMVWET
                            profs[p][k][l] = ppmvdry2ppmvwet(d[p,l], k, qdry[p,l])
                f.readline()
    except:
        print('No updated CO2/N2O/CH4 file found for div83, continuing with older profiles')

    # Read SO2 data if supplied (ppmv dry)

    try:
        so2 = np.zeros((nprof, nlev), dtype=np.float64)
        with open(fname_so2, 'r') as f:
            f.readline()
            for l in range(nlev):
                so2[:,l] = list(map(np.float64, f.readline().split(',')[1:]))
        for p in range(nprof):
            profs[p]['so2'] = []
            for l in range(nlev):
                # Conversion from ppmv dry to selected units
                k = 'so2'
                if (k not in GASMASSLIST) or gas_unit == UNIT_PPMVDRY:
                    profs[p][k].append(so2[p,l])
                elif gas_unit == UNIT_KGKGWET:
                    profs[p][k].append(ppmvdry2kgkgwet(so2[p,l], k, qdry[p,l]))
                else: # gas_unit == UNIT_PPMVWET
                    profs[p][k].append(ppmvdry2ppmvwet(so2[p,l], k, qdry[p,l]))
    except:
        print('No SO2 file found for div83, continuing without SO2 profiles')

    return profs


# -----------------------------------------------------------------------------
# Functions for writing out test suite files
# -----------------------------------------------------------------------------

def write_common(profs, dirname, gui=False):
    """Writes the common files for all profiles in profs in directory dirname:
       atm/simple_cloud.txt, ground/*.txt, angles.txt, be.txt, datetime.txt"""

    if gui:
        for p in range(profs['nprof']):
            d = dirname + '/{:03d}.py'.format(p + 1)
            with open(d , 'a') as f:
                f.write('\nself["GAS_UNITS"] = {}'.format(profs[p]['gas_units']))

                f.write('\nself["CTP"] = {}'.format(profs[p]['ctp']))
                f.write('\nself["CFRACTION"] = {}'.format(profs[p]['cfraction']))

                #f.write('\nself["CLWDE_PARAM"] = {}'.format(profs[p]['clwde_param']))
                #f.write('\nself["CLW_SCHEME"] = {}'.format(profs[p]['clw_scheme']))
                #f.write('\nself["ICEDE_PARAM"] = {}'.format(profs[p]['icede_param']))
                #f.write('\nself["ICE_SCHEME"] = {}'.format(profs[p]['ice_scheme']))

                f.write('\nself["S2M"]["T"] = {}'.format(profs[p]['t2m']))
                f.write('\nself["S2M"]["Q"] = {}'.format(profs[p]['q2m']))
                f.write('\nself["S2M"]["O"] = {}'.format(profs[p]['o32m']))
                f.write('\nself["S2M"]["P"] = {}'.format(profs[p]['p2m']))
                f.write('\nself["S2M"]["U"] = {}'.format(profs[p]['windu']))
                f.write('\nself["S2M"]["V"] = {}'.format(profs[p]['windv']))
                f.write('\nself["S2M"]["WFETC"] = {}'.format(profs[p]['windfetch']))

                f.write('\nself["SKIN"]["SURFTYPE"] = {}'.format(profs[p]['surftype']))
                f.write('\nself["SKIN"]["WATERTYPE"] = {}'.format(profs[p]['watertype']))
                f.write('\nself["SKIN"]["T"] = {}'.format(profs[p]['tskin']))
                f.write('\nself["SKIN"]["SALINITY"] = {}'.format(profs[p]['salinity']))
                f.write('\nself["SKIN"]["FOAM_FRACTION"] = {}'.format(profs[p]['foam_fraction']))

                f.write('\nself["ELEVATION"] = {}'.format(profs[p]['elevation']))
                f.write('\nself["ZENANGLE"] = {}'.format(profs[p]['zenangle']))
                f.write('\nself["AZANGLE"] = {}'.format(profs[p]['azangle']))
                f.write('\nself["SUNZENANGLE"] = {}'.format(profs[p]['sunzenangle']))
                f.write('\nself["SUNAZANGLE"] = {}'.format(profs[p]['sunazangle']))
                f.write('\nself["LATITUDE"] = {}'.format(profs[p]['lat']))
                f.write('\nself["LONGITUDE"] = {}'.format(profs[p]['lon']))

                #f.write('\nself["BE"] = {}'.format(profs[p]['be']))
                #f.write('\nself["COSBK"] = {}'.format(profs[p]['cosbk']))

            write_list(d, np.array(profs[p]['fastem'][:]), gui, 'SKIN"]["FASTEM')
            write_list(d, np.array(profs[p]['datetime'][0:3]), gui, 'DATE')
            write_list(d, np.array(profs[p]['datetime'][3:6]), gui, 'TIME')

    else:
        for p in range(profs['nprof']):
            d = dirname + '/{:03d}/atm/'.format(p + 1)
            with open(d + 'simple_cloud.txt', 'w') as f:
                f.write('&simple_cloud\n')
                f.write('  ctp       = ' + str(profs[p]['ctp']) + '\n')
                f.write('  cfraction = ' + str(profs[p]['cfraction']) + '\n')
                f.write('/\n')

            #with open(d + 'clw_scheme.txt', 'w') as f:
                #f.write('&clw_scheme_nml\n')
                #f.write('  clw_scheme = ' + str(profs[p]['clw_scheme']) + '\n')
                #f.write('  clwde_param = ' + str(profs[p]['clwde_param']) + '\n')
                #f.write('/\n')

            #with open(d + 'ice_scheme.txt', 'w') as f:
                #f.write('&ice_scheme_nml\n')
                #f.write('  ice_scheme = ' + str(profs[p]['ice_scheme']) + '\n')
                #f.write('  icede_param = ' + str(profs[p]['icede_param']) + '\n')
                #f.write('/\n')

            d = dirname + '/{:03d}/ground/'.format(p + 1)
            with open(d + 's2m.txt', 'w') as f:
                f.write('&s2m\n')
                f.write('  s0%t     = ' + str(profs[p]['t2m']) + '\n')
                f.write('  s0%q     = ' + str(profs[p]['q2m']) + '\n')
                f.write('  s0%o     = ' + str(profs[p]['o32m']) + '\n')
                f.write('  s0%p     = ' + str(profs[p]['p2m']) + '\n')
                f.write('  s0%u     = ' + str(profs[p]['windu']) + '\n')
                f.write('  s0%v     = ' + str(profs[p]['windv']) + '\n')
                f.write('  s0%wfetc = ' + str(profs[p]['windfetch']) + '\n')
                f.write('/\n')

            with open(d + 'skin.txt', 'w') as f:
                f.write('&skin\n')
                f.write('  k0%surftype        = ' + str(profs[p]['surftype']) + '\n')
                f.write('  k0%watertype       = ' + str(profs[p]['watertype']) + '\n')
                f.write('  k0%t               = ' + str(profs[p]['tskin']) + '\n')
                f.write('  k0%salinity        = ' + str(profs[p]['salinity']) + '\n')
                f.write('  k0%foam_fraction   = ' + str(profs[p]['foam_fraction']) + '\n')
                f.write('  k0%fastem(1)       = ' + str(profs[p]['fastem'][0]) + '\n')
                f.write('  k0%fastem(2)       = ' + str(profs[p]['fastem'][1]) + '\n')
                f.write('  k0%fastem(3)       = ' + str(profs[p]['fastem'][2]) + '\n')
                f.write('  k0%fastem(4)       = ' + str(profs[p]['fastem'][3]) + '\n')
                f.write('  k0%fastem(5)       = ' + str(profs[p]['fastem'][4]) + '\n')
                f.write('/\n')

            d = dirname + '/{:03d}/'.format(p + 1)
            with open(d + 'angles.txt', 'w') as f:
                f.write('&angles\n')
                f.write('  zenangle     = ' + str(profs[p]['zenangle']) + '\n')
                f.write('  azangle      = ' + str(profs[p]['azangle']) + '\n')
                f.write('  sunzenangle  = ' + str(profs[p]['sunzenangle']) + '\n')
                f.write('  sunazangle   = ' + str(profs[p]['sunazangle']) + '\n')
                f.write('  latitude     = ' + str(profs[p]['lat']) + '\n')
                f.write('  longitude    = ' + str(profs[p]['lon']) + '\n')
                f.write('  elevation    = ' + str(profs[p]['elevation']) + '\n')
                f.write('/\n')

            #with open(d + 'be.txt', 'w') as f:
                #f.write(str(profs[p]['be']) + '   ' + str(profs[p]['cosbk']) + '\n')

            with open(d + 'datetime.txt', 'w') as f:
                f.write('   '.join(map(str, profs[p]['datetime'])) + '\n')

            with open(d + 'gas_units.txt', 'w') as f:
                f.write('&units\n')
                f.write('  gas_units = ' + str(profs[p]['gas_units']) + '\n')
                f.write('/\n')


# =============================================================================
# Modify and write out the profile sets
# =============================================================================

# -----------------------------------------------------------------------------
# Change the zenith angle to a fixed value
# -----------------------------------------------------------------------------

def set_zenangle(profs, zen=None):
    """Set zenith angles to a new value. If zen is missing, angles cycle in
       steps of 10 degrees between 0 and 70 over the profile set."""
    if zen:
        zenlist = [zen]
    else:
        zenlist = range(0, 71, 10)

    for p in range(profs['nprof']):
        profs[p]['zenangle'] = zenlist[p % len(zenlist)]

    return profs

# -----------------------------------------------------------------------------
# Write out diverse profile set with no modifications
# -----------------------------------------------------------------------------

def make_div(dirname, profs, gui=False):
    """Write the given profiles without any modification"""

    make_dirs(dirname, profs['nprof'], gui)

    for p in range(profs['nprof']):
        if gui:
            proffn = dirname + '/{:03d}.py'.format(p + 1)
            write_list(proffn, profs[p]['p'], True, 'P')
        else:
            profdir = dirname + '/{:03d}/atm/'.format(p + 1)
            write_list(profdir + 'p.txt', profs[p]['p'])

        for v in PROF_LIST:
            if v in profs[p]:
                if gui:
                    gn = v.upper()
                    write_list(proffn, profs[p][v], gui, gn)
                else:
                    write_list(profdir + v + '.txt', profs[p][v])

    write_common(profs, dirname, gui)

# -----------------------------------------------------------------------------
# Write out diverse profile set interpolated to supplied levels
# -----------------------------------------------------------------------------

def make_div_interp(dirname, profs, pout, gui=False):
    """Interpolate each of the given profiles to the corresponding new levels
       in list p and write out."""

    make_dirs(dirname, profs['nprof'], gui)

    for p in range(profs['nprof']):
        if gui:
            proffn = dirname + '/{:03d}.py'.format(p + 1)
            write_list(proffn, pout[p], True, 'P')
        else:
            profdir = dirname + '/{:03d}/atm/'.format(p + 1)
            write_list(profdir + 'p.txt', pout[p])

        logpin = np.log(profs[p]['p'])
        logpout = np.log(pout[p])

        for v in PROF_LIST:
            if v in profs[p]:
                dout = np.interp(x=logpout, xp=logpin, fp=profs[p][v]) #, left=-999., right=-999.)
                if gui:
                    gn = v.upper()
                    write_list(proffn, dout, True, gn)
                else:
                    write_list(profdir + v + '.txt', dout)

    write_common(profs, dirname, gui)


# ---------------------------------------------------------------
# 52 diverse profiles - filenames as provided on NWP SAF web page
# ---------------------------------------------------------------

def make_div52(gui, gas_unit=UNIT_PPMVWET):
    # NB The CO2 profiles were not included in the original diverse 52 set
    profs = read_div52('diverse_52profiles_101L.dat', 'data52_aux.dat', 'diverse_52profiles_co2_101L.dat', gas_unit=gas_unit)

    make_div('div52', profs, gui)
    #make_div_interp('div52_54L', profs, [p54] * 52, gui)
    #make_div_interp('div52_44L', profs, [p44] * 52, gui)

    #for zen in [10., 30., 50.]:
        #profs = set_zenangle(profs, zen)
        #make_div('div52_zen' + str(int(zen)) + 'deg', profs, gui)


# -------------------------------------------------------------------------------------------
# 83 diverse profiles - filename as provided on NWP SAF web page (these are actually on 101L)
# -------------------------------------------------------------------------------------------

def make_div83(gui, gas_unit=UNIT_PPMVWET):
    profs = read_div83('ECMWF_83P_91L.dat', 'diverse_83profiles_co2_n2o_ch4_2016_101L.dat', 
                       'diverse_83profiles_so2_101L.dat', gas_unit=gas_unit)

    make_div('div83', profs, gui)
    #make_div_interp('div83_54L', profs, [p54] * 83, gui)


# =============================================================================
# Call required functions
# =============================================================================

def parse_args():
    parser = argparse.ArgumentParser(description='Generate RTTOV test profiles from diverse datasets', conflict_handler='resolve')
    parser.add_argument('-g', '--gui', dest='gui', action='store_true', help='generates Python file for RTTOV_GUI')
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    gui = args.gui

    make_div52(gui, gas_unit=UNIT_PPMVWET)
    make_div83(gui, gas_unit=UNIT_PPMVWET)
