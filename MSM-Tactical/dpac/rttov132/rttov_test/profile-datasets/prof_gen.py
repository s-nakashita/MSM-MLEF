#!/usr/bin/env python

# ============================================================================
# README
# ============================================================================

# This script can generate all the test profile datasets for the RTTOV test
# suite using the profiles defined in the associated data Python script.
# (This doesn't include the diverse profile datasets).

# The option --gui allows to generate the Python files for the RTTOV_GUI.

# The purpose of scripting this is to make it much easier to modify the test
# profiles, for example to account for missed cases or new code features.

# The code at the bottom of this script demonstrates how each type of profile
# dataset is created. The code is quite general so should accommodate changes
# in array dimensions such as the addition of new profiles, changes in
# profile levels, etc, as long as they are implemented consistently across
# the input data.

# Gas units in the profile data are assumed to be ppmv over dry air.
# Test suite profiles are generally converted to ppmv over moist air.

# The associated data script should define the following variables:
# NPROF - the number of profiles to create per dataset
# NLEV  - the number of levels in the underlying profile set (e.g. US76)
#
# p, t, q, o3, co2, co,  - arrays of profiles in underlying set (e.g. US76)
# n2o, ch4, so2
#
# clw                    - cloud liquid water array on same levels as
#                          basis profile set
#
# mmr_cldaer             - flag indicating cloud/aerosol units
#                          T => kg/kg; F => g/m^3 (cloud), cm^-3 (aerosol)
#
# cld101, cfrac101,      - additional 101L arrays for cloud/aerosol profiles
# icede101, aer101         (can also define profiles on different numbers of
#                          levels if required - these profiles are not tied
#                          to particular pressure levels and are not
#                          currently interpolated by the code below)
#
# s2m, skin, datetime,   - remaining profile variables
# satzen, satazi,
# sunzen, sunazi,
# lat, lon, elev, ctp,
# cfraction,
# be, cosbk, 
# clw_scheme, clwde_param
# ice_scheme, icede_param
#
# p101, p54, p51         - arrays containing RTTOV standard pressures
#
# ch4ref101, co2ref101,  - trace gas reference (background) profiles on 101L
# coref101, n2oref101,
# o3ref101, qref101,
# so2ref101

# co2ref54, o3ref54,     - trace gas reference profiles on 54L
# qref54
#
# co2ref51, qref51       - trace gas reference profiles on 51L
#
# Trace gases and standard pressures may be supplied for other levels.
#
# ============================================================================

from prof_gen_data import *
import numpy as np
import matplotlib.pyplot as plt
import os
import sys
import pprint
import argparse

UNIT_PPMVDRY = 0
UNIT_KGKGWET = 1
UNIT_PPMVWET = 2

# These masses should match those in rttov_const.F90
DRYAIRMASS = np.float64(28.9644)
GASMASSLIST = {'q'   : np.float64(18.01528),
               'o3'  : np.float64(47.9982),
               'co2' : np.float64(44.0095),
               'co'  : np.float64(28.0101),
               'ch4' : np.float64(16.04246),
               'n2o' : np.float64(44.0128),
               'so2' : np.float64(64.064)}

# ============================================================================
# Helper functions to create profiles and write profile datasets
# ============================================================================

# ----------------------------------------------------------------------------
# Conversion functions for gas units
# ----------------------------------------------------------------------------

def ppmvdry2kgkgwet(gas_ppmvdry, gasname, h2o_ppmvdry):
    """Convert ppmv dry to kgkg wet for gas gasname"""
    return GASMASSLIST[gasname] * gas_ppmvdry / (np.float64(1.E+06) * DRYAIRMASS + GASMASSLIST['q'] * h2o_ppmvdry)

def ppmvdry2ppmvwet(gas_ppmvdry, gasname, h2o_ppmvdry):
    """Convert ppmv dry to ppmv wet for gas gasname"""
    return gas_ppmvdry / (1. + np.float64(1.E-06) * h2o_ppmvdry)

def kgkgdry2ppmvdry(gas_kgkgdry, gasname):
    """Convert kgkg dry to ppmv dry for gas gasname"""
    return np.float64(1.E+06) * gas_kgkgdry * DRYAIRMASS / GASMASSLIST[gasname]

# Simple tests to ensure consistency between conversion functions
#assert(kgkgwet2ppmvwet(.2, 'o3', .3) == ppmvdry2ppmvwet(kgkgwet2ppmvdry(.2, 'o3'), 'o3', kgkgwet2ppmvdry(.3, 'q')))
#assert(kgkgwet2ppmvdry(ppmvdry2kgkgwet(1000., 'o3'), 'o3') == 1000.)


# ----------------------------------------------------------------------------
# These two functions generate 101L profiles using variations on the AIRS formula
# ----------------------------------------------------------------------------

def make_p(nlev, i_fix, p_fix, e):
    """Make new pressure profiles using the AIRS 101L formula"""
    coef = np.zeros([3,3])
    v = np.array(list(map(lambda x:x**(1./e), p_fix)))

    for r in range(3):
        for c in range(3):
          coef[r,c] = i_fix[r]**(2-c)

    abc = np.linalg.solve(coef, v)
    f = lambda i: (abc[0] * i**2 + abc[1] * i + abc[2])**e
    newp = list(map(f, range(nlev)))
    newp.reverse()
    return np.array(newp)

def make_nL(n):
    """Create NPROF n level pressure profiles using slightly variations on the AIRS formula
       This is fairly arbitrary, but it makes a selection of nice smooth profiles which
       differ significantly from one another"""
    plist = []
    for (i, f) in enumerate(np.linspace(0.93, 1.03, NPROF)):
        newp = make_p(n, [0, int(0.35 * n + i), n-1], [f * 1060., f * 300., f * 0.005], (7. + i)/2.)
        if np.max(newp) > 1100.:
            print('ERROR: maximum generated pressure is > 1100 hPa')
            sys.exit(1)
        plist.append(newp)
        #plt.plot(newp)
    #plt.show()
    return plist

# ----------------------------------------------------------------------------
# Function to create the directories for a new profile dataset
# ----------------------------------------------------------------------------

def make_dirs(d, np, gui=False):
    """Create the necessary directories under the given dirname"""
    if not os.path.exists(d):
        os.makedirs(d)
    for i in range(1, np + 1):
        if gui:
            fn = d + '/{:03d}.py'.format(i)
            with open(fn, 'w') as f:
                f.write('"""\n    Profile {}\n'.format(fn))
                f.write('        file automaticaly created by prof_gen.py script\n')
                f.write('""" \n\n')
                f.write('self["ID"] = "{}"'.format(fn))

        else:
            for sd in ['atm', 'ground']:
                profdir = (d + '/{:03d}/' + sd).format(i)
                if not os.path.exists(profdir):
                    os.makedirs(profdir)

# ----------------------------------------------------------------------------
# The following functions write out different kinds of profile data
# ----------------------------------------------------------------------------

def write_list(fn, data, gui=False, name='', gas_unit=UNIT_PPMVWET, h2o_dry=None):
    """Write a 1-D profile to the given filename"""
    dat = data
    if name.lower() in GASMASSLIST:
        if gas_unit == UNIT_KGKGWET:
            dat = ppmvdry2kgkgwet(data, name.lower(), h2o_dry)
        elif gas_unit == UNIT_PPMVWET:
            dat = ppmvdry2ppmvwet(data, name.lower(), h2o_dry)
    if gui:
        with open(fn, 'a') as f:
            f.write('\nself["{}"] = numpy.'.format(name))
            pprint.pprint(dat, stream=f)
    else:
        with open(fn, 'w') as f:
            for v in dat:
                f.write('{:12.6E}'.format(v) + '\n')


def write_refgas(dirname, dlist, flist, gui=False, gas_unit=UNIT_PPMVWET, h2o_dry=None):
    """Write out the ref gas profiles from the dlist to the files in flist"""
    if h2o_dry is None: h2o_dry = [None] * NPROF
    if gui:
        for i in range(NPROF):
            for (d, fn) in zip(dlist, flist):
                gn = fn.replace('.txt','').upper()
                profdir = dirname + '/{:03d}.py'.format(i + 1)
                write_list(profdir, d, gui, gn, gas_unit, h2o_dry[i])

    else:
        for i in range(NPROF):
            for (d, fn) in zip(dlist, flist):
                gn = fn.replace('.txt','').upper()
                profdir = dirname + '/{:03d}/atm/'.format(i + 1)
                write_list(profdir + fn, d, gui, gn, gas_unit, h2o_dry[i])


def write_common(dirname, gui=False, gas_unit=UNIT_PPMVWET, vars2m=None):
    """Writes the common files for all profiles in directory dirname:
       atm/simple_cloud.txt, ground/*.txt, angles.txt, be.txt, datetime.txt"""

    if vars2m is None:
        p2m = s2m['p']
        t2m = s2m['t']
        q2m = s2m['q']
        o2m = s2m['o']
    else:
        p2m, t2m, q2m, o2m = vars2m

    if gui:
        for i in range(NPROF):
            d = dirname + '/{:03d}.py'.format(i + 1)
            with open(d , 'a') as f:
                f.write('\nself["ID"] = {}'.format(prof_id[i]))

                f.write('\nself["GAS_UNITS"] = {}'.format(gas_unit))

                f.write('\nself["CTP"] = {}'.format(ctp[i]))
                f.write('\nself["CFRACTION"] = {}'.format(cfraction[i]))

                f.write('\nself["S2M"]["T"] = {}'.format(t2m[i]))
                if gas_unit == UNIT_KGKGWET:
                    f.write('\nself["S2M"]["Q"] = {}'.format(ppmvdry2kgkgwet(q2m[i], 'q', q2m[i])))
                    f.write('\nself["S2M"]["O"] = {}'.format(ppmvdry2kgkgwet(o2m[i], 'o3', q2m[i])))
                elif gas_unit == UNIT_PPMVWET:
                    f.write('\nself["S2M"]["Q"] = {}'.format(ppmvdry2ppmvwet(q2m[i], 'q', q2m[i])))
                    f.write('\nself["S2M"]["O"] = {}'.format(ppmvdry2ppmvwet(o2m[i], 'o3', q2m[i])))
                else:
                    f.write('\nself["S2M"]["Q"] = {}'.format(q2m[i]))
                    f.write('\nself["S2M"]["O"] = {}'.format(o2m[i]))
                f.write('\nself["S2M"]["P"] = {}'.format(p2m[i]))
                f.write('\nself["S2M"]["U"] = {}'.format(s2m['u'][i]))
                f.write('\nself["S2M"]["V"] = {}'.format(s2m['v'][i]))
                f.write('\nself["S2M"]["WFETC"] = {}'.format(s2m['wfetc'][i]))

                f.write('\nself["SKIN"]["SURFTYPE"] = {}'.format(skin['surftype'][i]))
                f.write('\nself["SKIN"]["WATERTYPE"] = {}'.format(skin['watertype'][i]))
                f.write('\nself["SKIN"]["T"] = {}'.format(skin['t'][i]))
                f.write('\nself["SKIN"]["SALINITY"] = {}'.format(skin['salinity'][i]))
                f.write('\nself["SKIN"]["FOAM_FRACTION"] = {}'.format(skin['foam_fraction'][i]))
                f.write('\nself["SKIN"]["SNOW_FRACTION"] = {}'.format(skin['snow_fraction'][i]))

                f.write('\nself["ELEVATION"] = {}'.format(elev[i]))
                f.write('\nself["ZENANGLE"] = {}'.format(satzen[i]))
                f.write('\nself["AZANGLE"] = {}'.format(satazi[i]))
                f.write('\nself["SUNZENANGLE"] = {}'.format(sunzen[i]))
                f.write('\nself["SUNAZANGLE"] = {}'.format(sunazi[i]))
                f.write('\nself["LATITUDE"] = {}'.format(lat[i]))
                f.write('\nself["LONGITUDE"] = {}'.format(lon[i]))

                f.write('\nself["BE"] = {}'.format(be[i]))
                f.write('\nself["COSBK"] = {}'.format(cosbk[i]))

            write_list(d, np.array(skin['fastem'][i][:]), gui, 'SKIN"]["FASTEM')
            write_list(d, np.array(datetime[i][0:3]), gui, 'DATE')
            write_list(d, np.array(datetime[i][3:6]), gui, 'TIME')

    else:
        for i in range(NPROF):
            d = dirname + '/{:03d}/atm/'.format(i + 1)
            with open(d + 'simple_cloud.txt', 'w') as f:
                f.write('&simple_cloud\n')
                f.write('  ctp       = ' + str(ctp[i]) + '\n')
                f.write('  cfraction = ' + str(cfraction[i]) + '\n')
                f.write('/\n')

            d = dirname + '/{:03d}/ground/'.format(i + 1)
            with open(d + 's2m.txt', 'w') as f:
                f.write('&s2m\n')
                f.write('  s0%t     = ' + str(t2m[i]) + '\n')
                if gas_unit == UNIT_KGKGWET:
                    f.write('  s0%q     = ' + str(ppmvdry2kgkgwet(q2m[i], 'q', q2m[i])) + '\n')
                    f.write('  s0%o     = ' + str(ppmvdry2kgkgwet(o2m[i], 'o3', q2m[i])) + '\n')
                elif gas_unit == UNIT_PPMVWET:
                    f.write('  s0%q     = ' + str(ppmvdry2ppmvwet(q2m[i], 'q', q2m[i])) + '\n')
                    f.write('  s0%o     = ' + str(ppmvdry2ppmvwet(o2m[i], 'o3', q2m[i])) + '\n')
                else:
                    f.write('  s0%q     = ' + str(q2m[i]) + '\n')
                    f.write('  s0%o     = ' + str(o2m[i]) + '\n')
                f.write('  s0%p     = ' + str(p2m[i]) + '\n')
                f.write('  s0%u     = ' + str(s2m['u'][i]) + '\n')
                f.write('  s0%v     = ' + str(s2m['v'][i]) + '\n')
                f.write('  s0%wfetc = ' + str(s2m['wfetc'][i]) + '\n')
                f.write('/\n')

            with open(d + 'skin.txt', 'w') as f:
                f.write('&skin\n')
                f.write('  k0%surftype        = ' + str(skin['surftype'][i]) + '\n')
                f.write('  k0%watertype       = ' + str(skin['watertype'][i]) + '\n')
                f.write('  k0%t               = ' + str(skin['t'][i]) + '\n')
                f.write('  k0%salinity        = ' + str(skin['salinity'][i]) + '\n')
                f.write('  k0%foam_fraction   = ' + str(skin['foam_fraction'][i]) + '\n')
                f.write('  k0%snow_fraction   = ' + str(skin['snow_fraction'][i]) + '\n')
                f.write('  k0%fastem(1)       = ' + str(skin['fastem'][i][0]) + '\n')
                f.write('  k0%fastem(2)       = ' + str(skin['fastem'][i][1]) + '\n')
                f.write('  k0%fastem(3)       = ' + str(skin['fastem'][i][2]) + '\n')
                f.write('  k0%fastem(4)       = ' + str(skin['fastem'][i][3]) + '\n')
                f.write('  k0%fastem(5)       = ' + str(skin['fastem'][i][4]) + '\n')
                f.write('/\n')

            d = dirname + '/{:03d}/'.format(i + 1)
            with open(d + 'angles.txt', 'w') as f:
                f.write('&angles\n')
                f.write('  zenangle     = ' + str(satzen[i]) + '\n')
                f.write('  azangle      = ' + str(satazi[i]) + '\n')
                f.write('  sunzenangle  = ' + str(sunzen[i]) + '\n')
                f.write('  sunazangle   = ' + str(sunazi[i]) + '\n')
                f.write('  latitude     = ' + str(lat[i]) + '\n')
                f.write('  longitude    = ' + str(lon[i]) + '\n')
                f.write('  elevation    = ' + str(elev[i]) + '\n')
                f.write('/\n')

            with open(d + 'be.txt', 'w') as f:
                f.write(str(be[i]) + '   ' + str(cosbk[i]) + '\n')

            with open(d + 'datetime.txt', 'w') as f:
                f.write('   '.join(list(map(str, datetime[i]))) + '\n')

            with open(d + 'gas_units.txt', 'w') as f:
                f.write('&units\n')
                f.write('  gas_units = ' + str(gas_unit) + '\n')
                f.write('/\n')

            with open(d + 'prof_id.txt', 'w') as f:
                f.write(prof_id[i] + '\n')


def write_cloud(dirname, cld, cfrac, clwde, icede, nlev, gui=False):
    """Write out cloud.txt, cfrac.txt, clwde.txt, clw_scheme.txt, icede.txt and ice_scheme.txt"""
    ntyp = int(cld.shape[1] / (nlev - 1))
    if cld.shape[1] % ntyp > 0:
        print('ERROR: number of cloud levels does not match supplied nlev, ', nlev)
        sys.exit(1)

    cloud_list = ['STCO', 'STMA', 'CUCC', 'CUCP', 'CUMA', 'CIRR']

    formcld = lambda x:'{:12.6E}'.format(x)
    form = lambda x:'{:6.3f}'.format(x)

    if gui:
        for i in range(NPROF):
            d = dirname + '/{:03d}.py'.format(i + 1)
            acld = cld.reshape((NPROF, nlev-1, ntyp))
            for ityp in range(ntyp):
                if( any(acld[i,:,ityp] > 0) ):
                    write_list(d, acld[i,:,ityp], gui, cloud_list[ityp])
            write_list(d, cfrac[i,:], gui, "CFRAC")

            with open(d , 'a') as f:
                f.write('\nself["CLW_SCHEME"] = {}'.format(clw_scheme[i]))
                f.write('\nself["CLWDE_PARAM"] = {}'.format(clwde_param[i]))
                f.write('\nself["ICEDE_PARAM"] = {}'.format(icede_param[i]))
                f.write('\nself["ICE_SCHEME"] = {}'.format(ice_scheme[i]))
                f.write('\nself["MMR_CLDAER"] = ' + (1 if mmr_cldaer == 'T' else 0))

            if np.any(clwde[i,:] > 0):
                write_list(d, clwde[i,:], gui, "CLWDE")

            if np.any(icede[i,:] > 0):
                write_list(d, icede[i,:], gui, "ICEDE")
    else:
        for i in range(NPROF):
            d = dirname + '/{:03d}/atm/'.format(i + 1)
            with open(d + 'cloud.txt', 'w') as f:
                for l in range(nlev - 1):
                    f.write(' '.join(list(map(formcld, cld[i,ntyp*l:ntyp*(l+1)]))) + '\n')

            with open(d + 'cfrac.txt', 'w') as f:
                for l in range(nlev - 1):
                    f.write(form(cfrac[i,l]) + '\n')

            with open(d + 'clw_scheme.txt', 'w') as f:
                f.write('&clw_scheme_nml\n')
                f.write('  clw_scheme = ' + str(clw_scheme[i]) + '\n')
                f.write('  clwde_param = ' + str(clwde_param[i]) + '\n')
                f.write('/\n')

            with open(d + 'ice_scheme.txt', 'w') as f:
                f.write('&ice_scheme_nml\n')
                f.write('  ice_scheme = ' + str(ice_scheme[i]) + '\n')
                f.write('  icede_param = ' + str(icede_param[i]) + '\n')
                f.write('/\n')

            with open(d + 'mmr_cldaer.txt', 'w') as f:
                f.write('&cldaer_units\n')
                f.write('  mmr_cldaer = ' + mmr_cldaer + '\n')
                f.write('/\n')

            if np.all(clwde[i,:] >= 0):
                with open(d + 'clwde.txt', 'w') as f:
                    for l in range(nlev - 1):
                        f.write(form(clwde[i,l]) + '\n')

            if np.all(icede[i,:] >= 0):
                with open(d + 'icede.txt', 'w') as f:
                    for l in range(nlev - 1):
                        f.write(form(icede[i,l]) + '\n')


def write_aerosol(dirname, aer, nlev, gui=False, cams=False):
    """Write out aerosl.txt"""
    ntyp = int(aer.shape[1] / (nlev - 1))
    if aer.shape[1] % ntyp > 0:
        print('ERROR: number of aerosol levels does not match supplied nlev, ', nlev)
        sys.exit(1)

    if cams:
        aerosol_list = ['BCAR','DUS1','DUS2','DUS3','SULP','SSA1','SSA2','SSA3','OMAT']
    else:
        aerosol_list = ['INSO','WASO','SOOT','SSAM','SSCM','MINM','MIAM','MICM','MITR',  \
                        'SUSO','VOLA','VAPO','ASDU']
    ntyp_out = len(aerosol_list)

    form = lambda x:'{:12.6E}'.format(x)

    aaer = aer.reshape((NPROF, nlev-1, ntyp))
    if gui:
        for i in range(NPROF):
            d = dirname + '/{:03d}.py'.format(i + 1)
            for ityp in range(ntyp_out):
                if( any(aaer[i,:,ityp] > 0) ):
                    write_list(d, aaer[i,:,ityp], gui, aerosol_list[ityp] )

            with open(d , 'a') as f:
                f.write('\nself["MMR_CLDAER"] = ' + (1 if mmr_cldaer == 'T' else 0))
    else:
        for i in range(NPROF):
            d = dirname + '/{:03d}/atm/'.format(i + 1)
            with open(d + 'aerosl.txt', 'w')  as f:
                for l in range(nlev - 1):
                    f.write(' '.join(list(map(form, aaer[i,l,0:ntyp_out]))) + '\n')

            with open(d + 'mmr_cldaer.txt', 'w') as f:
                f.write('&cldaer_units\n')
                f.write('  mmr_cldaer = ' + mmr_cldaer + '\n')
                f.write('/\n')

def write_rttovscatt(dirname, cld_profiles_hydro, cld_profiles_hydro_frac, flux_conversion, user_cfrac, plist, nlev, gui=False):
    """Write cld_profiles.txt"""

    nvar = NHYDRO_SCATT #int(cld_profiles.shape[1] / nlev)
    form = lambda x:'{:9.3E}'.format(x)

    for i in range(NPROF):
        # Calculate pressure half-levels (excluding the bottom one which is set to s2m%p in the test suite)
        ph = np.zeros(nlev, dtype=np.float64)
        ph[1:] = np.exp(0.5 * (np.log(plist[i][:-1]) + np.log(plist[i][1:])))
        d = dirname + '/{:03d}/atm/'.format(i + 1)
        with open(d + 'cld_profiles.txt', 'w')  as f:
            f.write('{:3d}       ! nhydro\n'.format(NHYDRO_SCATT))
            for j in range(NHYDRO_SCATT): f.write('{:1d} '.format(flux_conversion[i][j]))
            f.write('! flux_conversion\n')
            f.write('{:8.4f}  ! user cfrac\n'.format(user_cfrac[i]))
            for l in range(nlev):
                f.write(' '.join(list(map(form, [ph[l]] + list(cld_profiles_hydro[i][l*nvar:(l+1)*nvar]) + \
                                                          list(cld_profiles_hydro_frac[i][l*nvar:(l+1)*nvar])))) + '\n')

        # The 2m pressures need to lie below the bottom pressure level
        d = dirname + '/{:03d}/ground/'.format(i + 1)
        with open(d + 's2m.txt', 'r') as f:
            lines = f.readlines()
        with open(d + 's2m.txt', 'w') as f:
            for line in lines:
                if 's0%p' not in line:
                    f.write(line)
                else:
                    newp = min(1100., np.exp(np.log(plist[i][-1]) + 0.2 * (np.log(plist[i][-1]) - np.log(plist[i][-2]))))
                    f.write('  s0%p     = ' + str(newp) + '\n')

# ----------------------------------------------------------------------------
# The following two functions are the main ones to call for generating profile
# datasets from the underlying set (e.g. US76)
# ----------------------------------------------------------------------------

def make_interp_profs(dirname, gui, pout, g, gas_unit=UNIT_PPMVWET, fix2m=False):
    """Write out the NPROF underlying profiles interpolated onto the p levels in each member of pout"""

    make_dirs(dirname, NPROF, gui)

    h2o_dry = [None] * NPROF
    p2m, t2m, q2m, o2m = [0.] * NPROF, [0.] * NPROF, [0.] * NPROF, [0.] * NPROF
    for i in range(NPROF):
        if gui:
            proffn = dirname + '/{:03d}.py'.format(i + 1)
            write_list(proffn, pout[i], True, 'P')
        else:
            profdir = dirname + '/{:03d}/atm/'.format(i + 1)
            write_list(profdir + 'p.txt', pout[i])

        logpin = np.log(p[i,:])
        logpout = np.log(pout[i])
        p2m[i] = pout[i][-1]
        for (din, fn) in zip(DATALIST[g], FILELIST[g]):
            dout = np.interp(x=logpout, xp=logpin[::-1], fp=din[i,::-1], left=-999., right=-999.)
            for j in range(len(dout)):
                if dout[j] >= 0.:
                    continue
                dout[j] = dout[j-1] + (logpout[j] - logpout[j-1]) * (dout[j-1] - dout[j-2]) / (logpout[j-1] - logpout[j-2])
            if fn == 't.txt': t2m[i] = dout[-1]
            if fn == 'q.txt':
                h2o_dry[i] = dout
                q2m[i] = dout[-1]
            if fn == 'o3.txt': o2m[i] = dout[-1]
            if gui:
                gn = fn.replace('.txt','').upper()
                write_list(proffn, dout, gui, gn, gas_unit, h2o_dry[i])
            else:
                gn = fn.replace('.txt','').upper()
                write_list(profdir + fn, dout, gui, gn, gas_unit, h2o_dry[i])

    vars2m = ([p2m, t2m, q2m, o2m] if fix2m else None)

    write_common(dirname, gui, gas_unit, vars2m)

    return h2o_dry # For ref_gas generation

def make_nointerp_profs(dirname, gui, nlevels, g, gas_unit=UNIT_PPMVWET):
    """Write out the bottom nlevels of the NPROF underlying profiles without any interpolation (1 <= nlevels <= NLEV)"""

    make_dirs(dirname, NPROF, gui)

    nlevels = max(1, min(nlevels, NLEV))
    for i in range(NPROF):
        if gui:
            proffn = dirname + '/{:03d}.py'.format(i + 1)
            write_list(proffn, p[i,nlevels-1::-1], True, 'P')
        else:
            profdir = dirname + '/{:03d}/atm/'.format(i + 1)
            write_list(profdir + 'p.txt', p[i,nlevels-1::-1])

        for (din, fn) in zip(DATALIST[g], FILELIST[g]):
            if gui:
                gn = fn.replace('.txt','').upper()
                write_list(proffn, din[i,nlevels-1::-1], gui, gn, gas_unit, q[i,nlevels-1::-1])
            else:
                gn = fn.replace('.txt','').upper()
                write_list(profdir + fn, din[i,nlevels-1::-1], gui, gn, gas_unit, q[i,nlevels-1::-1])

    write_common(dirname, gui, gas_unit)


# ============================================================================
# Define gas combinations for output profile datasets
# ============================================================================

# Each "DATALIST" contains lists of profile data in the data module for each gas combination
# Each "FILELIST" contains corresponding names of output files

DATALIST = {'allgas' : [t, q, o3, co2, co, n2o, ch4, so2],
            'nogas'  : [t, q],
            'co2'    : [t, q, co2],
            'o3'     : [t, q, o3],
            'co2o3'  : [t, q, o3, co2],
            'clw'    : [t, q, clw]}
FILELIST = {'allgas' : ['t.txt', 'q.txt', 'o3.txt', 'co2.txt', 'co.txt', 'n2o.txt', 'ch4.txt', 'so2.txt'],
            'nogas'  : ['t.txt', 'q.txt'],
            'co2'    : ['t.txt', 'q.txt', 'co2.txt'],
            'o3'     : ['t.txt', 'q.txt', 'o3.txt'],
            'co2o3'  : ['t.txt', 'q.txt', 'o3.txt', 'co2.txt'],
            'clw'    : ['t.txt', 'q.txt', 'clw.txt']}
GASLIST = DATALIST.keys()

REFGAS101DATALIST = {'allgasref' : [o3ref101, co2ref101, coref101, n2oref101, ch4ref101, so2ref101]}
REFGAS101FILELIST = {'allgasref' : ['o3.txt', 'co2.txt', 'co.txt', 'n2o.txt', 'ch4.txt', 'so2.txt']}
REFGAS101LIST = REFGAS101DATALIST.keys()

REFGAS54DATALIST = {'o3ref'     : [o3ref54],
                    'co2o3ref'  : [o3ref54, co2ref54]}
REFGAS54FILELIST = {'o3ref'     : ['o3.txt'],
                    'co2o3ref'  : ['o3.txt', 'co2.txt']}
REFGAS54LIST = REFGAS54DATALIST.keys()

REFGAS51DATALIST = {'co2ref'    : [co2ref51]}
REFGAS51FILELIST = {'co2ref'    : ['co2.txt']}
REFGAS51LIST = REFGAS51DATALIST.keys()


# ============================================================================
# Make the profile sets
# ============================================================================

# ----------------------------------------------------------------------------
# Standard and varying
# ----------------------------------------------------------------------------

def make_std_varying(gui, gas_unit=UNIT_PPMVWET):
    for g in GASLIST:
        if g == 'co2': continue # CO2-only is done below (only used by SSU)

        # standard 54L
        dirname = 'standard54lev_' + g
        make_interp_profs(dirname, gui, [p54] * NPROF, g, gas_unit)

        # varying 101L
        dirname = 'varying101lev_' + g
        plist = make_nL(101)
        make_interp_profs(dirname, gui, plist, g, gas_unit)

        if g == 'clw': continue # CLW not required by test suite on standard 101L

        # standard 101L
        dirname = 'standard101lev_' + g
        make_interp_profs(dirname, gui, [p101] * NPROF, g, gas_unit)

    # CO2-only on standard 51L and varying 101L
    g = 'co2'
    dirname = 'standard51lev_' + g
    make_interp_profs(dirname, gui, [p51] * NPROF, g, gas_unit)

    dirname = 'varying101lev_' + g
    plist = make_nL(101)
    make_interp_profs(dirname, gui, plist, g, gas_unit)

    # We need some additional 'nogas' profiles for the refgas_vs_nogas test
    g = 'nogas'
    dirname = 'standard54lev_' + g + '_ppmvdry'
    make_interp_profs(dirname, gui, [p54] * NPROF, g, UNIT_PPMVDRY)

    dirname = 'standard101lev_' + g + '_ppmvdry'
    make_interp_profs(dirname, gui, [p101] * NPROF, g, UNIT_PPMVDRY)

    # Make one set of kg/kg test profiles for testing in the GUI
    g = 'allgas'
    dirname = 'standard101lev_' + g + '_kgkg'
    make_interp_profs(dirname, gui, [p101] * NPROF, g, UNIT_KGKGWET)

    ## Make profiles with 2m variables set to lowest level values
    #g = 'allgas'
    #dirname = 'standard54lev_' + g + '_fix2m'
    #make_interp_profs(dirname, gui, [p54] * NPROF, g, gas_unit, fix2m=True)

# ----------------------------------------------------------------------------
# Cloud and aerosol
# ----------------------------------------------------------------------------

# Note that the cloud/aerosol profiles are not interpolated
# and are not associated with particular pressure levels:
# the script creates varying 50L profiles and then "drops"
# the cld/aer profiles in.

def make_cld_aer(gui, gas_unit=UNIT_PPMVWET):
    for g in ['co2o3']:
        # cloud 50L
        dirname = 'cld50lev_' + g
        plist = make_nL(50)
        make_interp_profs(dirname, gui, plist, g, gas_unit)
        write_cloud(dirname, cld50, cfrac50, clwde50, icede50, 50, gui)

        # aerosol 50L
        dirname = 'aer50lev_' + g
        plist = make_nL(50)
        make_interp_profs(dirname, gui, plist, g, gas_unit)
        write_aerosol(dirname, aer50, 50, gui)

        # CAMS aerosol 50L
        dirname = 'aercams50lev_' + g
        plist = make_nL(50)
        make_interp_profs(dirname, gui, plist, g, gas_unit)
        write_aerosol(dirname, aer50, 50, gui, cams=True)

        # cloud+aerosol 50L
        dirname = 'cldaer50lev_' + g
        plist = make_nL(50)
        make_interp_profs(dirname, gui, plist, g, gas_unit)
        write_cloud(dirname, cld50, cfrac50, clwde50, icede50, 50, gui)
        write_aerosol(dirname, aer50, 50, gui)

# ----------------------------------------------------------------------------
# Truncated (low model top)
# ----------------------------------------------------------------------------

# Exactly the same as above, but the pressure profile is truncated

def make_trunc(gui, gas_unit=UNIT_PPMVWET):
    for g in ['allgas', 'o3']:
        # standard 54L truncated (low model top)
        dirname = 'standard54lev_' + g + '_trunc'
        make_interp_profs(dirname, gui, [p54[19:]] * NPROF, g, gas_unit)

# ----------------------------------------------------------------------------
# With reference gas profiles
# ----------------------------------------------------------------------------

# Create the profile on standard levels as above and then
# overwrite the gas profiles with the references

def make_refgas(gui, gas_unit=UNIT_PPMVDRY):
    for g in REFGAS101LIST:
        dirname = 'standard101lev_' + g
        h2o_dry = make_interp_profs(dirname, gui, [p101] * NPROF, g[:-3], gas_unit)
        write_refgas(dirname, REFGAS101DATALIST[g], REFGAS101FILELIST[g], gui, gas_unit, h2o_dry)

    for g in REFGAS54LIST:
        dirname = 'standard54lev_' + g
        h2o_dry = make_interp_profs(dirname, gui, [p54] * NPROF, g[:-3], gas_unit)
        write_refgas(dirname, REFGAS54DATALIST[g], REFGAS54FILELIST[g], gui, gas_unit, h2o_dry)

    #for g in REFGAS51LIST:
        #dirname = 'standard51lev_' + g
        #h2o_dry = make_interp_profs(dirname, gui, [p51] * NPROF, g[:-3], gas_unit)
        #write_refgas(dirname, REFGAS51DATALIST[g], REFGAS51FILELIST[g], gui, gas_unit, h2o_dry)

# ----------------------------------------------------------------------------
# Uninterpolated basis profiles
# ----------------------------------------------------------------------------

def make_noninterp(gui, gas_unit=UNIT_PPMVWET):
    for g in ['allgas']:
        ## US76 on 43L (up to ~0.005hPa)
        #dirname = 'us76_43lev_' + g
        #make_nointerp_profs(dirname, gui, 43, g, gas_unit)

        # US76 on 50L
        dirname = 'us76_50lev_' + g
        make_nointerp_profs(dirname, gui, NLEV, g, gas_unit)

# ----------------------------------------------------------------------------
# RTTOV-SCATT profiles
# ----------------------------------------------------------------------------

def make_rttovscatt(gui, gas_unit=UNIT_PPMVWET):
    for g in ['nogas', 'o3']:
        dirname = 'scatt' + str(NLEV_SCATT) + 'lev_' + g
        plist = make_nL(NLEV_SCATT)
        make_interp_profs(dirname, gui, plist, g, gas_unit)
        write_rttovscatt(dirname, cld_profiles_hydro, cld_profiles_hydro_frac, \
                         flux_conversion, user_cfrac, plist, NLEV_SCATT, gui)

# ----------------------------------------------------------------------------
# Call required functions
# ----------------------------------------------------------------------------

def parse_args():
    parser = argparse.ArgumentParser(description='Generate RTTOV test profiles', conflict_handler='resolve')
    parser.add_argument('-g', '--gui', dest='gui', action='store_true', help='generates Python file for RTTOV_GUI')
    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    gui = args.gui

    make_std_varying(gui)
    make_cld_aer(gui)
    make_trunc(gui)
    make_refgas(gui)
    make_noninterp(gui)
    make_rttovscatt(gui)

