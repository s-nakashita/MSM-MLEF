
# ====================================================================

# This module defines a collection of functions for extracting data
# from the RTTOV test suite input and output files.

# Of particular interest to users/developers are two functions
# which read data from the output test files for a selection of
# profiles and channels. These are quite general, especially
# readChannelData, and together should allow data for specific
# profiles/channels to be read from any test suite output file.

# The main inputs are the test directory, input file name (for
# readChannelData), variable name to read, the profile list, the channel
# list and the number of levels of data to read (readChannelData).

# readChannelData - read per channel data from the radiance.txt or
#                   transmission.txt output files. This can be TOA
#                   radiances or level-to-space quantities. This
#                   can also read emissivity_out.txt, emissivity_k.txt,
#                   and other similar files.

# readJacobians   - read Jacobians from profiles_k.txt and profiles_k_rec.txt

# calcStats       - given the output from readChannelData or readJacobians
#                   calculates mean, stdv and RMS values over the profiles.
#                   Useful when looking at the difference between tests.

# calcWeightingFunction - given pressure (or logp) and transmittance/optical depth
#                         arrays returns "dtau/dp"


# Additional functions:

# getProfileList   - get list of profile dirs in testDir/in/profiles/
# getProfileDirs   - get directory names of specified profiles
# readPressure     - read pressures for specified profiles
# readProfileVar   - read profile variable data for specified profiles
# readNameListVar  - read profile data from namelist input files e.g. skin.txt
# readIntList      - read a list of integers from a file
# getChanprof      - return a list of lists representing chanprof for the whole test
# getChanprofPCrec - return a list of lists representing chanprof for PC rec rads
# getChanprofIndex - return list of indices into RTTOV chanprof(:) for given channel list
# checkChanList    - check that a given channel list is valid for the selected profile

# Helper functions for readChannelData and readJacobians:

# readSetUp     - does some basic checks to make sure the requested files exist and
#                 returns the chanprof and chanprof index lists
# readStructure - checks that the variable is in the given file and returns info
#                 about the number of items per block and lines per block in the file

from __future__ import print_function
import os
import numpy as np

# --- Read channel data ----------------------------------------------

def readChannelData(testDir, fileName, varName, profList, chanList, nlevels=1, model='direct', pcrec=False):
    """ Read per-channel data (e.g. output radiances, transmittances)
          testDir is the RTTOV test directory (e.g. tests.1.ifort/amsua/001)
          fileName the file to read (i.e. radiance.txt or transmission.txt)
          varName is the section to read
          profList is a list of profile numbers
          chanList is a list of channel numbers, same channels returned for each profile
          nlevels optionally specifies how many levels-worth of data to read per channel
          model is optional and allows you to specify the RTTOV model output dir (direct/tl/ad/k) to read from
          pcrec should be True if reading reconstructed radiances/BTs from pcscores.txt
        Returns numpy array indexed by [iprof][ichan][ilev] (last dimension not present if nlevels == 1)
    """

    # Construct output full path to output file
    fullFileName = testDir + '/out/' + model + '/' + fileName

    # Do basic checks on inputs and get chanprof and ichanprof lists
    chanprof, ichanprof = readSetUp(testDir, fullFileName, profList, chanList, pcrec=pcrec)
    if chanprof is None:
        return np.array([])

    # Read entire data file
    with open(fullFileName, 'r') as f:
        lines = f.readlines()

    # Check the array variable is present and determine record size
    result = readStructure(fullFileName, lines, varName)
    if result:
        recordSize, nlinesPerRecord = result
    else:
        return np.array([])

    # Set up output data array
    data = np.zeros((len(profList), len(chanList), nlevels))

    try:
      # Find required section by varName
      iline = 1
      str1 = '%' + varName.upper() + ' '
      str2 = varName.upper() + ' '
      while str1 not in lines[iline-1] and lines[iline-1][:len(str2)] != str2:
          iline += 1

      # Read the data into output array
      for ilev in range(nlevels):
          record = []
          for k in range(nlinesPerRecord):
              record.extend(map(float, lines[iline+k].split()))
          for iprof in range(len(profList)):
              for (ichan, i) in enumerate(ichanprof[iprof]):
                  data[iprof][ichan][ilev] = record[i]
          iline += nlinesPerRecord + 1
    except:
        print('Error reading channel data')
        return np.array([])

    if nlevels == 1:
        return data[:,:,0]
    else:
        return data

# --------------------------------------------------------------------

# --- Read Jacobians -------------------------------------------------

def readJacobians(testDir, varName, profList, chanList, pcrec=None, icldaer=0, kfile=None, kdir='k'):
    """ Read Jacobians from profiles_k.txt or profiles_k_rec.txt
          testDir is the RTTOV test directory (e.g. tests.1.ifort/amsua/001)
          varName is the variable of interest
          profList is a list of profile numbers
          chanList is a list of channel numbers, same channels returned for each profile
          pcrec optionally specifies the PC rec rad chanlist
          icldaer optionally specifies the column index for cloud/aerosol Jacobians (counted from 0)
          kfile can be used to specify cld_profiles.txt, for example, for RTTOV-SCATT Jacobians
          kdir can be used to specify e.g. k_bf directory for brute force Jacobians
        Returns numpy array indexed by [iprof][ichan][ilev] (last dimension not present if nlevels == 1)
    """

    # Construct output full path to output file
    if kfile is not None:
        fullFileName = testDir + '/out/' + kdir + '/' + kfile
    elif pcrec:
        fullFileName = testDir + '/out/' + kdir + '/profiles_k_rec.txt'
    else:
        fullFileName = testDir + '/out/' + kdir + '/profiles_k.txt'

    # Do basic checks on inputs and get chanprof and ichanprof lists
    chanprof, ichanprof = readSetUp(testDir, fullFileName, profList, chanList, pcrec)
    if not chanprof:
        return np.array([])

    # Read entire data file
    with open(fullFileName, 'r') as f:
        lines = f.readlines()

    # Check the profile variable is present and determine nlevels
    struct = readStructure(fullFileName, lines, varName)
    if struct:
        nlevels, nlinesPerRecord = struct
    else:
        return np.array([])

    # Set up output data array
    data = np.zeros((len(profList), len(chanList), nlevels))

    try:
        iline = 0
        for iprof in range(len(profList)):
            for ichan, i in enumerate(ichanprof[iprof]):

                # Find the variable name plus index number e.g. "(3)%T" for this variable/channel
                while iline < len(lines):
                    line = lines[iline]
                    if ')%' + varName.upper() + ' ' in line:
                        profknum = int(line[line.find('(')+1:line.find(')')])
                        if profknum == i + 1:
                            break
                    iline += 1
                iline += 1

                # If necessary skip blocks until we find the one specified by icldaer
                if icldaer > 0:
                    ica = icldaer
                    while ica > 0:
                        iline += nlinesPerRecord + 1
                        if ')' in lines[iline]:
                            print('Cannot find selected cld/aer type')
                            return np.array([])
                        ica -= 1

                # Read in the data for this block
                record = []
                for k in range(nlinesPerRecord):
                    record.extend(map(float, lines[iline+k].split()))
                data[iprof][ichan][:] = record[:]
                iline += nlinesPerRecord + 1
    except:
        print('Error reading Jacobian data')
        return np.array([])

    if nlevels == 1:
        return data[:,:,0]
    else:
        return data

# --------------------------------------------------------------------

# --- Read Hydrometeor (+fraction) Jacobians -------------------------

def readHydroJacobians(testDir, varName, profList, chanList, ihydro=0, kdir='k'):
    """ Read HYDRO/HYDRO_FRAC Jacobians from cld_profiles_k.txt
          testDir is the RTTOV test directory (e.g. tests.1.ifort/amsua/scatt)
          varName is the variable of interest (HYDRO or HYDRO_FRAC)
          profList is a list of profile numbers
          chanList is a list of channel numbers, same channels returned for each profile
          ihydro specifies the column index (counted from 0)
          kdir can be used to specify e.g. k_bf directory for brute force Jacobians
        Returns numpy array indexed by [iprof][ichan][ilev]
    """

    # Construct output full path to output file
    fullFileName = testDir + '/out/' + kdir + '/cld_profiles_k.txt'

    # Do basic checks on inputs and get chanprof and ichanprof lists
    chanprof, ichanprof = readSetUp(testDir, fullFileName, profList, chanList)
    if not chanprof:
        return np.array([])

    # Read entire data file
    with open(fullFileName, 'r') as f:
        lines = f.readlines()

    # Check the profile variable is present and determine nlevels
    struct = readHydroStructure(fullFileName, lines, varName)
    if struct:
        nlevels, nhydro, nlinesPerRecord = struct
    else:
        return np.array([])

    # Set up output data array
    data = np.zeros((len(profList), len(chanList), nlevels))

    try:
        iline = 0
        for iprof in range(len(profList)):
            for ichan, i in enumerate(ichanprof[iprof]):

                # Find the variable name plus index number e.g. "(3)%T" for this variable/channel
                while iline < len(lines):
                    line = lines[iline]
                    if ')%' + varName.upper() + ' ' in line:
                        profknum = int(line[line.find('(')+1:line.find(')')])
                        if profknum == i + 1:
                            break
                    iline += 1
                iline += 1

                for lev in range(nlevels):
                    record = []
                    for k in range(nlinesPerRecord):
                        record.extend(map(float, lines[iline+k].split()))
                    data[iprof][ichan][lev] = record[ihydro]
                    iline += nlinesPerRecord + 1
    except:
        print('Error reading Hydro Jacobian data')
        return np.array([])

    if nlevels == 1:
        return data[:,:,0]
    else:
        return data

# --------------------------------------------------------------------


# --- Calculate stats ------------------------------------------------

def calcStats(data):
    """ Given an array of data, calculate mean, stdv and RMS value over first dimension.
        Typically the first dimension is profile index.
    """

    # Input data and output stats shapes are usually one of the following:
    # data[nprof][nchan]            stats[nchan]
    # data[nprof][nlevels]          stats[nlevels]
    # data[nprof][nchan][nlevels]   stats[nchan][nlevels]

    shape = data.shape
    nprof = shape[0]
    mean = np.zeros(shape[1:])
    stdv = np.zeros(shape[1:])
    rmsv = np.zeros(shape[1:])

    for i in range(nprof):
        mean += data[i][:]
        rmsv += data[i][:] * data[i][:]

    mean = mean / nprof
    rmsv = rmsv / nprof
    stdv = np.sqrt(rmsv - mean * mean)
    rmsv = np.sqrt(rmsv)

    maxv = np.max(np.abs(data), axis=0)

    return mean, stdv, rmsv, maxv


def calcWeightingFunction(p, t):
    """ Calculate dt/dp where p is typically an array of pressure profiles
        p[nprof,nlevels] (or logp) and t is an array of transmittances
        (or optical depths) t[nprof,nchan,nlevels].
        Calculation is performed as: (t(lev-1) - t(lev)) / (p(lev) - p(lev-1))
        Result is an array of size [nprof,nchan,nlevels-1] """

    nprof, nchan, nlev = t.shape
    result = np.zeros((nprof, nchan, nlev-1))
    for ip in range(nprof):
        dp = p[ip,1:] - p[ip,:nlev-1]
        for ic in range(nchan):
            result[ip,ic,:] = (t[ip,ic,:nlev-1] - t[ip,ic,1:]) / dp
    return result

# --------------------------------------------------------------------


# --- Read input profile datasets ------------------------------------

def readPressure(testDir, profList):
    """ Return a numpy array of floats containing the pressure profiles
    """
    return readProfileVar(testDir, profList, 'p.txt')

def readProfileVar(testDir, profList, fileName, column=0):
    """ Return numpy array of floats containing the profiles in filename (p, T, q, etc).
        Column can be used to specify a column to read aerosol or cloud input files.
    """
    data = []
    profDirs = getProfileDirs(testDir, profList)
    for p, d in zip(profList, profDirs):
        fullFileName= testDir + '/in/profiles/' + d + '/atm/' + fileName
        data.append([])
        try:
            with open(fullFileName, 'r') as f:
                for line in f:
                    if line.strip():
                        data[-1].append(float(line.split()[column]))
        except:
            print('Cannot find valid ', fullFileName)
            if column > 0:
                print('Is the cloud/aerosol index correct?')
            data = []
    return np.array(data)

def readNameListVar(testDir, profList, fileName, varName, dtype=float):
    """ Return numpy array of floats containing the profile variables given by
        varName in the test input fileName (e.g. skin T in ground/skin.txt).
        The varName is case sensitive and must uniquely match the required
        variable. The fileName must include any sub-dirs e.g. ground/
    """
    data = []
    profDirs = getProfileDirs(testDir, profList)
    for p, d in zip(profList, profDirs):
        fullFileName= testDir + '/in/profiles/' + d + '/' + fileName
        found = False
        with open(fullFileName, 'r') as f:
            for line in f:
                if varName in line:
                    data.append(dtype(line.split()[-1]))
                    found = True
                    break
            if not found:
                data = [0.]
                print('Error reading ' + varName + ' in ' + fullFileName)
    return np.array(data)

# --------------------------------------------------------------------


# --- Return information about profiles ------------------------------

def getProfileList(testDir):
    """ Return list of strings of profile directories for selected test
    """
    profDir = testDir + '/in/profiles/'
    try:
        return sorted(os.listdir(profDir))
    except:
        print('Cannot find profiles dir: ', profDir)
        return []

def getProfileDirs(testDir, profList):
    """ Return list of strings of the profile directory names for profList (counting from 1)
    """
    profDirList = getProfileList(testDir)
    if max(profList) > len(profDirList):
        print('Invalid profile number: ', max(profList))
        return []
    return [profDirList[i] for i in map(lambda x:x-1, profList)]

# --------------------------------------------------------------------


# --- Read channel and profile lists ---------------------------------

def readIntList(fullFileName):
    """ Read a file containing list of integers and return integer list
    """
    data = []
    try:
        with open(fullFileName, 'r') as f:
            for line in f:
                data.extend(map(int, line.split()))
    except:
        print('Cannot find valid ', fullFileName)
    return data

def getChanprof(testDir):
    """ Return a list of lists of ints containing the channels associated with each profile
    """
    chans = readIntList(testDir + '/in/channels.txt')
    profs = readIntList(testDir + '/in/lprofiles.txt')
    chanprof = []
    for i in range(max(profs)):
        chanprof.append([])
    for (c, p) in zip(chans, profs):
        chanprof[p-1].append(c)
    return chanprof

def getChanprofPCrec(testDir):
    """ Return a list of lists of ints containing the reconstructed channels for PCs
        (same channels for each profile)
    """
    chans = readIntList(testDir + '/in/channels_rec.txt')
    if not chans:
        return
    profs = readIntList(testDir + '/in/lprofiles.txt')
    # Add list of all channels for each profile
    chanprof = []
    for i in range(max(profs)):
        chanprof.append(chans)
    return chanprof

def getChanprofIndex(chanprof, profile, chanList):
    """ List of indices into the RTTOV chanprof(:) array corresponding to the chanlist.
        NB This assumes you've checked the chanlist against chanprof already.
    """
    ilo = sum(map(len, chanprof[:profile-1]))
    ichanprof = []
    for c in chanList:
        ichanprof.append(ilo + chanprof[profile-1].index(c))
    return ichanprof

def checkChanList(chanprof, profile, chanList):
    """ Return non-zero value if any element of chanlist is not in the channel list of profile
    """
    for c in chanList:
        if c not in chanprof[profile-1]:
            return 1
    return 0

# --------------------------------------------------------------------



# --- Helper functions for reading data ------------------------------

def readSetUp(testDir, fullFileName, profList, chanList, pcrec=None):
    """ Carries out some basic checks and returns the chanprof and ichanprof
        lists containing chanprof data and indices into RTTOV chanprof structure
    """

    # Check file to read exists
    if not os.path.isfile(fullFileName):
        print('Cannot find input file ', fullFileName)
        return None, None

    # Check profile number
    nprof = len(getProfileList(testDir))
    if max(profList) > nprof:
        print('Profile number is too large, max allowed is ', nprof)
        return None, None

    # Get the chanprof structure and check channel list is valid for all profiles
    chanprof = getChanprofPCrec(testDir) if pcrec else getChanprof(testDir)
    for p in profList:
        if checkChanList(chanprof, p, chanList):
            chanFile = 'channels_rec.txt' if pcrec else 'channels.txt'
            print('Channel list does not match ' + chanFile + ' for profile ', p, ' in ' + testDir)
            print('Valid channels: ', chanprof[p-1])
            return None, None

    # Get indices to chanprof for each profile
    ichanprof = []
    for prof in profList:
        ichanprof.append(getChanprofIndex(chanprof, prof, chanList))

    return chanprof, ichanprof

def readStructure(fullFileName, lines, varName):
    """ Determine if varName blocks exist in the file contained in lines
        and if so return the number of items per block and the number of
        lines over which this is spread in the file
    """

    try:
        # Find first varName block
        iline = 0
        str1 = '%' + varName.upper() + ' '
        str2 = varName.upper() + ' '
        while iline < len(lines):
            if str1 in lines[iline] or lines[iline][:len(str2)] == str2:
                break
            iline += 1
        if iline == len(lines):
            print('Variable ' + varName.upper() + ' not found in ' + fullFileName)
            return

        # Get the size of each block
        record = []
        iline += 1
        nlinesPerRecord = 0
        while True:
            line = lines[iline + nlinesPerRecord]
            if ')' in line or not line.strip() or iline >= len(lines):
                break
            record.extend(map(float, line.split()))
            nlinesPerRecord += 1
        return len(record), nlinesPerRecord
    except:
        print('Failed to determine block size, problem with file format?')
        return

def readHydroStructure(fullFileName, lines, varName):
    """ Determine if varName blocks exist in the file contained in lines
        and if so return the number of hydro types (items per block), the number
        of lines the block is spread over, and the number of levels (blocks)
    """

    try:
        # Find first varName block
        iline = 0
        str1 = '%' + varName.upper() + ' '
        str2 = varName.upper() + ' '
        while iline < len(lines):
            if str1 in lines[iline] or lines[iline][:len(str2)] == str2:
                break
            iline += 1
        if iline == len(lines):
            print('Variable ' + varName.upper() + ' not found in ' + fullFileName)
            return

        # Get the size of each block
        record = []
        iline += 1
        nlinesPerRecord = 0
        while True:
            line = lines[iline + nlinesPerRecord]
            if ')' in line or not line.strip() or iline >= len(lines):
                break
            record.extend(map(float, line.split()))
            nlinesPerRecord += 1
        nlev = 0
        while True:
            nlev += 1
            line = lines[iline + nlev * (nlinesPerRecord + 1)]
            if ')' in line:
                break
        return nlev, len(record), nlinesPerRecord
    except:
        print('Failed to determine block size, problem with file format?')
        return

# --------------------------------------------------------------------

if __name__ == "__main__":
    print('To start the RTTOV test plotting GUI run the rttov_test_plot.py script')



