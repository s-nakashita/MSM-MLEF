Notes for installing and running the NWP SAF Radiance Simulator v3.1.

----------------------
1. System requirements
----------------------

- Unix or Linux operating system
- Fortran 90 compiler that supports a small number of F2003 features
- Python3 (v3.7 or later*) for some ancillary scripts
- 20 MB free disk space for installation (considerably more required for output
  data files)
- 4GB RAM minimum recommended**

*Older versions of Python3 may work but are untested.

**The RAM requirement will vary considerably depending on the application.

The code has been tested successfully in one or more standard configurations
after installation with each of the following compilers.

ifort     v15.0.0, v17.0.7
pgfortran v18.7-0
gfortran  v8.1.0, v11.2.0

Newer versions of these Fortran compilers are expected to be compatible. It may 
also work with some older versions, but these have not been tested and are not
supported. 

The xlf compiler was supported for version 1 of RadSim but this is no longer the
case. A legacy build configuration file is provided, and users may attempt to 
install and run with xlf if they wish. The file can be edited if the specified 
options are no longer appropriate. However no additional support can be given in
the event of successful installation.

------------
2. Libraries
------------

Fortran versions of the following libraries are required. Each can be
downloaded from the given locations if not already available.

- RTTOV v13.1 (recommended) or v13.0

https://nwp-saf.eumetsat.int/site/software/rttov/

Note: You may also need to download the relevant RTTOV coefficient files.

- ecCodes (version 2.0.0 or later, version 2.23.0 or later recommended)

https://software.ecmwf.int/wiki/display/ECC/ecCodes+Home

NB For ingest of ICON cloud liquid and ice particle size fields, ecCodes
v2.23.0 or later is required. It is generally recommended to use the most
recent version of ecCodes.

- netCDF-4 (version 4.0 or later)

http://www.unidata.ucar.edu/software/netcdf/

------------

The following libraries may also be required but this is system dependent. Note
that if you have the above libraries installed then these should be present on
the system already.

- HDF5 (version 1.8.8 or later, used by netCDF and RTTOV)

http://www.hdfgroup.org/HDF5/

- Jasper and libaec (may be used by ecCodes)

---------------
3. Installation
---------------

All Radiance Simulator code is contained in the single tar file:
radsim-3.1.tar.gz.

You will also need access to some external libraries (see above).

Installation consists of the following steps.

1) Unpack the tar file in your installation directory

tar -xf radsim-3.1.tar.gz

- The directory should now contain, in addition, the following files

build/*
etc/*
src/*
radsim_install
readme.txt      # this file
user.cfg

2) Edit the user.cfg file to supply paths to external libraries. Also set the
   name of the compiler you wish to use (one of ifort, gfortran or pgfortran).

Notes on compiler options:

The files containing these can be found in the build/cfg/ directory. In some 
cases you may need to edit the relevant file if the default options are not 
suitable. Additional linker flags can be specified in the LDFLAGS variable.

It is recommended to compile both RTTOV and RadSim with OpenMP enabled. The
RadSim compiler flag files include the relevant OpenMP options by default.

All currently supported input profile datasets are written in big-endian byte
order (with the exception of GRIB and netCDF which are portable). It is assumed
in the case of the first 3 compilers listed that these will be used on a little-
endian platform and therefore byte-swapping for I/O has been included as a
compiler flag in each case. For the ifort compiler, the F_UFMTENDIAN environment
variable may be used to override this behaviour.

3) Run the install script.

./radsim_install

- The directory should now contain, in addition, the following files

bin/radsim.exe
bin/radsim_run  # Wrapper script for radsim.exe

4) Check the installation (optional)

./radsim_check_install

- This performs a simple simulation to check that the code is ready to use but
  is not a necessary part of the installation process.

-------------------
4. Running the code
-------------------

A Fortran namelist configuration file is required to drive the Radiance
Simulator. An example file radsim_cfg_example.nl and a cut down version
radsim_cfg_basic.nl can be found in the etc/ subdirectory of the installation
directory. As a minimum, you will need to specify the following in a copy of
one of those files

model_datafile = <path to profile dataset>
model_filetype = <type of file>
rttov_coeffs_dir = <path to directory containing RTTOV coefficient files>
platform = <satellite name>
inst = <satellite instrument>
satid = <satellite identifier>

Most other settings are optional but it's recommended that the output directory
is specified as output files can be large. See the User Guide for more details.
Also, it is recommended that an emissivity atlas is used. The default is not to
use one because it requires additional data files but these can be downloaded
from the RTTOV website.

Run the simulations by issuing the following command (assumes your namelist
file is called radsim_cfg.nl, but you are free to rename it)

bin/radsim_run radsim_cfg.nl

RadSim also provides a Python script src/scripts/radsim_run.py which generates a
configuration file and optionally calls radsim_run using this configuration
file. All RadSim options can be specified via arguments to this script: most
arguments are optional, and defaults are used where these are omitted. Annex A
of the User Guide provides more information.

----------
5. Outputs
----------

Output data is written to a netCDF file. You can optionally specify the file
name, otherwise the following naming convention applies:

radsim-<platform>_<satid>_<instrument>-<datatime>.nc

e.g.,

radsim-metop_2_amsua-201401010012.nc

The datatime is an optional suffix (of the form YYYYMMDDHHMM) and will be
omitted, along with the last '-' separator, if the profile dataset is not
related to a single validity time (e.g. the NWP SAF datasets). The data-time
corresponds to the validity time of the first set of fields in the file. By
default the output file is written to the current directory, but you can specify
an alternative directory in the output_dir configuration namelist variable.

Most output data are floating point arrays with the first dimension the number
of simulated observations (labelled dimension 'obs' in the file). Other
dimensions may be levels and channels.
