
# Data for example code

# The arrays here can be used directly with pyrttov.
# For the direct wrapper interface the datetimes, angles, etc must be transposed
# as shown in the interface_example_rttovscatt_python.py code.

import numpy as np


# Units for gas profiles
gas_units = 2  # ppmv over moist air

# datetimes[6][nprofiles]: yy, mm, dd, hh, mm, ss
datetimes = np.array([[2015, 8, 1, 0, 0, 0],
                      [2015, 8, 1, 0, 0, 0]], dtype=np.int32)

# angles[2][nprofiles]: satzen, satazi
angles = np.array([[23., 0.],
                   [23., 0.]], dtype=np.float64)

# surftype[nprofiles]: surftype
surftype = np.array([1, 0], dtype=np.int32)

# surfgeom[3][nprofiles]: lat, lon, elev
surfgeom = np.array([[0., 0., 0.],
                     [10., 20., 0.]], dtype=np.float64)

# s2m[5][nprofiles]: 2m p, 2m t, 2m q, 10m wind u, v
s2m = np.array([[990.138, 279.638, 8507.6938, -2.69568, 3.88115],
                [989.504, 278.232, 6534.2795, -6.16443, 4.91045]], dtype=np.float64)

# skin[8][nprofiles]: skin T, salinity, foam_frac, fastem_coefsx5
skin = np.array([[280.403, 35., 0., 3.0, 5.0, 15.0, 0.1, 0.3],
                 [280.356, 35., 0., 3.0, 5.0, 15.0, 0.1, 0.3]], dtype=np.float64)

# zeeman[2][nprofiles]: be, cosbk
zeeman = np.array([[0., 0.],
                   [0., 0.]], dtype=np.float64)

#!       p        ph         t         q        cc       clw       ciw      rain      snow
#!   [hPa]     [hPa]       [K]    [ppmv]     [0-1]   [kg/kg]   [kg/kg]   [kg/kg]   [kg/kg]
prof_data = np.array(list(map(np.float64, 
"""0.498E-02 0.000E+00 0.225E+03 0.307E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.100E+00 0.100E-01 0.225E+03 0.614E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.292E+00 0.200E+00 0.235E+03 0.615E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.510E+00 0.384E+00 0.241E+03 0.617E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.796E+00 0.636E+00 0.243E+03 0.620E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.115E+01 0.956E+00 0.233E+03 0.617E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.158E+01 0.134E+01 0.222E+03 0.605E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.208E+01 0.181E+01 0.218E+03 0.559E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.267E+01 0.235E+01 0.220E+03 0.523E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.336E+01 0.298E+01 0.223E+03 0.526E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.419E+01 0.374E+01 0.223E+03 0.523E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.520E+01 0.465E+01 0.223E+03 0.521E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.644E+01 0.576E+01 0.221E+03 0.498E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.798E+01 0.713E+01 0.218E+03 0.486E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.989E+01 0.884E+01 0.214E+03 0.451E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.123E+02 0.109E+02 0.214E+03 0.389E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.152E+02 0.136E+02 0.215E+03 0.361E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.188E+02 0.168E+02 0.215E+03 0.363E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.233E+02 0.208E+02 0.214E+03 0.359E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.289E+02 0.258E+02 0.214E+03 0.354E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.358E+02 0.320E+02 0.215E+03 0.352E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.443E+02 0.396E+02 0.218E+03 0.352E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.546E+02 0.491E+02 0.219E+03 0.349E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.666E+02 0.602E+02 0.221E+03 0.345E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.804E+02 0.731E+02 0.222E+03 0.339E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.960E+02 0.877E+02 0.221E+03 0.336E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.113E+03 0.104E+03 0.221E+03 0.321E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.133E+03 0.123E+03 0.223E+03 0.311E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.154E+03 0.143E+03 0.222E+03 0.326E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.177E+03 0.165E+03 0.222E+03 0.345E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.201E+03 0.189E+03 0.220E+03 0.577E+01 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.228E+03 0.214E+03 0.213E+03 0.154E+02 0.000E+00 0.000E+00 0.000E+00 0.000E+00 0.000E+00
 0.256E+03 0.241E+03 0.212E+03 0.363E+02 0.100E+01 0.000E+00 0.182E-05 0.000E+00 0.316E-06
 0.286E+03 0.270E+03 0.218E+03 0.709E+02 0.758E+00 0.000E+00 0.364E-05 0.000E+00 0.133E-05
 0.317E+03 0.301E+03 0.224E+03 0.131E+03 0.484E+00 0.000E+00 0.319E-05 0.000E+00 0.192E-05
 0.350E+03 0.333E+03 0.230E+03 0.259E+03 0.875E+00 0.000E+00 0.906E-05 0.000E+00 0.406E-05
 0.384E+03 0.367E+03 0.236E+03 0.441E+03 0.100E+01 0.000E+00 0.247E-04 0.205E-08 0.130E-04
 0.419E+03 0.401E+03 0.241E+03 0.667E+03 0.828E+00 0.000E+00 0.251E-04 0.000E+00 0.218E-04
 0.456E+03 0.437E+03 0.245E+03 0.887E+03 0.414E+00 0.000E+00 0.921E-05 0.278E-08 0.235E-04
 0.493E+03 0.474E+03 0.247E+03 0.102E+04 0.102E+00 0.000E+00 0.116E-05 0.126E-07 0.214E-04
 0.530E+03 0.511E+03 0.250E+03 0.119E+04 0.109E+00 0.000E+00 0.134E-06 0.000E+00 0.188E-04
 0.568E+03 0.549E+03 0.252E+03 0.145E+04 0.102E+00 0.596E-07 0.435E-05 0.000E+00 0.170E-04
 0.605E+03 0.587E+03 0.255E+03 0.164E+04 0.164E+00 0.477E-06 0.907E-05 0.303E-07 0.148E-04
 0.642E+03 0.624E+03 0.258E+03 0.201E+04 0.203E+00 0.167E-05 0.123E-04 0.553E-07 0.134E-04
 0.679E+03 0.661E+03 0.261E+03 0.279E+04 0.195E+00 0.268E-05 0.995E-05 0.131E-06 0.124E-04
 0.714E+03 0.697E+03 0.264E+03 0.358E+04 0.297E+00 0.930E-05 0.184E-04 0.375E-06 0.132E-04
 0.748E+03 0.731E+03 0.266E+03 0.417E+04 0.242E+00 0.158E-04 0.175E-04 0.846E-06 0.137E-04
 0.780E+03 0.765E+03 0.268E+03 0.478E+04 0.266E+00 0.350E-04 0.234E-04 0.151E-05 0.146E-04
 0.811E+03 0.796E+03 0.270E+03 0.533E+04 0.391E+00 0.885E-04 0.355E-04 0.280E-05 0.163E-04
 0.839E+03 0.825E+03 0.271E+03 0.578E+04 0.602E+00 0.194E-03 0.469E-04 0.515E-05 0.186E-04
 0.865E+03 0.853E+03 0.272E+03 0.635E+04 0.781E+00 0.294E-03 0.371E-04 0.830E-05 0.203E-04
 0.889E+03 0.878E+03 0.273E+03 0.682E+04 0.891E+00 0.304E-03 0.343E-05 0.126E-04 0.201E-04
 0.910E+03 0.900E+03 0.274E+03 0.717E+04 0.961E+00 0.194E-03 0.000E+00 0.180E-04 0.000E+00
 0.928E+03 0.920E+03 0.275E+03 0.751E+04 0.883E+00 0.102E-03 0.000E+00 0.187E-04 0.000E+00
 0.944E+03 0.937E+03 0.276E+03 0.780E+04 0.617E+00 0.361E-04 0.000E+00 0.186E-04 0.114E-07
 0.957E+03 0.951E+03 0.277E+03 0.801E+04 0.000E+00 0.107E-05 0.000E+00 0.182E-04 0.283E-07
 0.967E+03 0.963E+03 0.278E+03 0.808E+04 0.000E+00 0.119E-05 0.000E+00 0.179E-04 0.917E-08
 0.976E+03 0.972E+03 0.278E+03 0.813E+04 0.000E+00 0.834E-06 0.000E+00 0.174E-04 0.000E+00
 0.982E+03 0.979E+03 0.279E+03 0.818E+04 0.000E+00 0.656E-06 0.000E+00 0.170E-04 0.000E+00
 0.986E+03 0.984E+03 0.279E+03 0.823E+04 0.000E+00 0.358E-06 0.000E+00 0.168E-04 0.000E+00
 0.989E+03 0.988E+03 0.279E+03 0.831E+04 0.000E+00 0.119E-06 0.000E+00 0.165E-04 0.132E-07""".split())), dtype=np.float64)

p_ex    = prof_data[0::9]
ph_ex   = prof_data[1::9]
t_ex    = prof_data[2::9]
q_ex    = prof_data[3::9]
cc_ex   = prof_data[4::9]
clw_ex  = prof_data[5::9]
ciw_ex  = prof_data[6::9]
rain_ex = prof_data[7::9]
snow_ex = prof_data[8::9]
usercfrac_ex = 0.

