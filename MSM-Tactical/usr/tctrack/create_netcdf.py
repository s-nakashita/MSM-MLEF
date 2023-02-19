import sys
import netCDF4 as nc4
import numpy as np
from datetime import datetime,timedelta
from pathlib import Path
import pandas as pd
import librotate

#Usage echo yyyymmddhh | python create_netcdf.py
#param = sys.stdin.readline().strip("\n").split(" ")
#yyyymmddhh = param[0]

datadir = Path('./')
nc_scl  = Path('np_scl.nc')
nc_uv  = Path('np_uv.nc')

outvar_scl = ['msl','t','rh']
outvar_uv = ['u10','v10','u','v']
stname = {
'msl':'mean sea level pressure',
't':'Temparature',
'rh':'Relative Humidity',
'u10':'U component of wind',
'v10':'V component of wind',
'u':'U component of wind',
'v':'V component of wind'}
units = {'msl':'Pa','t':'K','rh':'%',\
    'u10':'m/s','v10':'m/s','u':'m/s','v':'m/s'}

outvar_dict = {}

outnc = nc4.Dataset(Path('np.nc'), 'w')
# create output information about dimensions and variables
in_scl = nc4.Dataset(datadir/nc_scl,'r')
in_uv = nc4.Dataset(datadir/nc_uv,'r')
nlev = in_uv.variables["level"][:].size
nlat = in_uv.variables["latitude"][:].size
nlon = in_uv.variables["longitude"][:].size
print(f"nlev={nlev},nlat={nlat},nlon={nlon}")

time = outnc.createDimension('time',None)
level = outnc.createDimension('level',nlev)
lat = outnc.createDimension('lat',nlat)
lon = outnc.createDimension('lon',nlon)

times = outnc.createVariable("time",np.float64,("time",))
times.units = "seconds since 1970-01-01 00:00:00.0 0:00"
times.calendar = "standard"
times.axis = "T"
outvar_dict["time"] = times

levels = outnc.createVariable("level",np.float64,("level",))
levels.units = "hPa"
levels.axis = "Z"
outvar_dict["level"] = levels

latitudes = outnc.createVariable("lat",np.float64,("lat",))
latitudes.units = "degrees_north"
latitudes.axis = "Y"
outvar_dict["lat"] = latitudes

longitudes = outnc.createVariable("lon",np.float64,("lon",))
longitudes.units = "degrees_east"
longitudes.axis = "X"
outvar_dict["lon"] = longitudes

nvar_scl = len(outvar_scl)
nvar_uv = len(outvar_uv)
for i in range(nvar_scl):
    vkey = outvar_scl[i]
    if i==0: #msl
        var = outnc.createVariable(vkey,np.float32,("time","lat","lon"))
    else:
        var = outnc.createVariable(vkey,np.float32,("time","level","lat","lon"))
    var.standard_name = stname[vkey]
    var.units = units[vkey]
    outvar_dict[vkey] = var
for i in range(nvar_uv):
    vkey = outvar_uv[i]
    if i < 2: #u10,v10
        var = outnc.createVariable(vkey,np.float32,("time","lat","lon"))
    else:
        var = outnc.createVariable(vkey,np.float32,("time","level","lat","lon"))
    var.standard_name = stname[vkey]
    var.units = "m/s"
    outvar_dict[vkey] = var
print(outvar_dict)

# scalar
print(in_scl.variables["time"].units)
outvar_dict["level"][:] = in_scl.variables["level"][:]
outvar_dict["lat"][:] = in_scl.variables["latitude"][:]
outvar_dict["lon"][:] = in_scl.variables["longitude"][:]
for t in range(len(in_scl.variables["time"][:])):
    date = nc4.num2date(in_scl.variables["time"][t],in_scl.variables["time"].units)
    t0 = nc4.date2num(date,outvar_dict["time"].units)
    print(date,t0)
    outvar_dict["time"][t] = t0
    for name in in_scl.variables.keys():
        if(name == "time" or name == "latitude" \
           or name == "longitude" or name == "level"):
            continue
        if(name in outvar_dict):
            print(name)
            outvar_dict[name][t,:] = in_scl.variables[name][t,:]
            print(outvar_dict[name][:].shape)
        else:
            print("error")
# vector
for t in range(len(in_uv.variables["time"][:])):
    for name in in_uv.variables.keys():
        if(name == "time" or name == "latitude" \
           or name == "longitude" or name == "level"):
            continue
        if(name in outvar_dict):
            print(name)
            outvar_dict[name][t,:] = in_uv.variables[name][t,:]
            print(outvar_dict[name][:].shape)
        else:
            print("error")
print("time",outnc.variables["time"][:])
print("level",outnc.variables["level"][:])
print("lat",outnc.variables["lat"][:])
print("lon",outnc.variables["lon"][:])