#!/usr/bin/env python3.8
import os
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
import xarray as xr
import pandas as pd
import datetime
from metpy.units import units
import metpy.calc as mpcalc
import cartopy.crs as ccrs
import sys
if len(sys.argv)<=2:
    print(f'Usage : {sys.argv[0]} resolution forecast_time')
    exit() 
res = int(sys.argv[1])
ft = int(sys.argv[2])
basetime = datetime.datetime(1900,1,1,0)
t_offset = datetime.timedelta(hours=9)
# read GRIB-1 dataset list
ds = xr.open_dataset(f'r_pgb.f{ft:02d}',engine='cfgrib',\
    backend_kwargs={'filter_by_keys':\
    {'typeOfLevel':'isobaricInhPa'}})
lon1d = ds.longitude.values
lat1d = ds.latitude.values
if res == 3 or res == 9:
    nlon=385; nlat=325
elif res==1:
    nlon=193; nlat=145
else:
    nlon=325; nlat=289
lat = lat1d.reshape(nlat,nlon)[:,0]
lon = lon1d.reshape(nlat,nlon)[0,:]
print(ds.data_vars)
data = ds.gh.sel(isobaricInhPa=300.0).values.reshape(nlat,nlon)
z300 = xr.DataArray(data, coords=[lat,lon],\
      dims=['latitude','longitude'], attrs=ds.gh.attrs)
print(z300)
udata = ds.u.sel(isobaricInhPa=300.0).values.reshape(nlat,nlon)
vdata = ds.v.sel(isobaricInhPa=300.0).values.reshape(nlat,nlon)
data = np.sqrt(udata**2 + vdata**2)
ws = xr.DataArray(data, coords=[lat,lon],\
      dims=['latitude','longitude'], \
      attrs={'name':'Horizontal wind speed','units':'m/s'})
print(ws)
t = ds.time
dt = ds.step
t += dt
jst = pd.to_datetime(t.values) + t_offset
fig = plt.figure(figsize=(8,8), constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
if res>3:
#MSM9km, 27km
    tlevs = np.arange(30,80,10)
    zlevs = np.arange(8600,9800,40)
else:
#MSM3km
    tlevs = np.arange(30,80,10)
    zlevs = np.arange(8600,9800,20)
p = ax.contourf(lon,lat,ws,tlevs,transform=ccrs.PlateCarree())
fig.colorbar(p,orientation='horizontal')
p2= ax.contour(lon,lat,z300,zlevs,colors=['k'],transform=ccrs.PlateCarree())
ax.clabel(p2,manual=False,inline=True)
ax.coastlines()
ax.set_title('Z300+'+r'$\|\mathbf{u}\|$ '+jst.strftime('%Y-%m-%d %HJST'),fontsize=16)
fig.savefig(f'z300+wspd_ft{ft:02d}.png',dpi=150)
#plt.show()
