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
level = 850.0
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
data = ds.gh.sel(isobaricInhPa=level).values.reshape(nlat,nlon)
z = xr.DataArray(data, coords=[lat,lon],\
      dims=['latitude','longitude'], attrs=ds.gh.attrs)
print(z)
temp = ds.t.sel(isobaricInhPa=level).values.reshape(nlat,nlon)
temp = temp * units.kelvin
spfh = ds.q.sel(isobaricInhPa=level).values.reshape(nlat,nlon)
spfh = spfh * units('kg/kg')
pres = level * units.hPa
dpt = mpcalc.dewpoint_from_specific_humidity(\
      pres,temp,spfh)
data= mpcalc.equivalent_potential_temperature(\
      pres,temp,dpt)
thetae = xr.DataArray(data, coords=[lat,lon],\
      dims=['latitude','longitude'], \
      attrs={'name':'equivalent potential temperature','units':'K'})
print(thetae)
t = ds.time
dt = ds.step
t += dt
jst = pd.to_datetime(t.values) + t_offset
fig = plt.figure(figsize=(8,8), constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
if res>3:
#MSM9km, 27km
    tlevs = np.arange(282,360,3)
    if level == 850.0:
        zlevs = np.arange(1400,1560,8)
    if level == 925.0:
        zlevs = np.arange(700,860,8)
else:
#MSM3km
    tlevs = np.arange(309,354,3)
    if level == 850.0:
        zlevs = np.arange(1448,1520,4)
    if level == 925.0:
        zlevs = np.arange(730,810,4)
p = ax.contourf(lon,lat,thetae,tlevs,cmap='coolwarm',transform=ccrs.PlateCarree())
fig.colorbar(p,orientation='horizontal')
p2= ax.contour(lon,lat,z,zlevs,colors=['k'],transform=ccrs.PlateCarree())
ax.clabel(p2,manual=False,inline=True)
ax.coastlines()
ax.set_title(f'Z{int(level)}+'+r'$\theta_e$ '+jst.strftime('%Y-%m-%d %HJST'),fontsize=16)
fig.savefig(f'z{int(level)}+thetae_ft{ft:02d}.png',dpi=150)
#plt.show()
