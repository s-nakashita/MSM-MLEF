#!/usr/bin/env python3.8
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
import xarray as xr
import pandas as pd
import datetime
from metpy.units import units
import metpy.calc as mpcalc
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
t_offset = datetime.timedelta(hours=9)
if len(sys.argv)<=2:
    print(f'Usage : {sys.argv[0]} resolution forecast_time')
    exit() 
res = int(sys.argv[1])
ft = int(sys.argv[2])
# location of Shionomisaki
lons = 135.76
lats = 33.43

# read GRIB-1 dataset list
ds = xr.open_dataset(f'r_pgb.f{ft:02d}',engine='cfgrib',\
    backend_kwargs={'filter_by_keys':\
    {'typeOfLevel':'meanSea'}})
# reshape data
lat1d = ds.latitude.values
lon1d = ds.longitude.values
if res==3 or res==9:
    nlon=385; nlat=325
elif res==1:
    nlon=193; nlat=145
else:
    nlon=325; nlat=289
lat = lat1d.reshape(nlat,nlon)[:,0]
lon = lon1d.reshape(nlat,nlon)[0,:]
data = ds.msl.values.reshape(nlat,nlon)
data *= 1e-2
slp = xr.DataArray(data, coords=[lat,lon],\
 dims=['latitude','longitude'], \
 attrs={'name':'Mean Sea Level Pressure','units':'hPa'})
if ft>0:
    ds = xr.open_dataset(f'r_pgb.f{ft:02d}',engine='cfgrib',\
    backend_kwargs={'filter_by_keys':\
    {'typeOfLevel':'surface',
     'stepType':'accum'}})
    data = ds.tp.values.reshape(nlat,nlon)
    apcp = xr.DataArray(data, coords=[lat,lon],\
     dims=['latitude','longitude'], \
     attrs={'name':'Total precipitation','units':'kg/m^2'})
t = ds.time
dt = ds.step
t += dt
jst = pd.to_datetime(t.values) + t_offset
fig = plt.figure(figsize=(8,8), constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree(central_longitude=180.0))
#ax.add_feature(cfeature.OCEAN)
ax.add_feature(cfeature.LAND)
# scale like JMA
tlevs = [0.5,1,5,10,20,30,50,80]
tcols = ('azure','paleturquoise','deepskyblue','royalblue','yellow','orange','orangered','purple')
# scale like GSMaP
tlevs = [0.1,0.5,1.0,2.0,3.0,5.0,10.0,15.0,20.0,25.0]
tcols = ('mediumblue','royalblue','deepskyblue','lime','greenyellow','yellow','orange','orangered','red','purple')
if res==27:
#RSM27km
    plevs = np.arange(992,1028,4)
elif res==9:
#MSM9km
    plevs = np.arange(992,1028,2)
else:
#MSM3km
    plevs = np.arange(992,1028,1)
if ft>0:
    p = ax.contourf(lon,lat,apcp,tlevs,\
        colors=tcols,extend='max',\
        transform=ccrs.PlateCarree())
    fig.colorbar(p,orientation='horizontal')
p2= ax.contour(lon,lat,slp,plevs,colors=['k'],transform=ccrs.PlateCarree())
ax.clabel(p2,manual=False,inline=True)
ax.coastlines()
ax.set_title('SLP+'+' APCP1H'+jst.strftime('%Y-%m-%d %HJST'),fontsize=16)
fig.savefig(f'slp_apcp_ft{ft:02d}.png',dpi=150)
#plt.show()
