#!/usr/bin/env python3.8
import os
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
import xarray as xr
import pandas as pd
import datetime
from metpy.units import units
from metpy.calc import mixing_ratio_from_relative_humidity
basetime = datetime.datetime(1900,1,1,0)
t_offset = datetime.timedelta(hours=9)
# location of Shionomisaki
lons = 135.76
lats = 33.43
ds = xr.open_dataset('r_pgb.f00',engine='cfgrib',\
    backend_kwargs={'filter_by_keys':\
    {'typeOfLevel':'surface'}})
lon1d = ds.longitude.values
lat1d = ds.latitude.values
if lat1d.size == 125125:
    nlon=385; nlat=325
else:
    nlon=193; nlat=145
lat = lat1d.reshape(nlat,nlon)[:,0]
lon = lon1d.reshape(nlat,nlon)[0,:]
# read GRIB-1 dataset list
tfile = "t2m+q2m_shio.txt"
if os.path.isfile(tfile):
    data = np.loadtxt(tfile)
    hour = data[0,:]
    t2m = data[1,:]
    q2m = data[2,:]
    time = [basetime + datetime.timedelta(hours=h) for h in hour]
else:
    t2m = []
    q2m = []
    time = []
    hour = []
    for ft in range(34):
        ds = xr.open_dataset(f'r_pgb.f{ft:02d}',engine='cfgrib',\
        backend_kwargs={'filter_by_keys':\
        {'typeOfLevel':'heightAboveGround',\
         'level':2}})
        t = ds.time
        dt = ds.step
        t += dt
        jst = pd.to_datetime(t.values) + t_offset
        time.append(jst)
        delta = jst - basetime
        h = delta.total_seconds()/3600.
        hour.append(h)
        data = ds.t2m.values.reshape(nlat,nlon)
        da = xr.DataArray(data, coords=[lat,lon],\
             dims=['latitude','longitude'])
        val = da.sel(latitude=lats,longitude=lons,method="nearest").values
        val -= 273.15
        t2m.append(val)
        data = ds.q.values.reshape(nlat,nlon)
        da = xr.DataArray(data, coords=[lat,lon],\
             dims=['latitude','longitude'])
        val = da.sel(latitude=lats,longitude=lons,method="nearest").values
        val *= 1e3
        q2m.append(val)
    data = np.stack([hour,t2m,q2m])
    np.savetxt(tfile,data)
print(t2m)
print(q2m)
print(time)
amedas = np.loadtxt("amedas20220511.txt")
hour_ame = amedas[:,0]
time_ame = [datetime.datetime(2022,5,11,0)+datetime.timedelta(hours=h) \
            for h in hour_ame]
t2m_ame = amedas[:,1]*units.degC
rh_ame = amedas[:,3]*units.percent
slp_ame = amedas[:,4]*units.hPa
w2m_ame = mixing_ratio_from_relative_humidity(\
         slp_ame,t2m_ame,rh_ame)
q2m_ame = w2m_ame / (1.0 + w2m_ame) * 1e3 
fig, axes = plt.subplots(ncols=1,nrows=2,sharex=True,figsize=(14,14))
axes[0].plot(time,t2m,label='MSM')
axes[0].plot(time_ame,t2m_ame,color='r',label='AMeDAS')
axes[0].set_title('Temperature at 2m height',fontsize=16)
axes[0].set_ylabel(r'$^{\circ}C$',fontsize=14)
axes[1].plot(time,q2m)
axes[1].plot(time_ame,q2m_ame,color='r',label='AMeDAS')
axes[1].set_title('Specific humidity at 2m height',fontsize=16)
axes[1].set_ylabel('g/kg',fontsize=14)
axes[1].set_xlabel('JST',fontsize=14)
axes[0].legend()
fig.savefig('t2m+q2m_shio.png',dpi=150)
plt.show()
