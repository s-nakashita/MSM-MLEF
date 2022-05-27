#!/usr/bin/env python3.8
import os
import numpy as np
import matplotlib.pyplot as plt
import cfgrib
import xarray as xr
import pandas as pd
import datetime
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
tfile = "apcp_shio.txt"
if os.path.isfile(tfile):
    data = np.loadtxt(tfile)
    hour = data[0,:]
    apcp = data[1,:]
    time = [basetime + datetime.timedelta(hours=h) for h in hour]
else:
    apcp = []
    time = []
    hour = []
    for ft in range(1,34):
        ds = xr.open_dataset(f'r_pgb.f{ft:02d}',engine='cfgrib',\
        backend_kwargs={'filter_by_keys':\
        {'typeOfLevel':'surface',\
         'stepType':'accum'}})
        #print(ds)
        t = ds.time
        dt = ds.step
        t += dt
        jst = pd.to_datetime(t.values) + t_offset
        delta = jst - basetime
        h = delta.total_seconds()/3600.
        hour.append(h)
        data = ds.tp.values.reshape(nlat,nlon)
        da = xr.DataArray(data, coords=[lat,lon],\
             dims=['latitude','longitude'])
        val = da.sel(latitude=lats,longitude=lons,method="nearest").values
        apcp.append(val)
        time.append(jst)
    data = np.stack([hour,apcp])
    np.savetxt(tfile,data)
print(apcp)
print(time)
amedas = np.loadtxt("amedas20220511.txt")
hour_ame = amedas[:,0]
time_ame = [datetime.datetime(2022,5,11,0)+datetime.timedelta(hours=h) \
            for h in hour_ame]
apcp_ame = amedas[:,2]
fig, ax = plt.subplots(figsize=(14,8))
width=datetime.timedelta(minutes=30)
ax.bar(time,apcp,width=width,label='MSM')
ax.plot(time_ame,apcp_ame,color='r',label='AMeDAS')
ax.set_title('1H accumulated precipitation',fontsize=16)
ax.set_ylabel('kg/m^2',fontsize=14)
ax.set_xlabel('JST',fontsize=14)
ax.legend()
fig.savefig('apcp_shio.png',dpi=150)
plt.show()
