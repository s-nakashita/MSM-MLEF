import sys
import numpy as np
from pathlib import Path
from datetime import datetime, timedelta
import xarray as xr
import pandas as pd
import librotate

## essential
print(sys.argv)
if len(sys.argv) < 5:
    print(f"Usage : {sys.argv[0]} initial-date trackfile nolon nolat [latmax]")
init = sys.argv[1] 
tfile = sys.argv[2]
nolon = int(sys.argv[3]) #output resolution
nolat = int(sys.argv[4]) # //
## optional
latmax = 15.0
if len(sys.argv)>5:
    latmax = float(sys.argv[5])
dlat = latmax/(nolat-1)
print("{} {} {} {} {:.1f}".format(init,tfile,nolon,nolat,latmax))

idate=datetime.strptime(init,'%Y%m%d%H')
print(idate)

## input coordinates
lon = np.loadtxt("rlon.txt")
lat = np.loadtxt("rlat.txt")
lev = np.loadtxt("rlev.txt")
nlon = lon.size; nlat = lat.size; nlev = lev.size
print(f"lon {nlon} {lon[:5]}")
print(f"lat {nlat} {lat[:5]}")
print(f"lev {nlev} {lev[:]}")

var_sfc = ['msl']
var_pls = ['t','rh']
units = {'msl':'Pa','t':'K','rh':'%'}
nlevall = len(var_sfc) + len(var_pls)*nlev

## Step 1: generate polar coordinates
lonin, latin = librotate.generate_points(nolon,nolat,dlat)
print(lonin)
print(latin)

## Step 2: rotate input data
dalist = []
with Path(tfile).open() as track:
    for l in track:
## Step 2.1: reading track
        data = l.split()
        year = data[0]; month = data[1]; day = data[2]; hour=data[3]
        date = datetime.strptime(f'{year}/{month}/{day} {hour}:00',\
            '%Y/%m/%d %H:00')
        ft = date - idate
        fh = int(ft.total_seconds() / 3600)
        print(f"init:{idate} valid:{date} fh:{fh}")
        lonc = float(data[4]); latc = float(data[5])
        print(f"center=({lonc:.2f},{latc:.2f})")

## Step 2.2: rotate polar coordinates to TC center
        lonout, latout = librotate.rotate_lonlat(lonc,latc,lonin,latin)
        lonmax = lonout.max(); lonmin = lonout.min()
        latmax = latout.max(); latmin = latout.min()
        if lonmax > lon.max() or lonmin < lon.min() \
            or latmax > lat.max() or latmin < lat.min():
            print(f"out of region. \n"+\
                f"input lon: {lon.min():.2f}<lon<{lon.max():.2f}\n"+\
                f"output lon: {lonmin:.2f}<lon<{lonmax:.2f}\n"+
                f"input lat: {lat.min():.2f}<lat<{lat.max():.2f}\n"+\
                f"output lat: {latmin:.2f}<lat<{latmax:.2f}")
            break
        print(f"lonout : max={lonout.max():.2f} min={lonout.min():.2f}")
        print(f"latout : max={latout.max():.2f} min={latout.min():.2f}")

## Step 2.3: reading input data
        inpf = f"r_pgb_scl.f{fh:02d}.grd"
        try:
            buf = np.fromfile(inpf,dtype=">f4").reshape(nlevall,nlat,nlon)
        except FileNotFoundError:
            print(f"not found {inpf}")
            continue
        
        newlon = xr.DataArray(lonout,dims='np_lonlat')
        newlat = xr.DataArray(latout,dims='np_lonlat')
## Step 2.4: interpolate into new coordinates
        daoutlist = []
        i=0
        for vname in var_sfc:
            tmp = buf[i,:,:]
            ### create DataArray
            da = xr.DataArray(tmp,coords=[lat,lon],
            dims=['lat','lon'],\
            attrs={'units':units[vname]})
            ### interpolation
            da_intp = da.interp(lon=newlon,lat=newlat)
            ### output
            da_out = xr.DataArray(da_intp.values.reshape(1,nolat,nolon),\
                [('time',pd.date_range(date,periods=1)),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da.attrs,name=vname)
            print(da_out)
            daoutlist.append(da_out)
            i+=1

        for vname in var_pls:
            tmp = buf[i:i+nlev,:,:]
            ### create DataArray
            da = xr.DataArray(tmp,coords=[lev,lat,lon],\
                dims=['level','lat','lon'],\
                attrs={'units':units[vname]})
            ### interpolation
            da_intp = da.interp(lon=newlon,lat=newlat)
            ### output
            da_out = xr.DataArray(da_intp.values.reshape(1,nlev,nolat,nolon),\
                [('time',pd.date_range(date,periods=1)),\
                ('level',lev),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da.attrs,name=vname)
            print(da_out)
            daoutlist.append(da_out)
            i+=nlev
        dalist.append(xr.merge(daoutlist))
ds = xr.merge(dalist)
print(ds)
### output netCDF
ds.to_netcdf("np_scl.nc","w")