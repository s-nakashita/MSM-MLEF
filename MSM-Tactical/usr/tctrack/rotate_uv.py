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

var_sfc = ['u10','v10']
var_pls = ['u','v']
units = {'u10':'m/s','v10':'m/s','u':'m/s','v':'m/s'}
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
        inpf = f"r_pgb_uv.f{fh:02d}.grd"
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
        #sfc
        utmp = buf[i,:,:]
        i+=1
        vtmp = buf[i,:,:]
        i+=1
        ### create DataArray
        da_u = xr.DataArray(utmp,coords=[lat,lon],\
                dims=['lat','lon'],\
                attrs={'units':units['u']})
        da_v = xr.DataArray(vtmp,coords=[lat,lon],\
                dims=['lat','lon'],\
                attrs={'units':units['v']})
        ### Step 2.4.1 : convert into Cartesian
        lon2d = lon[np.newaxis,:]
        lat2d = lat[:,np.newaxis]
        xd,yd,zd = librotate.uv2xyzd(utmp,vtmp,lon2d,lat2d)
        ### Step 2.4.2 : interpolation
        da_xd = xr.DataArray(xd.reshape(1,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
            ('lat',lat),('lon',lon)],\
                name='xd')
        da_yd = xr.DataArray(yd.reshape(1,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
             ('lat',lat),('lon',lon)],\
                name='yd')
        da_zd = xr.DataArray(zd.reshape(1,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
            ('lat',lat),('lon',lon)],\
                name='zd')
        xd_intp = da_xd.interp(lon=newlon,lat=newlat)
        yd_intp = da_yd.interp(lon=newlon,lat=newlat)
        zd_intp = da_zd.interp(lon=newlon,lat=newlat)
        ### Step 2.4.3 : rotate in Cartesian coordinates
        xd_np, yd_np, zd_np = \
            librotate.tc2np(lonc,latc,\
                xd_intp.values.reshape(1,nolat,nolon),
                yd_intp.values.reshape(1,nolat,nolon),
                zd_intp.values.reshape(1,nolat,nolon))
        ### Step 2.4.4 : convert back to spherical
        lonnp = lonin[np.newaxis,np.newaxis,:]
        latnp = latin[np.newaxis,:,np.newaxis]
        unp, vnp = librotate.xyzd2uv(xd_np,yd_np,zd_np,lonnp,latnp)
        ### output
        da_uout = xr.DataArray(unp,\
                [('time',pd.date_range(date,periods=1)),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da_u.attrs,name='u10')
        da_vout = xr.DataArray(vnp,\
                [('time',pd.date_range(date,periods=1)),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da_v.attrs,name='v10')
        print(da_uout)
        print(da_vout)
        daoutlist.append(da_uout)
        daoutlist.append(da_vout)
        #plev
        utmp = buf[i:i+nlev,:,:]
        i+=nlev
        vtmp = buf[i:i+nlev,:,:]
        i+=nlev
        ### create DataArray
        da_u = xr.DataArray(utmp,coords=[lev,lat,lon],\
                dims=['level','lat','lon'],\
                attrs={'units':units['u']})
        da_v = xr.DataArray(vtmp,coords=[lev,lat,lon],\
                dims=['level','lat','lon'],\
                attrs={'units':units['v']})
        ### Step 2.4.1 : convert into Cartesian
        lon3d = lon[np.newaxis,np.newaxis,:]
        lat3d = lat[np.newaxis,:,np.newaxis]
        xd,yd,zd = librotate.uv2xyzd(utmp,vtmp,lon3d,lat3d)
        ### Step 2.4.2 : interpolation
        da_xd = xr.DataArray(xd.reshape(1,nlev,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
            ('lev',lev),\
            ('lat',lat),('lon',lon)],\
                name='xd')
        da_yd = xr.DataArray(yd.reshape(1,nlev,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
            ('lev',lev),\
            ('lat',lat),('lon',lon)],\
                name='yd')
        da_zd = xr.DataArray(zd.reshape(1,nlev,nlat,nlon),\
            [('time',pd.date_range(date,periods=1)),\
            ('lev',lev),\
            ('lat',lat),('lon',lon)],\
                name='zd')
        xd_intp = da_xd.interp(lon=newlon,lat=newlat)
        yd_intp = da_yd.interp(lon=newlon,lat=newlat)
        zd_intp = da_zd.interp(lon=newlon,lat=newlat)
        ### Step 2.4.3 : rotate in Cartesian coordinates
        xd_np, yd_np, zd_np = \
            librotate.tc2np(lonc,latc,\
                xd_intp.values.reshape(1,nlev,nolat,nolon),
                yd_intp.values.reshape(1,nlev,nolat,nolon),
                zd_intp.values.reshape(1,nlev,nolat,nolon))
        ### Step 2.4.4 : convert back to spherical
        lonnp = lonin[np.newaxis,np.newaxis,np.newaxis,:]
        latnp = latin[np.newaxis,np.newaxis,:,np.newaxis]
        unp, vnp = librotate.xyzd2uv(xd_np,yd_np,zd_np,lonnp,latnp)
        ### output
        da_uout = xr.DataArray(unp,\
                [('time',pd.date_range(date,periods=1)),\
                ('level',lev),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da_u.attrs,name='u')
        da_vout = xr.DataArray(vnp,\
                [('time',pd.date_range(date,periods=1)),\
                ('level',lev),\
                ('latitude',latin),('longitude',lonin)],\
                attrs=da_v.attrs,name='v')
        print(da_uout)
        print(da_vout)
        daoutlist.append(da_uout)
        daoutlist.append(da_vout)
        dalist.append(xr.merge(daoutlist))
ds = xr.merge(dalist)
print(ds)
### output netCDF
ds.to_netcdf("np_uv.nc","w")