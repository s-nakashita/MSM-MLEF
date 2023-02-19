import xarray as xr
import pandas as pd
from tracking import tracking
import sys
from pathlib import Path
from datetime import datetime, timedelta
import numpy as np

dt = timedelta(hours = 12)
dc = 5
pmax = 102000
#latmax = 45
latmax = 38
sigma = 0.0

wdir="rsm2msm9"
nlon=385;nlat=325
inchour=1
endhour=48
delm = 9.0
lon = np.loadtxt("rlon.txt")
lat = np.loadtxt("rlat.txt")

plot=True
if plot:
    import matplotlib.pyplot as plt
    from metpy.units import units
    import metpy.calc as mpcalc
    import cartopy.crs as ccrs
    plt.rcParams['font.size'] = 16

datadir=Path(f"/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/{wdir}")
trackdir=datadir
t0 = datetime(2022, 9, 17, 12)
t0max = datetime(2022, 9, 17, 12)
tcnum = 14
if len(sys.argv) > 1:
    yyyymmddhh=sys.argv[1]
    t0 = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
    t0max = t0
if len(sys.argv) > 2:
    tcnum = int(sys.argv[2])
### 2214 : Nanmador
tstart = datetime(2022, 9, 14, 3)
fguess = {
          "2022091403":[140.1,22.4,99600.0],
          "2022091412":[140.6,22.9,99600.0],
          "2022091500":[140.2,23.2,99000.0],
          "2022091512":[138.5,23.3,98000.0],
          "2022091600":[136.6,23.4,97000.0],
          "2022091612":[135.7,23.9,95000.0],
          "2022091700":[134.2,25.2,92500.0],
          "2022091712":[132.8,26.4,91000.0]}
### besttrack
yyyy = t0.strftime("%Y")
yy = yyyy[2:]
lbst=False
fbst=f"/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/bsttrack/{yyyy}/bst{yy}{tcnum:02d}.txt"
try:
    bsttrack = np.loadtxt(fbst)
except FileNotFoundError:
    print(f"not found {fbst}")
else:
    lbst = True
    nbst = bsttrack.shape[0]
    tstart = datetime(int(bsttrack[0,0]),int(bsttrack[0,1]),
    int(bsttrack[0,2]),int(bsttrack[0,3]))

while t0 <= t0max:
    init = t0.strftime("%Y%m%d%H") # -> 2019100600
    sdate = init
    print(init)
    outdir=trackdir/init
    if not outdir.exists():
        outdir.mkdir(parents=True)
    outfile = f"track{tcnum:02d}.txt"
    track = open(outdir/outfile, "w")
    #ddir = datadir/init
    lonpre = None 
    latpre = None
    slppre = None
    preexist = False
    tdif = tstart - t0
    if tdif.total_seconds() > 0:
        f0 = int(tdif.total_seconds()/3600)
        ts = t0 + timedelta(hours=f0)
        sdate = ts.strftime("%Y%m%d%H")
    else:
        f0 = 0
        ts = t0
    if lbst:
        ddhh = ts.strftime("%d%H") 
        for i in range(nbst):
            print( f"{int(bsttrack[i,2]):02d}"+f"{int(bsttrack[i,3]):02d}", ddhh )
            if f"{int(bsttrack[i,2]):02d}"+f"{int(bsttrack[i,3]):02d}"==ddhh:
                break
        lon0=bsttrack[i,4]; lat0=bsttrack[i,5]; slp0=bsttrack[i,6]
    else:
        lon0, lat0, slp0 = fguess[sdate]
    print(f"f0={f0} lon0={lon0} lat0={lat0} slp0={slp0}")
    lonpre = lon0
    latpre = lat0
    slppre = slp0
    ### initialize
    paramlist = ['MSLP','RV850']
    for ft in range(f0,endhour+inchour,inchour):
        fdict = dict()
        # read binary file
        buf = np.fromfile(f"r_pgb.f{ft:02d}.grd",dtype=">f4").reshape(3,nlat,nlon)
        slpdata = buf[0,:,:]
        #print(f"latitude:{lat}");print(f"longitude:{lon}")#;exit()
        slp = xr.DataArray(slpdata, coords=[lat,lon],\
         dims=['latitude','longitude'], \
         attrs={'name':'Mean Sea Level Pressure','units':'hPa'})
        fdict['MSLP'] = slp.values
        udata = buf[1,:,:]
        vdata = buf[2,:,:]
        u850 = xr.DataArray(udata,coords=[lat,lon],
            dims=['latitude','longitude'], \
            attrs={'name':'Zonal Wind','units':'m/s'})
        v850 = xr.DataArray(vdata,coords=[lat,lon],
            dims=['latitude','longitude'], \
            attrs={'name':'Meridional Wind','units':'m/s'})
        u850=u850*units('m/s')
        v850=v850*units('m/s')
        #print(u);print(v)
        # calcurate vorticity
        dx,dy = mpcalc.lat_lon_grid_deltas(lon,lat)
        #print(dx,dy)
        rv850 = mpcalc.vorticity(u850,v850,dx=dx,dy=dy)
        fdict['RV850'] = rv850.values
        t = t0 + timedelta(hours=ft) 
        print(t)
        if lonpre is not None and latpre is not None and slppre is not None:
            print(f"previous center {lonpre:.3f}, {latpre:.3f}, {slppre:.5f}")
        print(f"first guess center {lon0:.3f}, {lat0:.3f}, {slp0:.5f}")
        tctrack = tracking(delm,lon,lat,\
            paramlist,lon0,lat0,\
            d0=0.5,\
            debug=True)
        tctrack.tcycle(fdict,paramlist)
        lonmin=0.0; latmin=0.0
        for param in paramlist:
            lonc,latc=tctrack.centerdict[param]
            lonmin+=lonc
            latmin+=latc
            if param=='MSLP':
                slpmin = tctrack.cvaldict[param]
        lonmin/=float(len(paramlist))
        latmin/=float(len(paramlist))
        if lonmin is None and latmin is None and slpmin is None:
            break
        print(f"estimated center {lonmin:.3f}, {latmin:.3f}, {slpmin:.3f}")
        if latmin > latmax :
            print(f"latitude exceeds {latc}")
            break
        if slpmin > pmax :
            print(f"mslp exceeds {pmax}")
            continue
        lon0 = lonmin
        lat0 = latmin
        slp0 = slpmin
        lonpre = lonmin
        latpre = latmin
        slppre = slpmin
        print("{} {} {} {} {} {} {}".format(t.year, t.month, t.day, t.hour, lonmin, latmin, slpmin), file = track)
        if plot and ft%3==0:
            slp=slp*1e-2
            rv850=rv850*1e4
            fig = plt.figure(figsize=(8,8),constrained_layout=True)
            ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
            ax.coastlines()
            dlon=5
            lonw=int(np.ceil(np.min(lon)));lone=int(np.floor(np.max(lon)))
            #lonw=int(lon0-dlon);lone=int(lon0+dlon)
            dlat=5
            lats=int(np.ceil(np.min(lat)));latn=int(np.floor(np.max(lat)))
            #lats=int(lat0-dlat);latn=int(lat0+dlat)
            ax.set_xticks(list(range(lonw,lone+dlon,dlon)),crs=ccrs.PlateCarree())
            ax.set_yticks(list(range(lats,latn+dlat,dlat)),crs=ccrs.PlateCarree())
            ax.set_extent([np.min(lon),np.max(lon),np.min(lat),np.max(lat)], \
                ccrs.PlateCarree())
            ax.gridlines()
            ax.set_title(f'SLP[hPa] + Vor850'+r'[$10^{-4}$/s]'+f', FT={ft}H V:{t}')
            clevs = np.arange(-20,20,1)
            p = ax.contourf(lon,lat,rv850,clevs,cmap='coolwarm',\
                transform=ccrs.PlateCarree())
            fig.colorbar(p,orientation='horizontal')
            clevs = [960.0,980.0,990.0,1000.0,1004.0,1008.0,1012.0]
            p2 = ax.contour(lon,lat,slp,clevs,colors=['k'],\
                transform=ccrs.PlateCarree())
            ax.clabel(p2,manual=False,inline=False)
            for param,mark in zip(paramlist,['s','o']):
                lonc, latc = tctrack.centerdict[param]
                ax.scatter([lonc],[latc],edgecolors='b',marker=mark,\
                facecolors='None',
                transform=ccrs.PlateCarree(),
                label=f"{param} estimated center")
            ax.scatter([lon0],[lat0],edgecolors='g',marker='^',\
                facecolors='None',
                transform=ccrs.PlateCarree(),
                label="mean estimated center")
            ax.legend()
            fig.savefig(outdir/f'tc{tcnum:02d}_ft{ft:03d}.png')
            #plt.show()
            plt.close()
    t0 += dt



