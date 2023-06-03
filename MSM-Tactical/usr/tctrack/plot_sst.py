import xarray as xr
import numpy as np 
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from pathlib import Path
from datetime import datetime, timedelta
import sys
plt.rcParams['font.size'] = 18

wdir='rsm2msm9'
#wdir='DATA/gfs/rda'
sdate = datetime(2022,8,29,0)
dt = timedelta(hours=24)
tcnum = 11
ft = 0
if len(sys.argv)>1:
    wdir=sys.argv[1]
if len(sys.argv) > 2:
    yyyymmddhh=sys.argv[2]
    sdate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 3:
    ft = int(sys.argv[3])
if len(sys.argv) > 4:
    tcnum = int(sys.argv[4])
datadir = Path(f'/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/{wdir}')
figdir = datadir / 'fig'
if not figdir.exists():
    figdir.mkdir(parents=True)

lon = np.loadtxt("rlon.txt")
lat = np.loadtxt("rlat.txt")
nlon = lon.size
nlat = lat.size

valid = sdate + timedelta(hours=ft)
## best track
yyyy = sdate.strftime("%Y")
fbst = Path(f'/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/bsttrack/{yyyy}/bst{yyyy[2:]}{tcnum:02d}.txt')
try:
    bsttrack = np.loadtxt(fbst)
except FileNotFoundError:
    print(f"not found {fbst}")

fig = plt.figure(figsize=(8,6),constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
ax.coastlines()
lonw = int(np.min(lon)/5.0+1)*5
lone = int(np.max(lon)/5.0)*5
lats = int(np.min(lat)/5.0+1)*5
latn = int(np.max(lat)/5.0)*5
dlon = dlat = 5
ax.set_xticks(list(range(lonw,lone+dlon,dlon)),crs=ccrs.PlateCarree())
ax.set_yticks(list(range(lats,latn+dlat,dlat)),crs=ccrs.PlateCarree())
ax.set_extent([np.min(lon),np.max(lon),\
    np.min(lat),np.max(lat)], ccrs.PlateCarree())
ax.gridlines()

# read binary file
buf = np.fromfile(f"r_sst.f{ft:02d}.grd",dtype=">f4").reshape(-1,nlat,nlon)
i=0
tdata = buf[i,:,:]
tdata -= 273.15
i+=1
slmsk = buf[i,:,:]
tdata = np.where(slmsk==0.0, tdata, np.nan)
tsfc = xr.DataArray(tdata, coords=[lat,lon],\
        dims=['latitude','longitude'], \
        attrs={'name':'Surface temperature','units':'K'})
ax.set_title(f'SST[K], FT={ft}H V:{valid}')
clevs = np.linspace(25.0,32.0,29)
p = ax.contourf(lon,lat,tsfc,clevs,cmap='coolwarm',\
    transform=ccrs.PlateCarree())
fig.colorbar(p,orientation='horizontal')

# best track
lonbst = bsttrack[:,4]
latbst = bsttrack[:,5]
slpbst = bsttrack[:,6]
lonsel = []
latsel = []
for i in range(bsttrack.shape[0]):
    btime=datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
        int(bsttrack[i,2]),int(bsttrack[i,3]))
    if btime==valid:
        lonsel.append(lonbst[i]);latsel.append(latbst[i])
ax.plot(lonbst,latbst,c='k',lw=2.0,\
    transform=ccrs.PlateCarree(),label='best track')
ax.plot(lonsel,latsel,marker='o',c='k',lw=0.0,\
    transform=ccrs.PlateCarree())
tfile = datadir / sdate.strftime('%Y%m%d%H') / Path(f'track{tcnum:02d}.txt')
try:
        track = np.loadtxt(tfile)
except FileNotFoundError:
        print(f"not found {tfile}")
lonsel = []
latsel = []
for i in range(track.shape[0]):
    ttime=datetime(int(track[i,0]),int(track[i,1]),\
    int(track[i,2]),int(track[i,3]))
    if ttime==valid:
        lonsel.append(track[i,4]);latsel.append(track[i,5])
lonc = track[:,4]
latc = track[:,5]
slpc = track[:,6]
ax.plot(lonc,latc,c='g',lw=2.0,\
        transform=ccrs.PlateCarree(),label=sdate.strftime('%HZ%d%b'))
ax.plot(lonsel,latsel,marker='o',c='g',lw=0.0,\
        transform=ccrs.PlateCarree())
fig.savefig(figdir/f'sst{sdate.strftime("%Y%m%d%H")}+{ft:02d}h.png',dpi=300)
plt.show()