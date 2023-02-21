import numpy as np 
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from pathlib import Path
from datetime import datetime, timedelta
import sys
plt.rcParams['font.size'] = 18

datadir = Path('/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/rsm2msm9')
figdir = datadir / 'fig'
if not figdir.exists():
    figdir.mkdir(parents=True)

sdate = datetime(2022,8,28,0)
edate = datetime(2022,8,31,0)
dt = timedelta(hours=24)
tcnum = 11
if len(sys.argv) > 1:
    yyyymmddhh=sys.argv[1]
    sdate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 2:
    yyyymmddhh=sys.argv[2]
    edate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 3:
    tcnum = int(sys.argv[3])

## best track
yyyy = sdate.strftime("%Y")
fbst = Path(f'/Users/nakashita/mnt/dandelion/data/tctrack/{yyyy}/bst{yyyy[2:]}{tcnum:02d}.txt')
try:
    bsttrack = np.loadtxt(fbst)
except FileNotFoundError:
    print(f"not found {fbst}")

# track
fig = plt.figure(figsize=(8,8),constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
ax.coastlines()
lonw = 120
lone = 155
lats = 15
latn = 40
dlon = dlat = 5
ax.set_xticks(list(range(lonw,lone+dlon,dlon)),crs=ccrs.PlateCarree())
ax.set_yticks(list(range(lats,latn+dlat,dlat)),crs=ccrs.PlateCarree())
ax.set_extent([lonw,lone,lats,latn], ccrs.PlateCarree())
ax.gridlines()

# mslp
fig2, ax2 = plt.subplots(figsize=(10,6),constrained_layout=True)

lonbst = bsttrack[:,4]
latbst = bsttrack[:,5]
slpbst = bsttrack[:,6]
lonsel = []
latsel = []
btime = []
for i in range(bsttrack.shape[0]):
    btime.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
        int(bsttrack[i,2]),int(bsttrack[i,3])))
    if int(bsttrack[i,3])==0:
        lonsel.append(lonbst[i]);latsel.append(latbst[i])
ax.plot(lonbst,latbst,c='k',lw=2.0,\
    transform=ccrs.PlateCarree(),label='best track')
ax.plot(lonsel,latsel,marker='o',c='k',lw=0.0,\
    transform=ccrs.PlateCarree())
ax2.plot(btime,slpbst,c='k',lw=3.0,label='best track')

cmap = plt.get_cmap('tab10')
icol = 0
while sdate <= edate:
    tfile = datadir / sdate.strftime('%Y%m%d%H') / Path(f'track{tcnum:02d}.txt')
    try:
        track = np.loadtxt(tfile)
    except FileNotFoundError:
        print(f"not found {tfile}")
        continue
    time = []
    for i in range(track.shape[0]):
        time.append(datetime(int(track[i,0]),int(track[i,1]),\
        int(track[i,2]),int(track[i,3])))
    lonc = track[:,4]
    latc = track[:,5]
    slpc = track[:,6]
    ax.plot(lonc,latc,c=cmap(icol),lw=2.0,\
        transform=ccrs.PlateCarree(),label=sdate.strftime('%HZ%d%b'))
    ax.plot(lonc[::6],latc[::6],marker='o',c=cmap(icol),lw=0.0,\
        transform=ccrs.PlateCarree())
    ax2.plot(time,slpc,c=cmap(icol),lw=3.0,label=sdate.strftime('%HZ%d%b'))
    icol+=1
    sdate+=dt
ax.legend()
ax2.legend()
plt.setp(ax2.get_xticklabels(),rotation=30,ha="right")
fig.savefig(figdir/f'track{yyyy[2:]}{tcnum:02d}.png',dpi=300)
fig2.savefig(figdir/f'mslp{yyyy[2:]}{tcnum:02d}.png',dpi=300)
plt.show()