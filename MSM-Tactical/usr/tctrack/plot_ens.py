import numpy as np 
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from pathlib import Path
from datetime import datetime, timedelta
import sys
plt.rcParams['font.size'] = 18

wdir='rsm2msm9_da'
bgmtype='dry'
bp='wbp'
header=f'bv{bgmtype}{bp}'
member=20
psub=True
if psub:
    member*=2
dt = timedelta(hours=24)
sdate = datetime(2022,8,30,0)
edate = datetime(2022,8,30,0)
tcnum = 11
if len(sys.argv) > 1:
    yyyymmddhh=sys.argv[1]
    sdate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 2:
    yyyymmddhh=sys.argv[2]
    edate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 3:
    tcnum = int(sys.argv[3])

#datadir = Path(f'/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/{wdir}/{sdate.strftime("%Y%m%d%H")}')
datadir = Path(f'/Volumes/G-DRIVE/{wdir}/{sdate.strftime("%Y%m%d%H")}')
figdir = datadir / 'fig'
if not figdir.exists():
    figdir.mkdir(parents=True)

## best track
yyyy = sdate.strftime("%Y")
fbst = Path(f'/Users/nakashita/Development/grmsm/MSM-Tactical/usr/work/bsttrack/{yyyy}/bst{yyyy[2:]}{tcnum:02d}.txt')
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
xticks = []
xticks_minor = []
for i in range(bsttrack.shape[0]):
    btime.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
        int(bsttrack[i,2]),int(bsttrack[i,3])))
    if int(bsttrack[i,3])==0:
        lonsel.append(lonbst[i]);latsel.append(latbst[i])
        xticks.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
        int(bsttrack[i,2]),int(bsttrack[i,3])))
    if int(bsttrack[i,3])%6==0:
        xticks_minor.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
        int(bsttrack[i,2]),int(bsttrack[i,3])))
ax.plot(lonbst,latbst,c='k',lw=2.0,\
    transform=ccrs.PlateCarree(),label='best track')
ax.plot(lonsel,latsel,marker='o',c='k',lw=0.0,\
    transform=ccrs.PlateCarree())
ax2.plot(btime,slpbst,c='k',lw=3.0,label='best track')
ax2.set_xticks(xticks)
ax2.set_xticks(xticks_minor,minor=True)

cmap = plt.get_cmap('viridis')
icolint = 6
#while sdate <= edate:
#control & mean
tfiles = [ datadir / Path(f'track{tcnum:02d}.txt'),\
        datadir / f'{header}mean' / Path(f'track{tcnum:02d}.txt')]
for tfile, col, label in zip(tfiles,['red','blue'],['cntl','mean']):
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
        lw=3.0
        ax.plot(lonc,latc,c=col,lw=lw,\
        transform=ccrs.PlateCarree(),label=label)
        ax.plot(lonc[::6],latc[::6],marker='o',c=col,lw=0.0,\
        transform=ccrs.PlateCarree())
        ax2.plot(time,slpc,c=col,lw=lw,\
        label=label)
icol = 0
for m in range(1,member+1):
        if m%2==1:
            mm = int( (m+1) / 2 )
            mem = f'{header}{mm:03d}'
        else:
            mm = int( m / 2 )
            mem = f'{header}m{mm:03d}'
        tfile = datadir / mem /Path(f'track{tcnum:02d}.txt')
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
        ax.plot(lonc,latc,c=cmap(icol),lw=1.0,alpha=0.6,\
            transform=ccrs.PlateCarree())#,label=sdate.strftime('%HZ%d%b'))
        #ax.plot(lonc[::6],latc[::6],marker='o',c=cmap(icol),lw=0.0,\
        #    transform=ccrs.PlateCarree())
        ax2.plot(time,slpc,c=cmap(icol),lw=1.0,alpha=0.6)#,\
            #label=sdate.strftime('%HZ%d%b'))
        icol+=icolint
#    sdate+=dt
ax2.set_xlim(time[0]-timedelta(hours=3),time[-1]+timedelta(hours=3))
ax2.grid()
ax2.grid(which='minor')
ax.legend()
ax2.legend()
plt.setp(ax2.get_xticklabels(),rotation=30,ha="right")
fig.savefig(figdir/f'track{yyyy[2:]}{tcnum:02d}.png',dpi=300)
fig2.savefig(figdir/f'mslp{yyyy[2:]}{tcnum:02d}.png',dpi=300)
plt.show()
