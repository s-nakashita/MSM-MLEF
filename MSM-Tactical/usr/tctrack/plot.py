import numpy as np 
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from pathlib import Path
from datetime import datetime, timedelta
import sys
plt.rcParams['font.size'] = 18

wdir='rsm2msm9_gfsz'
#wdir='rsm2msm9_tparc'
#wdir='DATA/gfs/rda'
datadir = Path(f'/Volumes/dandelion/GRMSMJob/{wdir}')
figdir = datadir / 'fig'
if not figdir.exists():
    figdir.mkdir(parents=True)

#sdate = datetime(2022,7,1,0)
#edate = datetime(2022,7,4,12)
#sdate = datetime(2022,8,28,0)
#edate = datetime(2022,8,31,0)
#sdate = datetime(2022,9,12,12)
#edate = datetime(2022,9,17,12)
sdate = datetime(2023,5,22,0)
edate = datetime(2023,5,31,0)
dt = timedelta(hours=24)
tcnum = 2
if len(sys.argv) > 1:
    yyyymmddhh=sys.argv[1]
    sdate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 2:
    yyyymmddhh=sys.argv[2]
    edate = datetime.strptime(yyyymmddhh,"%Y%m%d%H")
if len(sys.argv) > 3:
    tcnum = int(sys.argv[3])

## best track
lbst = True
yyyy = sdate.strftime("%Y")
fbst = Path(f'/Volumes/dandelion/data/bsttrack/{yyyy}/bst{yyyy[2:]}{tcnum:02d}.txt')
try:
    bsttrack = np.loadtxt(fbst)
except FileNotFoundError:
    print(f"not found {fbst}")
    frpd = Path(f'/Volumes/dandelion/data/bsttrack/{yyyy}/rpd{yyyy[2:]}{tcnum:02d}.txt')
    try:
        bsttrack = np.loadtxt(frpd)
    except FileNotFoundError:
        lbst=False
        print(f"not found {frpd}")

# track
fig = plt.figure(figsize=(8,8),constrained_layout=True)
ax = fig.add_subplot(111,projection=ccrs.PlateCarree())
ax.coastlines()
lonw = 120
lone = 155
lats = 5
latn = 40
dlon = dlat = 5
ax.set_xticks(list(range(lonw,lone+dlon,dlon)),crs=ccrs.PlateCarree())
ax.set_yticks(list(range(lats,latn+dlat,dlat)),crs=ccrs.PlateCarree())
ax.set_extent([lonw,lone,lats,latn], ccrs.PlateCarree())
ax.gridlines()

# mslp
fig2, ax2 = plt.subplots(figsize=(10,6),constrained_layout=True)

if lbst:
    lonbst = bsttrack[:,4]
    latbst = bsttrack[:,5]
    slpbst = bsttrack[:,6]
    lonsel = []
    latsel = []
    btime = []
    xticks = []
    texts = []
    for i in range(bsttrack.shape[0]):
        btime.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
            int(bsttrack[i,2]),int(bsttrack[i,3])))
        if int(bsttrack[i,3])==0:
            lonsel.append(lonbst[i]);latsel.append(latbst[i])
            xticks.append(datetime(int(bsttrack[i,0]),int(bsttrack[i,1]),\
            int(bsttrack[i,2]),int(bsttrack[i,3])))
            texts.append(f"{int(bsttrack[i,2])}")
    ax.plot(lonbst,latbst,c='k',lw=2.0,\
        transform=ccrs.PlateCarree(),label='best track')
    ax.plot(lonsel,latsel,marker='o',lw=0.0,\
        markerfacecolor='w',markeredgecolor='k',markersize=12.0,
        transform=ccrs.PlateCarree())
    for lon1,lat1,text in zip(lonsel,latsel,texts):
        if lon1 < lonw or lon1 > lone or lat1 < lats or lat1 > latn: continue
        ax.text(lon1,lat1,text,{'ha':'center','va':'center','c':'k','size':8},\
            transform=ccrs.PlateCarree())
    ax2.plot(btime,slpbst,c='k',lw=3.0,label='best track')
    ax2.set_xticks(xticks)

cmap = plt.get_cmap('tab10')
icol = 0
style = 'solid'
while sdate <= edate:
    #for wdir, style in zip(['rsm2msm9_gfsz'],\
    #    ['solid','dashed']):
    #for wdir, style in zip(['DATA/gfs/rda'],['solid','dashed']):
    datadir = Path(f'/Volumes/dandelion/GRMSMJob/{wdir}')
    tfile = datadir / sdate.strftime('%Y%m%d%H') / Path(f'track{tcnum:02d}.txt')
    try:
            track = np.loadtxt(tfile)
    except FileNotFoundError:
            print(f"not found {tfile}")
            sdate+=dt
            continue
    time = []
    lonsel = []
    latsel = []
    texts = []
    for i in range(track.shape[0]):
        time.append(datetime(int(track[i,0]),int(track[i,1]),\
            int(track[i,2]),int(track[i,3])))
        if int(track[i,3])==0:
            lonsel.append(track[i,4]);latsel.append(track[i,5])
            texts.append(f"{int(track[i,2])}")
    lonc = track[:,4]
    latc = track[:,5]
    slpc = track[:,6]
    if style=='solid':
            ax.plot(lonc,latc,c=cmap(icol),lw=2.0,ls=style,\
            transform=ccrs.PlateCarree(),label=sdate.strftime('%HZ%d%b'))
            ax.plot(lonsel,latsel,marker='o',lw=0.0,\
            markerfacecolor=cmap(icol),markeredgecolor='k',markersize=12.0,
            transform=ccrs.PlateCarree())
            ax2.plot(time,slpc,c=cmap(icol),lw=3.0,ls=style,\
            label=sdate.strftime('%HZ%d%b'))
    else:
            ax.plot(lonc,latc,c=cmap(icol),lw=2.0,ls=style,\
            transform=ccrs.PlateCarree())
            ax.plot(lonsel,latsel,marker='o',c=cmap(icol),lw=0.0,\
            transform=ccrs.PlateCarree())
            ax2.plot(time,slpc,c=cmap(icol),lw=3.0,ls=style)
    for lon1,lat1,text in zip(lonsel,latsel,texts):
        if lon1 < lonw or lon1 > lone or lat1 < lats or lat1 > latn: continue
        ax.text(lon1,lat1,text,{'ha':'center','va':'center','c':'k','size':8},\
        transform=ccrs.PlateCarree())
    icol+=1
    sdate+=dt
ax2.grid()
ax.legend()
ax2.legend()
plt.setp(ax2.get_xticklabels(),rotation=30,ha="right")
fig.savefig(figdir/f'track{yyyy[2:]}{tcnum:02d}.png',dpi=300)
fig2.savefig(figdir/f'mslp{yyyy[2:]}{tcnum:02d}.png',dpi=300)
plt.show()
