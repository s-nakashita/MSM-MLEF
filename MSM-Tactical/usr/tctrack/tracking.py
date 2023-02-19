import numpy as np
from scipy import ndimage
import matplotlib.pyplot as plt
plt.rcParams['font.size'] = 18

rearth = 6.371e3 #[km]
def __distance__(lon,lat,lonc,latc):
    dx = np.deg2rad(lon - lonc)
    y1 = np.deg2rad(lat)
    y0 = np.deg2rad(latc)
    d = np.arccos(np.sin(y0) * np.sin(y1) + np.cos(y0) * np.cos(y1) * np.cos(dx))
    return d

def linint2_points(xi, yi, fi, xo, yo):
    i = np.argmin(np.abs(xi - xo))
    i = np.where(xi[i] <= xo, i, i-1)
    j = np.argmin(np.abs(yi - yo))
    j = np.where(yi[j] <= yo , j, j-1)
    t = (xo - xi[i]) / (xi[i+1] - xi[i])
    u = (yo - yi[j]) / (yi[j+1] - yi[j])
    fo = (1-u) * ((1-t) * fi[j, i] + t * fi[j, i+1]) + \
             u * (t * fi[j+1, i+1] + (1-t) * fi[j+1,i])
    return fo

class tracking:
    def __init__(self,delm,lonm,latm,\
        paramlist,lon0,lat0,\
        ng=10,d0=1.0,dgmin=0.025,debug=False,plot=False):
        
        self.debug = debug
        self.plot = plot
        self.lonm = lonm
        self.latm = latm
        self.lon2d, self.lat2d = np.meshgrid(self.lonm,self.latm)
        if self.debug:
            print(f"lon2d shape={self.lon2d.shape} lat2d shape={self.lat2d.shape}")
            print(f"lon2d={self.lon2d[0]} lat2d={self.lat2d[0]}")
        self.delm = delm # distance of model grid [km]
        self.re = 75.0 # e-folding radius [km]
        if self.delm < 10.0:
            self.re = 60.0
        elif self.delm > 125.0:
            self.re = 150.0
        print(f"model grid={self.delm}km e-folding={self.re}km")
        # non-dimensionalized by rearth
        self.delm = self.delm / rearth
        self.re = self.re / rearth
        
        self.ng = ng #analysis grid points = (2*ng+1) x (2*ng+1)
        self.d0 = d0 #initial analysis grid distance [degree]
        self.dgmin = dgmin #minimum analysis grid distance [degree]
        self.ngi = self.ngj = 2*self.ng + 1
        self.set_ggrid(lon0,lat0,self.d0)
        if self.debug:
            print(f"analysis grid lon={self.long} lat={self.latg}")
        self.centerdict = dict()
        self.cvaldict = dict()
        for param in paramlist:
            self.centerdict[param] = [lon0,lat0]
            self.cvaldict[param] = 0.0
    
    def set_ggrid(self,lon0,lat0,dg):
        self.long = np.zeros(self.ngi)
        self.latg = np.zeros(self.ngj)
        self.long[self.ng] = lon0
        self.latg[self.ng] = lat0
        for i in range(1,self.ng+1):
            self.long[self.ng-i]=self.long[self.ng-i+1]-dg
            self.long[self.ng+i]=self.long[self.ng+i-1]+dg
            self.latg[self.ng-i]=self.latg[self.ng-i+1]-dg
            self.latg[self.ng+i]=self.latg[self.ng+i-1]+dg

    def barnes(self,f,param):
        ggrid = np.zeros((self.ngj,self.ngi))
        if param=="MSLP" or param[:3]=="GPH":
            ggrid[:,:] = 9.99e6
        for j in range(ggrid.shape[0]):
            latc = self.latg[j]
            for i in range(ggrid.shape[1]):
                lonc = self.long[i]
                distall = __distance__(self.lon2d,self.lat2d,lonc,latc)
                if self.debug: print(f"distall={distall.shape} {np.min(distall)} {np.max(distall)}")
                dist = distall[distall<2.0*self.re].copy()
                if dist.size == 0: continue
                if self.debug: print(f"dist={dist.shape} {np.min(dist)} {np.max(dist)}")
                wgt = np.exp(-dist*dist/self.re/self.re)
                if self.debug: print(f"wgt={np.min(wgt)} {np.max(wgt)}")
                ggrid[j,i] = np.sum(f[distall<2.0*self.re]*wgt)/np.sum(wgt)
        if param=="MSLP" or param[:3]=="GPH":
            gg = ggrid
        else:
            gg = -1.0*ggrid
        loc_min = np.where( \
            ndimage.minimum_filter( \
            gg, size=(3,3), mode=('nearest', 'wrap')) == gg)
        g_min = gg[loc_min[0],loc_min[1]]
        if self.debug: print(g_min)
        ncand = np.argsort(g_min)
        imin = loc_min[1][ncand[0]]
        jmin = loc_min[0][ncand[0]]
        self.centerdict[param] = [self.long[imin],self.latg[jmin]]
        f0 = linint2_points(self.lonm, self.latm, f, self.long[imin],self.latg[jmin])
        self.cvaldict[param] = f0
    
    def tcycle(self,fdict,paramlist):
        for param in paramlist:
            f = fdict[param]
            if self.plot:
                fig, ax = plt.subplots(figsize=[8,8])
                c = ax.contourf(self.lonm, self.latm, f, levels=np.linspace(-0.07, 0.0, 8))
            dg = self.d0
            icyc=0
            while dg > self.dgmin:
                self.barnes(f,param)
                icyc+=1
                dg *= 0.5
                lon0, lat0 = self.centerdict[param]
                f0 = self.cvaldict[param]
                print(f"cycle={icyc} center=({lon0},{lat0}) val={f0}")
                self.set_ggrid(lon0, lat0, dg)
                if self.plot: ax.plot(lon0,lat0,marker='^')
        if self.plot:
            ax.set_aspect("equal")
            fig.colorbar(c, ax=ax)
            #fig.savefig("tcycletest_barnes.png", bbox_inches="tight", dpi=300)
            plt.show()

if __name__=="__main__":
    paramlist=["MSLP"]
    fdict = dict()
    n = 61 
    d = 1.0 / (n - 1)
    x = np.linspace(0, 1, n)
    y = np.linspace(0, 1, n)
    X, Y = np.meshgrid(x, y, indexing="ij")

    slp = (X**2 - X**4) * (Y**4 - Y**2)
    print(f"slp: min={slp.min()} max={slp.max()}")
    fdict["MSLP"] = slp
    lon = np.linspace(120, 150, n)
    lat = np.linspace(0, 30, n)
    print(f"lon={lon} lat={lat}")
    delm = __distance__(lon[0], lat[0], lon[0], lat[1])
    delm = delm*rearth
    lon0, lat0 = np.mean(lon), np.mean(lat)
    print(f"guess=({lon0},{lat0})")
    track = tracking(delm, lon, lat, paramlist, lon0, lat0, debug=True, plot=True)

    track.tcycle(fdict,paramlist)
