import numpy as np
from scipy import ndimage
import librotate
import xarray as xr
import matplotlib.pyplot as plt
plt.rcParams['font.size'] = 18

global rearth
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
        lonpre=None,latpre=None,dt=1,\
        ng=10,d0=1.0,dgmin=0.025,distmax=200.0,\
        debug=False,plot=False):
        
        self.debug = debug
        self.plot = plot
        ## model grid
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
        ### non-dimensionalized by rearth
        self.delm = self.delm / rearth
        self.re = self.re / rearth
        
        ## analysis grid
        self.lon0 = lon0 #first guess center longitude
        self.lat0 = lat0 #first guess center latitude
        self.lonpre = lonpre #previous center longitude
        self.latpre = latpre #previous center latitude
        self.dt = dt #tracking interval [hour]
        self.ng = ng #analysis grid points = (2*ng+1) x (2*ng+1)
        self.d0 = d0 #initial analysis grid distance [degree]
        self.dgmin = dgmin #minimum analysis grid distance [degree]
        self.ngi = self.ngj = 2*self.ng + 1
        self.set_ggrid(self.lon0,self.lat0,self.d0)
        if self.debug:
            print(f"analysis grid lon={self.long} lat={self.latg}")
        ### for circulation
        self.ri = np.array([35.0+i*30.0 for i in range(7)])
        self.dtheta = 15.0
        self.di = self.ri * np.deg2rad(self.dtheta)
        self.latcirc = np.arcsin( np.cos(self.ri / rearth) )
        self.latcirc = np.rad2deg(self.latcirc)
        self.loncirc = np.arange(24) * self.dtheta
        if self.debug:
            print(f"di={self.di}")
            print(f"latcirc={self.latcirc}")
            print(f"loncirc={self.loncirc}")

        ## QC thresholds
        self.distmax = distmax #maximum distance between previous and current center [km]
        self.pgradmin = 0.5 / 333.0 #minimum MSLP gradients [mb/km]
        self.tanwrad = 275.0 #radius for evaluation region of tangential wind speed [km]
        self.tanwmin = 1.5 #minimum tangential wind speed within tanwrad circle [m/s]
        self.distp2vmax = 425.0 #maximum distance between mslp center and rv850 center [km]
        self.tvelmax = 60.0 * 0.5144 #maximum translation speed [m/s]
        self.distp2cmax = self.tvelmax * self.dt * 3.6 #[km]
        
        self.centerdict = dict()
        self.cvaldict = dict()
        for param in paramlist:
            self.centerdict[param] = [self.lon0,self.lat0]
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
        return gg

    def circulation(self,fdict,param):
        ggrid = np.zeros((self.ngj,self.ngi))
        if param=='WC10':
            u=fdict['U10']
            v=fdict['V10']
        elif param=='WC850':
            u=fdict['U850']
            v=fdict['V850']
        else:
            u=fdict['U700']
            v=fdict['V700']
        for j in range(ggrid.shape[0]):
            latc = self.latg[j]
            for i in range(ggrid.shape[1]):
                lonc = self.long[i]
                unp,vnp = self.rotate_uv(lonc, latc, self.loncirc, self.latcirc, u, v)
                ggrid[j,i] += np.sum(unp+self.di[:,None])
        return -1.0*ggrid

    def rotate_uv(self,lonc,latc,lonin,latin,u,v):
        lonout, latout = \
            librotate.rotate_lonlat(lonc,latc,\
                lonin,latin)
        nolat = latin.size
        nolon = lonin.size
        newlon = xr.DataArray(lonout,dims='np_lonlat')
        newlat = xr.DataArray(latout,dims='np_lonlat')
        ### Step 1 : convert into Cartesian
        nlat = self.latm.size
        nlon = self.lonm.size
        lon2d = self.lonm[np.newaxis,:]
        lat2d = self.latm[:,np.newaxis]
        xd,yd,zd = librotate.uv2xyzd(u,v,lon2d,lat2d)
        ### Step 2 : interpolation
        da_xd = xr.DataArray(xd.reshape(nlat,nlon),\
            [('lat',self.latm),('lon',self.lonm)],\
                name='xd')
        da_yd = xr.DataArray(yd.reshape(nlat,nlon),\
            [('lat',self.latm),('lon',self.lonm)],\
                name='yd')
        da_zd = xr.DataArray(zd.reshape(nlat,nlon),\
            [('lat',self.latm),('lon',self.lonm)],\
                name='zd')
        #print(da_xd)
        xd_intp = da_xd.interp(lon=newlon,lat=newlat)
        yd_intp = da_yd.interp(lon=newlon,lat=newlat)
        zd_intp = da_zd.interp(lon=newlon,lat=newlat)
        ### Step 3 : rotate in Cartesian coordinates
        xd_np, yd_np, zd_np = \
            librotate.tc2np(lonc,latc,\
                xd_intp.values.reshape(nolat,nolon),
                yd_intp.values.reshape(nolat,nolon),
                zd_intp.values.reshape(nolat,nolon))
        ### Step 4 : convert back to spherical
        lonnp = lonin[np.newaxis,:]
        latnp = latin[:,np.newaxis]
        unp, vnp = librotate.xyzd2uv(xd_np,yd_np,zd_np,lonnp,latnp)
        return unp,vnp

    def rotate(self,lonc,latc,lonin,latin,z):
        lonout, latout = \
            librotate.rotate_lonlat(lonc,latc,\
                lonin,latin)
        nolat = latin.size
        nolon = lonin.size
        newlon = xr.DataArray(lonout,dims='np_lonlat')
        newlat = xr.DataArray(latout,dims='np_lonlat')
        print(newlat)
        print(newlon)
        print(np.min(z),np.max(z))
        ### create DataArray
        nlat = self.latm.size
        nlon = self.lonm.size
        da = xr.DataArray(z.reshape(nlat,nlon),\
            [('lat',self.latm),('lon',self.lonm)],name='z')
        print(da)
        ### interpolation
        da_intp = da.interp(lon=newlon,lat=newlat)
        ### output
        zout = da_intp.values.reshape(nolat,nolon)
        return zout

    def tcycle(self,fdict,paramlist):
        for param in paramlist:
            if param[:2]!='WC':
                f = fdict[param]
                if self.plot:
                    fig, ax = plt.subplots(figsize=[8,8])
                    c = ax.contourf(self.lonm, self.latm, f, levels=np.linspace(-0.07, 0.0, 8))
            dg = self.d0
            icyc=0
            while dg > self.dgmin:
                if param[:2]=='WC':
                    gg = self.circulation(fdict,param)
                else:
                    gg = self.barnes(f,param)
                loc_min = np.where( \
                        ndimage.minimum_filter( \
                        gg, size=(3,3), mode=('nearest', 'wrap')) == gg)
                g_min = gg[loc_min[0],loc_min[1]]
                if self.debug: print(g_min)
                n = np.argsort(g_min)[0]
                imin = loc_min[1][n]
                jmin = loc_min[0][n]
                self.centerdict[param] = [self.long[imin],self.latg[jmin]]
                if param[:2]=='WC':
                    f0 = -1.0*g_min[n]
                else:
                    f0 = linint2_points(self.lonm, self.latm, f, self.long[imin],self.latg[jmin])
                self.cvaldict[param] = f0
                icyc+=1
                dg *= 0.5
                lon0, lat0 = self.centerdict[param]
                print(f"{param} iter={icyc} center=({lon0:.3f},{lat0:.3f}) val={f0}")
                self.set_ggrid(lon0, lat0, dg)
                if self.plot: ax.plot(lon0,lat0,marker='^')
            if self.plot:
                ax.set_aspect("equal")
                fig.colorbar(c, ax=ax)
                #fig.savefig("tcycletest_barnes.png", bbox_inches="tight", dpi=300)
                plt.show()
        ## QC
        lonmin=0.0; latmin=0.0
        # Check 1: distance from first guess
        for param in paramlist:
            lonc, latc = self.centerdict[param]
            dist = __distance__(lonc,latc,self.lon0,self.lat0)
            lonmin+=lonc
            latmin+=latc
            if dist > self.distmax / rearth:
                print(f"{param} center is too far from first guess")
                return None,None,1
        # Calculate mean center
        lonmin/=float(len(paramlist))
        latmin/=float(len(paramlist))
        ## Check 2: MSLP gradient
        #ri = np.array([100.0])
        #lattmp = np.arcsin( np.cos(ri / rearth) )
        #lattmp = np.rad2deg(lattmp)
        #slp = fdict['MSLP']
        #slpnp = self.rotate(lonmin, latmin, \
        #    self.loncirc, lattmp, slp)
        #slpmin = self.cvaldict['MSLP']
        #for i in range(slpnp.shape[1]):
        #    psg = slpmin - slpnp[-1,i]
        #    psg = psg * 0.01 / ri
        #    if np.abs(psg) < self.pgradmin:
        #        print(f"MSLP gradient is too small, {psg} mb/km")
        #        return None,None,1
        # Check 3: tangential wind speed
        ri = np.linspace(50.0,self.tanwrad,9)
        lattmp = np.arcsin( np.cos(ri / rearth) )
        lattmp = np.rad2deg(lattmp)
        unp,vnp = self.rotate_uv(lonmin, latmin,\
            self.loncirc, lattmp, fdict['U850'], fdict['V850'])
        tanwavg = 0.0
        n=0
        for i in range(ri.size):
            tanwavg+=np.sum(unp[i,])
            n+=unp.shape[1]
        tanwavg/=float(n)
        if tanwavg < self.tanwmin:
            print(f"Tangential wind is too weak, {tanwavg} m/s")
            return None,None,1
        # Check 4: distance between MSLP and RV850 center
        if 'MSLP' in paramlist and 'RV850' in paramlist:
            lonp, latp = self.centerdict['MSLP']
            lonv, latv = self.centerdict['RV850']
            distp2v = __distance__(lonp,latp,lonv,latv)
            if distp2v > self.distp2vmax / rearth:
                print("MSLP and RV850 centers are diverged")
                return None,None,1
        # Check 5: translation speed
        if self.lonpre is not None and self.latpre is not None:
            distp2c = __distance__(self.lonpre,self.latpre,lonmin,latmin)
            if distp2c > self.distp2cmax / rearth:
                print("Too fast translation")
                return None,None,1
        return lonmin,latmin,0

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

    _,_,_=track.tcycle(fdict,paramlist)
