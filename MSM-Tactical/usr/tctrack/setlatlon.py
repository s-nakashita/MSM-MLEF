import sys
import numpy as np
from numpy import pi, cos, tan, arctan, log, abs, exp

re = 6.3712 * 1e6
qpi = pi / 4.
rad = pi / 180.

def calc_y(lat,a2):
    tanlat = tan( qpi + lat*0.5 )
    y = a2 * log( abs( tanlat ) )
    return y

def calc_lat(y,a2):
    expy = exp( y / a2 )
    invtan = arctan( expy )
    lat = 2.0 * (invtan - qpi)
    return lat

def set_lonlat(rproj, *params):
    if rproj == 0:
        ## mercator projection
        rtruth, delx, dely, igrd, jgrd, rbtmgrd, rlftgrd, rlatc, rlonc \
            = params
        truth = rtruth * rad
        #lats  = rlats * rad
        #lonw  = rlonw * rad
        latc  = rlatc * rad
        lonc  = rlonc * rad
        a2 = re * cos( truth )
#        print(f"a2={a2:11.2f}")

        ### latitude
        rlat = np.zeros(jgrd+1)
        y0 = calc_y(latc,a2)
#        print(f"y0={y0:11.2f}")
        y00 = -(rbtmgrd - 1)*dely
        ys = y00 + y0
#        print(f"ys={ys:11.2f}")
        for j in range(rlat.size):
            y = ys + j*dely
            lat = calc_lat(y,a2)
            rlat[j] = lat / rad

        ### longitude
        rlon = np.zeros(igrd+1)
        x0 = 0.0
        xw = -(rlftgrd - 1)*delx
        for i in range(rlon.size):
            x = xw + x0 + i*delx
            lon = x / a2 + lonc
            rlon[i] = lon / rad
    elif rproj == 4:
        dlat,dlon,clat1,clat2,clon1,clon2 = params
        nlat = int((clat2-clat1)/dlat)+1
        nlon = int((clon2-clon1)/dlon)+1
        rlat = np.linspace(clat1,clat2,nlat,endpoint=True)
        rlon = np.linspace(clon1,clon2,nlon,endpoint=True)
    return rlat, rlon

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} igrd jgrd rproj *params")
        exit()

    igrd = int(sys.argv[1])
    jgrd = int(sys.argv[2])
    rproj = int(sys.argv[3])
    if rproj == 0:
        if len(sys.argv) < 10:
            print(f"Usage: {sys.argv[0]} igrd jgrd rproj rtruth delx dely rbtmgrd rlftgrd rlatc rlonc")
            exit()
        rtruth = float(sys.argv[4])
        delx = float(sys.argv[5])
        dely = float(sys.argv[6])
#       rlats = float(sys.argv[7])
#       rlonw = float(sys.argv[8])
        rbtmgrd = float(sys.argv[7])
        rlftgrd = float(sys.argv[8])
        rlatc = float(sys.argv[9])
        rlonc = float(sys.argv[10])
        params = (rtruth,delx,dely,igrd,jgrd,rbtmgrd,rlftgrd,rlatc,rlonc)
    elif rproj == 4:
        if len(sys.argv) < 9:
            print(f"Usage: {sys.argv[0]} igrd jgrd rproj dlat dlon clat1 clat2 clon1 clon2")
            exit()
        dlat = float(sys.argv[4])
        dlon = float(sys.argv[5])
        clat1 = float(sys.argv[6])
        clat2 = float(sys.argv[7])
        clon1 = float(sys.argv[8])
        clon2 = float(sys.argv[9])
        params = (dlat,dlon,clat1,clat2,clon1,clon2)

    rlat, rlon = \
        set_lonlat(rproj,*params)

    #print(f"rbtmgrd={rbtmgrd:.0f}")
    print(f"rlats={rlat[0]:.3f}")
    print(f"rlatn={rlat[-1]:.3f}")
    np.savetxt("rlat.txt",rlat)
    #print(f"rlftgrd={rlftgrd:.0f}")
    print(f"rlonw={rlon[0]:.3f}")
    print(f"rlone={rlon[-1]:.3f}")
    np.savetxt("rlon.txt",rlon)

