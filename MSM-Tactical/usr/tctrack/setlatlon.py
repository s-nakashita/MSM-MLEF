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

def set_grids(rtruth, delx, dely, igrd, jgrd, rlats, rlonw, rlatc, rlonc):
    truth = rtruth * rad
    lats  = rlats * rad
    lonw  = rlonw * rad
    latc  = rlatc * rad
    lonc  = rlonc * rad
    a2 = re * cos( truth )
#    print(f"a2={a2:11.2f}")

    y0 = calc_y(latc,a2)
#    print(f"y0={y0:11.2f}")
    ys = calc_y(lats,a2)
#    print(f"ys={ys:11.2f}")
    y00 = ys - y0
#    print(f"y00={y00:11.2f}")
    rbtmgrd = -y00 / dely + 1
    yn = y0 + y00 + (jgrd+1)*dely
#    print(f"yn={yn:11.2f}")
    latn = calc_lat(yn,a2)
    rlatn = latn / rad

    x0 = 0.0
    x00 = (lonw - lonc) * a2
    rlftgrd = -x00 / delx + 1
    xe = x00 + (igrd+1)*delx
    lone = xe / a2 + lonc
    rlone = lone / rad
    return rbtmgrd, rlftgrd, rlatn, rlone

def set_lonlat(rtruth, delx, dely, igrd, jgrd, rbtmgrd, rlftgrd, rlatc, rlonc):
    truth = rtruth * rad
    #lats  = rlats * rad
    #lonw  = rlonw * rad
    latc  = rlatc * rad
    lonc  = rlonc * rad
    a2 = re * cos( truth )
#    print(f"a2={a2:11.2f}")

    ### latitude
    rlat = np.zeros(jgrd+1)
    y0 = calc_y(latc,a2)
#    print(f"y0={y0:11.2f}")
    y00 = -(rbtmgrd - 1)*dely
    ys = y00 + y0
#    print(f"ys={ys:11.2f}")
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

    return rlat, rlon

if __name__ == "__main__":
    if len(sys.argv) < 9:
        print(f"Usage: {sys.argv[0]} igrd jgrd rtruth delx dely rbtmgrd rlftgrd rlatc rlonc")
        exit()

    igrd = int(sys.argv[1])
    jgrd = int(sys.argv[2])
    rtruth = float(sys.argv[3])
    delx = float(sys.argv[4])
    dely = float(sys.argv[5])
#    rlats = float(sys.argv[6])
#    rlonw = float(sys.argv[7])
    rbtmgrd = float(sys.argv[6])
    rlftgrd = float(sys.argv[7])
    rlatc = float(sys.argv[8])
    rlonc = float(sys.argv[9])

    #rbtmgrd, rlftgrd, rlatn, rlone = \
    #set_grids(rtruth, delx, dely, igrd, jgrd, rlats, rlonw, rlatc, rlonc)
    rlat, rlon = \
        set_lonlat(rtruth,delx,dely,igrd,jgrd,rbtmgrd,rlftgrd,rlatc,rlonc)
    #print(f"rbtmgrd={rbtmgrd:.0f}")
    print(f"rlats={rlat[0]:.3f}")
    print(f"rlatn={rlat[-1]:.3f}")
    np.savetxt("rlat.txt",rlat)
    #print(f"rlftgrd={rlftgrd:.0f}")
    print(f"rlonw={rlon[0]:.3f}")
    print(f"rlone={rlon[-1]:.3f}")
    np.savetxt("rlon.txt",rlon)

