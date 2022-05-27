import sys
from numpy import pi, cos, tan, arctan, log, abs, exp

if len(sys.argv) < 9:
    print(f"Usage: {sys.argv[0]} rtruth delx dely igrd jgrd rlats rlonw rlatc rlonc")
    exit()

rtruth = float(sys.argv[1])
delx = float(sys.argv[2])
dely = float(sys.argv[3])
igrd = int(sys.argv[4])
jgrd = int(sys.argv[5])
rlats = float(sys.argv[6])
rlonw = float(sys.argv[7])
rlatc = float(sys.argv[8])
rlonc = float(sys.argv[9])

re = 6.3712 * 1e6
qpi = pi / 4.
rad = pi / 180.

truth = rtruth * rad
lats  = rlats * rad
lonw  = rlonw * rad
latc  = rlatc * rad
lonc  = rlonc * rad
a2 = re * cos( truth )
print(f"a2={a2:11.2f}")

def calc_y(lat):
    tanlat = tan( qpi + lat*0.5 )
    y = a2 * log( abs( tanlat ) )
    return y
def calc_lat(y):
    expy = exp( y / a2 )
    invtan = arctan( expy )
    lat = 2.0 * (invtan - qpi)
    return lat

y0 = calc_y(latc)
print(f"y0={y0:11.2f}")
ys = calc_y(lats)
print(f"ys={ys:11.2f}")
y00 = ys - y0
print(f"y00={y00:11.2f}")
rbtmgrd = -y00 / dely + 1
print(f"rbtmgrd={rbtmgrd:.1f}")
yn = y0 + y00 + (jgrd+1)*dely
print(f"yn={yn:11.2f}")
latn = calc_lat(yn)
rlatn = latn / rad
print(f"rlatn={rlatn:.1f}")

x0 = 0.0
x00 = (lonw - lonc) * a2
rlftgrd = -x00 / delx + 1
print(f"rlftgrd={rlftgrd:.1f}")
xe = x00 + (igrd+1)*delx
lone = xe / a2 + lonc
rlone = lone / rad
print(f"rlone={rlone:.1f}")

