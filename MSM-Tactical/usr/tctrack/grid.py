# https://docs.scipy.org/doc/scipy/reference/generated/scipy.ndimage.minimum_filter.html#scipy.ndimage.minimum_filter
# https://stackoverflow.com/questions/3986345/how-to-find-the-local-minima-of-a-smooth-multidimensional-array-in-numpy-efficie
import numpy as np
from scipy import ndimage
import biquadratic

def __set_stencil(g, i0, j0):
    ny, nx = g.shape
    ia = (i0 + nx - 1) % nx
    ib = (i0 + 1) % nx
    ja = max(j0 - 1, 0)
    jb = min(j0 + 1, ny - 1)
    f = np.zeros(9)
    f[0] = g[j0, i0]
    f[1] = g[ja, ia]
    f[2] = g[ja, i0]
    f[3] = g[ja, ib]
    f[4] = g[j0, ib]
    f[5] = g[jb, ib]
    f[6] = g[jb, i0]
    f[7] = g[jb, ia]
    f[8] = g[j0, ia]
    return f

def find_minimum(g, lon, lat, lon0, lat0, lonpre, latpre, slppre, sigma=0):
    gg = g if sigma==0 else ndimage.gaussian_filter(g, sigma)
    #print(np.min(gg),np.max(gg))
    loc_min = np.where( \
        ndimage.filters.minimum_filter( \
            gg, size=(3,3), mode=('nearest', 'wrap')) == gg)
    dx = np.deg2rad(lon[loc_min[1]] - lon0)
    y1 = np.deg2rad(lat[loc_min[0]])
    y0 = np.deg2rad(lat0)
    d = np.arccos(np.sin(y0) * np.sin(y1) + np.cos(y0) * np.cos(y1) * np.cos(dx))
    if slppre is None:
        # local minimum sort based on distance
        ncand = np.argsort(d)
    else:
        # local minimum sort based on magnitude
        g_min = g[loc_min[0],loc_min[1]]
        #print(g_min)
        g_min = np.abs(g_min - slppre)
        ncand = np.argsort(g_min)
    print(ncand[:5])
    #  distance threshold
    re = 6.371e3 #[km]
    d_thres = 700.0 / re #[km]
    for i in range(ncand.size):
        n = ncand[i]
        imin = loc_min[1][n]
        jmin = loc_min[0][n]
        f = __set_stencil(g, imin, jmin)
        x, y, gmin = biquadratic.interpolate(f)
        dlon = lon[1] - lon[0]
        lonmin = (lon[imin] + x * dlon) % 360
        dlat = 0.5 * (lat[min(jmin + 1, lat.size-1)] - lat[max(jmin - 1, 0)])
        latmin = min(max(lat[jmin] + y * dlat, -90), 90)
        # check the distance from first guess and previous center
        dxtmp = np.deg2rad(lon0 - lonmin)
        y1tmp = np.deg2rad(lat0)
        y0tmp = np.deg2rad(latmin)
        dtmp0 = np.arccos(np.sin(y0tmp) * np.sin(y1tmp) \
            + np.cos(y0tmp) * np.cos(y1tmp) * np.cos(dxtmp))
        if dtmp0 < d_thres:
            if lonpre is None or latpre is None : break
            dxtmp = np.deg2rad(lonpre - lonmin)
            y1tmp = np.deg2rad(latpre)
            y0tmp = np.deg2rad(latmin)
            dtmp1 = np.arccos(np.sin(y0tmp) * np.sin(y1tmp) \
            + np.cos(y0tmp) * np.cos(y1tmp) * np.cos(dxtmp))
            if dtmp1 < d_thres : break
        #print(dtmp0*re, dtmp1*re)
    print("grid center")
    print(f"{lon[imin]:.3f}, {lat[jmin]:.3f}, {g[jmin,imin]:.5e}")
    return lonmin, latmin, gmin

def find_minimum_loc(g, lon, lat, lon0, lat0, lonpre, latpre, slppre, dc=0, sigma=0):
    gg = g if sigma==0 else ndimage.gaussian_filter(g, sigma)
    # crop local region
    jc = np.argmin(np.abs(lat - lat0))
    if dc > 0:
        jl = np.argmin(np.abs(lat - (lat0-dc)))
        jl = max(jl,0)
        jr = np.argmin(np.abs(lat - (lat0+dc)))
        jr = min(jr,lat.size)
    else:
        jl = max(jc - 50, 0)
        jr = min(jc + 50, lat.size)
    ic = np.argmin(np.abs(lon - lon0))
    if dc > 0:
        il = np.argmin(np.abs(lon - (lon0-dc)))
        il = max(il,0)
        ir = np.argmin(np.abs(lon - (lon0+dc)))
        ir = min(ir,lon.size)
    else:
        il = max(ic - 50, 0)
        ir = min(ic + 50, lon.size)
    lat_loc = lat[jl:jr+1]
    lon_loc = lon[il:ir+1]
    print("lat_loc med:{:.3f} min:{:.3f} max:{:.3f}".format(lat0,np.min(lat_loc),np.max(lat_loc)))
    print("lon_loc med:{:.3f} min:{:.3f} max:{:.3f}".format(lon0,np.min(lon_loc),np.max(lon_loc)))
    g_loc = g[jl:jr+1,il:ir+1]
    gg_loc = gg[jl:jr+1,il:ir+1]
    loc_min = np.where( \
        ndimage.filters.minimum_filter( \
            gg_loc, size=(3,3), mode=('nearest', 'wrap')) == gg_loc)
    dx = np.deg2rad(lon_loc[loc_min[1]] - lon0)
    y1 = np.deg2rad(lat_loc[loc_min[0]])
    y0 = np.deg2rad(lat0)
    d = np.arccos(np.sin(y0) * np.sin(y1) + np.cos(y0) * np.cos(y1) * np.cos(dx))
    if slppre is None:
        # local minimum sort based on distance
        ncand = np.argsort(d)
    else:
        # local minimum sort based on magnitude
        g_min = g_loc[loc_min[0],loc_min[1]]
        #print(g_min)
        g_min = np.abs(g_min - slppre)
        ncand = np.argsort(g_min)
    #### debug ###
    #for n in ncand:
    #    print(f"cand:{n} lon={lon_loc[loc_min[1][n]]} lat={lat_loc[loc_min[0][n]]}")
    #  distance threshold
    re = 6.371e3 #[km]
    d_thres = 600.0 / re #[km]
    for i in range(ncand.size):
        n = ncand[i]
        imin = loc_min[1][n]
        jmin = loc_min[0][n]
        f = __set_stencil(g_loc, imin, jmin)
        x, y, gmin = biquadratic.interpolate(f)
        dlon = lon[1] - lon[0]
        lonmin = (lon_loc[imin] + x * dlon) % 360
        dlat = 0.5 * (lat_loc[min(jmin + 1, lat_loc.size-1)] - lat_loc[max(jmin - 1, 0)])
        latmin = min(max(lat_loc[jmin] + y * dlat, -90), 90)
        if lonpre is None or latpre is None : break
        # check the distance from previous center
        dxtmp = np.deg2rad(lonpre - lonmin)
        y1tmp = np.deg2rad(latpre)
        y0tmp = np.deg2rad(latmin)
        dtmp  = np.arccos(np.sin(y0tmp) * np.sin(y1tmp) \
            + np.cos(y0tmp) * np.cos(y1tmp) * np.cos(dxtmp))
        if dtmp < d_thres : break
        print(f'{dtmp*re}>{d_thres}')
    if i == ncand.size-1 and dtmp >= d_thres:
        print(f"cannot find center")
        return
    print("grid center")
    print(f"{lon_loc[imin]:.3f}, {lat_loc[jmin]:.3f}, {g_loc[jmin,imin]:.5e}")
    return lonmin, latmin, gmin

    
