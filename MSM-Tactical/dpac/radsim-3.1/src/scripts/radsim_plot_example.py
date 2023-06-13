#!/usr/bin/env python

# -----------------------------------------------------------------------------
# Example script to plot the simulated data for one channel from a RadSim
# output netCDF file. If RadSim was run for a regular lat/lon grid it uses
# pcolormesh, otherwise it generates a scatter plot for an irregular grid (you
# can pass --scatter T to force a scatter plot). For regular lat/lon grids, the
# code assumes the longitudes are in ascending order.
#
# The netCDF file must contain the latitudes and longitudes so either
# write_latlon or write_profiles must be set to true when running RadSim.
#
# If you plot a per-channel dataset such as bt, refl, emiss, or brdf, you must
# specify the --channel argument (which lies between 1 and the number of
# simulated channels). If you want to plot a model field such as the land/sea
# mask (lsm) or sea-ice field (for example), then simply omit the --channel
# argument. If you want to plot a single level of a multi-level model field
# such as temperature, then you can use the --channel argument to specify the
# level to plot. Note that this script is intended to be modified for your own
# purposes.
#
# The script can also plot the differences in the specified dataset between
# two RadSim output files.
#
# To display usage information:
#   $ radsim_plot_example.py -h
# -----------------------------------------------------------------------------

import argparse
import numpy as np
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import netCDF4 as nc
from datetime import datetime, timedelta
import sys
plt.rcParams['font.size'] = 18

def parse_args():
    """
    Parse the script arguments
    """

    class TFAction(argparse.Action):
        def __init__(self, option_strings, dest, nargs=None, **kwargs):
            super(TFAction, self).__init__(option_strings, dest, **kwargs)
        def __call__(self, parser, namespace, values, option_string=None):
            if values.lower() in ('t', 'true'):
                setattr(namespace, self.dest, True)
            elif values.lower() in ('f', 'false'):
                setattr(namespace, self.dest, False)
            else:
                raise ValueError("True/False arguments require T or F")

    parser = argparse.ArgumentParser( \
        description='Plot simulated data for one channel from RadSim output file',
        conflict_handler='resolve')

    parser.add_argument('--filename', dest='filename', type=str, required=True,
                        help='name of RadSim netcdf file containing output data',
                        metavar='FILE')
    parser.add_argument('--varname', dest='varname', type=str, required=False,
                        help='name of variable to plot e.g. bt or refl (default: bt)',
                        metavar='STRING', default='bt')
    parser.add_argument('--channel', dest='channel', type=int, required=False,
                        help='channel to plot (required for per-channel datasets e.g. bt/refl/emiss/brdf)', 
                        metavar='INTEGER')

    parser.add_argument('--scatter', dest='scatter', type=str, action=TFAction, required=False,
                        help='generate a scatter plot (default: false)',
                        metavar='T|F', default=False)
    parser.add_argument('--size', dest='size', type=float, required=False,
                        help='size of dots for scatter plot (default: 1.5)',
                        metavar='FLOAT', default=1.5)

    parser.add_argument('--vmin', dest='vmin', type=float, required=False,
                        help='minimum value for colour scale (default: min value of data)',
                        metavar='FLOAT')
    parser.add_argument('--vmax', dest='vmax', type=float, required=False,
                        help='maximum value for colour scale (default: max value of data)',
                        metavar='FLOAT')
    parser.add_argument('--cmap', dest='cmap', type=str, required=False,
                        help='Python colour map to use for the plot (default: seismic for difference plots, jet otherwise)',
                        metavar='STRING')

    parser.add_argument('--title', dest='title', type=str, required=False,
                        help='Title for plot (in quotes "")',
                        metavar='STRING')

    parser.add_argument('--diff', dest='diff', type=str, required=False,
                        help='name of RadSim netcdf file containing output data to diff with --filename',
                        metavar='FILE')

    parser.add_argument('--lonmin', dest='lonmin', type=float, required=False,
                        help='minimum value for longitude (default: min value of data longitude)',
                        metavar='FLOAT')
    parser.add_argument('--lonmax', dest='lonmax', type=float, required=False,
                        help='maximum value for longitude (default: max value of data longitude)',
                        metavar='FLOAT')
    parser.add_argument('--latmin', dest='latmin', type=float, required=False,
                        help='minimum value for latitude (default: min value of data latitude)',
                        metavar='FLOAT')
    parser.add_argument('--latmax', dest='latmax', type=float, required=False,
                        help='maximum value for latitude (default: max value of data latitude)',
                        metavar='FLOAT')

    parser.add_argument('--boxlonmin', dest='boxlonmin', type=float, required=False,
                        help='minimum value for box longitude (default: None)',
                        metavar='FLOAT')
    parser.add_argument('--boxlonmax', dest='boxlonmax', type=float, required=False,
                        help='maximum value for box longitude (default: None)',
                        metavar='FLOAT')
    parser.add_argument('--boxlatmin', dest='boxlatmin', type=float, required=False,
                        help='minimum value for box latitude (default: None)',
                        metavar='FLOAT')
    parser.add_argument('--boxlatmax', dest='boxlatmax', type=float, required=False,
                        help='maximum value for box latitude (default: None)',
                        metavar='FLOAT')

    parser.add_argument('--cltop', dest='cltop', type=str, action=TFAction, required=False,
                        help='generate a cloud-top highright plot (default: false)',
                        metavar='T|F', default=False)

    return parser.parse_args()


def make_plot(filename, varname, chan, scatter, size, vmin, vmax, cmapstr, title, diff_filename, \
    lonmin, lonmax, latmin, latmax, cltop,
    boxlonmin, boxlonmax, boxlatmin, boxlatmax):
    """
    Read data and generate plot
    """

    do_diff = diff_filename is not None

    if cmapstr is None:
        if do_diff:
            cmapstr = 'seismic'
        else:
            cmapstr = 'jet'

    # Open RadSim netCDF file(s)
    try:
        rootgrp = nc.Dataset(filename, 'r', format='NETCDF4')
    except:
        print('Error opening file ' + filename)
        sys.exit(1)

    if do_diff:
        try:
            rootgrpdiff = nc.Dataset(diff_filename, 'r', format='NETCDF4')
        except:
            print('Error opening file ' + diff_filename)
            sys.exit(1)

    # Check for dataset
    varname = varname.lower()
    if varname not in rootgrp.variables:
        print('Error: varname ' + varname + ' not found in ' + filename)
        sys.exit(1)
    if 'lat' not in rootgrp.variables or 'lon' not in rootgrp.variables:
        print('Error: netCDF file must contain lat and lon values')
        sys.exit(1)
    if do_diff:
        if varname not in rootgrpdiff.variables:
            print('Error: varname ' + varname + ' not found in ' + diff_filename)
            sys.exit(1)

    # Begin by assuming a regular grid: we deal with irregular grids below.

    # For regular grids, we specify the pcolormesh boundaries at the mid-points
    # between the grid lat/lon so that the plotted rectangles are centred on
    # the grid lat/lon.

    # Determine latitudes: read all latitudes, count unique values,
    # find min/max, and generate the array of latitude values (they
    # may be increasing or decreasing)
    lat = rootgrp.variables['lat'][:]
    nprof = len(lat)
    nlat = len(set(lat))
    latstart, latend = lat[0], lat[-1]
    if latmin is None: latmin = min(latstart,latend)
    if latmax is None: latmax = max(latstart,latend)
    dlat = (latend - latstart) / float(nlat - 1)
    lat = np.arange(latstart, latend + dlat, dlat)

    # Determine longitudes: same as for latitudes. For simplicity we assume that
    # longitudes are in ascending order.
    lon = rootgrp.variables['lon'][:]
    nlon = len(set(lon))
    if lon[0] > lon[-1]:
        lon = np.where(lon >= 180., lon - 360., lon)
    lonstart, lonend = lon[0], lon[-1]
    if lonmin is None: lonmin = min(lonstart,lonend)
    if lonmax is None: lonmax = max(lonstart,lonend)
    dlon = (lonend - lonstart) / float(nlon - 1)
    lon = np.arange(lonstart, lonend + dlon, dlon)
    centerlon = 0.5*(lonstart + lonend)

    if scatter or nlat * nlon != nprof:
        # Irregular grid/scatter plot: use lats/lons directly
        scatter = True
        lat = rootgrp.variables['lat'][:]
        lon = rootgrp.variables['lon'][:]

    # Read dataset(s) and reshape
    dat = np.squeeze(rootgrp.variables[varname][:])
    print(dat.shape)
    if chan is not None:
        nchan = dat.shape[0]
        if chan < 1 or chan > nchan:
            print('Error: channel must lie between 1 and ' + str(nchan))
            sys.exit(1)
        dat = dat[chan-1,:]
    else:
        if len(dat.shape) > 1:
            print('Error: need to specify channel')
            sys.exit(1)

    if not scatter: dat.shape = (nlat,nlon)

    if do_diff:
        dat2 = np.squeeze(rootgrpdiff.variables[varname][:])

        if chan is not None:
            nchan = dat2.shape[0]
            if chan < 1 or chan > nchan:
                print('Error: channel must lie between 1 and ' + str(nchan) + ' for diff dataset')
                sys.exit(1)
            dat2 = dat2[chan-1,:]
        else:
            if len(dat2.shape) > 1:
                print('Error: need to specify channel')
                sys.exit(1)

        if not scatter: dat2.shape = (nlat,nlon)

        dat = dat - dat2

    # Get min/max values for colour bar
    if do_diff:
        # Defaut for difference plots is min/max equidistant from zero encompassing data range
        if vmin is None or vmax is None:
            v = max(np.abs(np.min(dat)), np.abs(np.max(dat)))
            vmin, vmax = -v, v
    else:
        # Default for normal plots is min/max of dataset
        if vmin is None: vmin = np.min(dat)
        if vmax is None: vmax = np.max(dat)
    if vmin > vmax:
        print('Error: vmin >= vmax')
        sys.exit(1)

    # Specify subplot
    fig, ax = plt.subplots(1, 1, figsize=(8,6),
        subplot_kw={'projection': ccrs.PlateCarree(central_longitude=180)})
    #fig.subplots_adjust(hspace=0, wspace=0, top=0.925, left=0.1)

    ## Add the colour bar axes
    #cbar_ax = fig.add_axes([0, 0, 0, 0])
    #posn = ax.get_position()
    #cbar_ax.set_position([0.91, posn.y0, 0.04, posn.height])

    # Plot the selected data
    if scatter:
        im = ax.scatter(lon, lat, c=dat, transform=ccrs.PlateCarree(), s=size,
                        cmap=cmapstr, vmin=vmin, vmax=vmax, antialiased=False)
    else:
        im = ax.pcolormesh(lon, lat, dat, transform=ccrs.PlateCarree(),
                           cmap=cmapstr, vmin=vmin, vmax=vmax)
    
    # highlight cloud-top
    if cltop:
        vminrgb, vmaxrgb = 200, 240
        cmaprgb = 'rainbow_r'
        datrgb = np.where(dat>vmaxrgb,np.nan,dat)
        if scatter:
            im = ax.scatter(lon, lat, c=datrgb, transform=ccrs.PlateCarree(), s=size,
                        cmap=cmaprgb, vmin=vminrgb, vmax=vmaxrgb, antialiased=False)
        else:
            im = ax.pcolormesh(lon, lat, datrgb, transform=ccrs.PlateCarree(),
                           cmap=cmaprgb, vmin=vminrgb, vmax=vmaxrgb)

    # Add coastlines, gridlines, colour bar, title
    #if abs(lonmax - lonmin) > 100:
    #    dlon = 5
    #else:
    #    dlon = 1
    #if abs(latmax - latmin) > 100:
    #    dlat = 5
    #else:
    #    dlat = 1
    #lonticks = np.arange(lonmin,lonmax,dlon)
    #latticks = np.arange(latmin,latmax,dlat)
    #print(lonticks)
    #print(latticks)
    #ax.set_xticks(lonticks,ccrs.PlateCarree())
    #ax.set_yticks(latticks,ccrs.PlateCarree())
    ax.set_extent([lonmin,lonmax,latmin,latmax],crs=ccrs.PlateCarree())
    ax.coastlines(resolution='50m') # options: 110m, 50m 10m
    gl=ax.gridlines(draw_labels=True,crs=ccrs.PlateCarree())
    gl.top_labels = False
    gl.right_labels = False
    fig.colorbar(im, ax=ax, pad=0.01, shrink=0.6)
    if boxlonmax is not None and boxlonmin is not None \
        and boxlatmax is not None and boxlatmin is not None:
        boxlon=[boxlonmin,boxlonmax,boxlonmax,boxlonmin,boxlonmin]
        boxlat=[boxlatmax,boxlatmax,boxlatmin,boxlatmin,boxlatmax]
        ax.plot(boxlon,boxlat,c='k',lw=2,transform=ccrs.PlateCarree())
    if title is None:
        vdate = datetime(*rootgrp.validity_time)
        idate = datetime(*rootgrp.data_time)
        fh = (vdate - idate).total_seconds()//3600.
        print(vdate,fh)
        try:
            if fh >= 0:
                timestr = f' at {idate}+{int(fh)}H'
            else:
                timestr = ' at data time ' + ' '.join(map(str, rootgrp.data_time))
        except:
            timestr = ''
        diffstr = ''
        if do_diff: diffstr = ' differences'
        if chan is not None:
            title = 'Simulated ' + varname.upper() + diffstr + ' for ' + \
                    rootgrp.platform.upper() + ' ' + str(rootgrp.satid) + ' ' + \
                    rootgrp.instrument.upper() + ' ch' + str(chan) + '\n' + timestr
        else:
            title = 'Simulated ' + varname.upper() + diffstr + ' for ' + \
                    rootgrp.platform.upper() + ' ' + str(rootgrp.satid) + ' ' + \
                    rootgrp.instrument.upper() + '\n' + timestr
    ax.set_title(title+f"\nmax={dat.max():.2f}K min={dat.min():.2f}K")
    fig.savefig(filename+".png",dpi=300)
    #fig.savefig(filename+".ps")

    # Display the plot
    #plt.show()


if __name__ == '__main__':
    args = parse_args()
    make_plot(args.filename, args.varname, args.channel, args.scatter,
              args.size, args.vmin, args.vmax, args.cmap, args.title, args.diff,
              args.lonmin,args.lonmax,args.latmin,args.latmax, args.cltop,
              args.boxlonmin,args.boxlonmax,args.boxlatmin,args.boxlatmax)
