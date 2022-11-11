#!/bin/sh
#
# makes a file with climate division data from grib file
#
flx=$1

grbpoint.pl -ncep_opn -s -4yr -lat 44.91 -lon -71.06      $flx >cd001
grbpoint.pl -ncep_opn -s -4yr -lat 44.00 -lon -70.07      $flx >cd002
grbpoint.pl -ncep_opn -s -4yr -lat 43.68 -lon -74.39      $flx >cd003
grbpoint.pl -ncep_opn -s -4yr -lat 41.86 -lon -72.64      $flx >cd004
grbpoint.pl -ncep_opn -s -4yr -lat 42.22 -lon -78.55      $flx >cd005
grbpoint.pl -ncep_opn -s -4yr -lat 40.22 -lon -83.01      $flx >cd006
grbpoint.pl -ncep_opn -s -4yr -lat 39.31 -lon -76.03      $flx >cd007
grbpoint.pl -ncep_opn -s -4yr -lat 40.95 -lon -77.51      $flx >cd008
grbpoint.pl -ncep_opn -s -4yr -lat 38.83 -lon -80.00      $flx >cd009
grbpoint.pl -ncep_opn -s -4yr -lat 36.95 -lon -77.69      $flx >cd010
grbpoint.pl -ncep_opn -s -4yr -lat 35.92 -lon -82.38      $flx >cd011
grbpoint.pl -ncep_opn -s -4yr -lat 34.22 -lon -78.92      $flx >cd012
grbpoint.pl -ncep_opn -s -4yr -lat 34.89 -lon -80.74      $flx >cd013
grbpoint.pl -ncep_opn -s -4yr -lat 45.50 -lon -87.63      $flx >cd014
grbpoint.pl -ncep_opn -s -4yr -lat 47.66 -lon -94.06      $flx >cd015
grbpoint.pl -ncep_opn -s -4yr -lat 47.48 -lon -98.75      $flx >cd016
grbpoint.pl -ncep_opn -s -4yr -lat 47.24 -lon -102.22      $flx >cd017
grbpoint.pl -ncep_opn -s -4yr -lat 46.95 -lon -105.62      $flx >cd018
grbpoint.pl -ncep_opn -s -4yr -lat 48.23 -lon -110.29      $flx >cd019
grbpoint.pl -ncep_opn -s -4yr -lat 46.24 -lon -109.52      $flx >cd020
grbpoint.pl -ncep_opn -s -4yr -lat 46.32 -lon -113.22      $flx >cd021
grbpoint.pl -ncep_opn -s -4yr -lat 43.78 -lon -84.91      $flx >cd022
grbpoint.pl -ncep_opn -s -4yr -lat 41.64 -lon -84.71      $flx >cd023
grbpoint.pl -ncep_opn -s -4yr -lat 40.17 -lon -87.64      $flx >cd024
grbpoint.pl -ncep_opn -s -4yr -lat 42.54 -lon -89.10      $flx >cd025
grbpoint.pl -ncep_opn -s -4yr -lat 45.01 -lon -90.57      $flx >cd026
grbpoint.pl -ncep_opn -s -4yr -lat 44.83 -lon -93.49      $flx >cd027
grbpoint.pl -ncep_opn -s -4yr -lat 44.92 -lon -96.70      $flx >cd028
grbpoint.pl -ncep_opn -s -4yr -lat 44.47 -lon -100.13      $flx >cd029
grbpoint.pl -ncep_opn -s -4yr -lat 44.24 -lon -102.88      $flx >cd030
grbpoint.pl -ncep_opn -s -4yr -lat 43.98 -lon -105.27      $flx >cd031
grbpoint.pl -ncep_opn -s -4yr -lat 43.96 -lon -109.37      $flx >cd032
grbpoint.pl -ncep_opn -s -4yr -lat 42.00 -lon -92.53      $flx >cd033
grbpoint.pl -ncep_opn -s -4yr -lat 42.67 -lon -96.43      $flx >cd034
grbpoint.pl -ncep_opn -s -4yr -lat 41.48 -lon -100.74      $flx >cd035
grbpoint.pl -ncep_opn -s -4yr -lat 40.82 -lon -97.53      $flx >cd036
grbpoint.pl -ncep_opn -s -4yr -lat 42.00 -lon -104.00      $flx >cd037
grbpoint.pl -ncep_opn -s -4yr -lat 38.35 -lon -84.01      $flx >cd038
grbpoint.pl -ncep_opn -s -4yr -lat 37.90 -lon -87.09      $flx >cd039
grbpoint.pl -ncep_opn -s -4yr -lat 37.32 -lon -90.08      $flx >cd040
grbpoint.pl -ncep_opn -s -4yr -lat 39.73 -lon -90.78      $flx >cd041
grbpoint.pl -ncep_opn -s -4yr -lat 39.04 -lon -93.65      $flx >cd042
grbpoint.pl -ncep_opn -s -4yr -lat 38.03 -lon -95.79      $flx >cd043
grbpoint.pl -ncep_opn -s -4yr -lat 38.60 -lon -98.27      $flx >cd044
grbpoint.pl -ncep_opn -s -4yr -lat 38.17 -lon -100.78      $flx >cd045
grbpoint.pl -ncep_opn -s -4yr -lat 39.82 -lon -103.44      $flx >cd046
grbpoint.pl -ncep_opn -s -4yr -lat 38.10 -lon -103.97      $flx >cd047
grbpoint.pl -ncep_opn -s -4yr -lat 39.13 -lon -107.73      $flx >cd048
grbpoint.pl -ncep_opn -s -4yr -lat 41.94 -lon -108.26      $flx >cd049
grbpoint.pl -ncep_opn -s -4yr -lat 35.31 -lon -86.54      $flx >cd050
grbpoint.pl -ncep_opn -s -4yr -lat 34.88 -lon -89.92      $flx >cd051
grbpoint.pl -ncep_opn -s -4yr -lat 35.71 -lon -93.89      $flx >cd052
grbpoint.pl -ncep_opn -s -4yr -lat 35.52 -lon -97.83      $flx >cd053
grbpoint.pl -ncep_opn -s -4yr -lat 34.10 -lon -99.55      $flx >cd054
grbpoint.pl -ncep_opn -s -4yr -lat 34.50 -lon -101.64      $flx >cd055
grbpoint.pl -ncep_opn -s -4yr -lat 33.63 -lon -83.71      $flx >cd056
grbpoint.pl -ncep_opn -s -4yr -lat 33.27 -lon -86.55      $flx >cd057
grbpoint.pl -ncep_opn -s -4yr -lat 32.39 -lon -89.74      $flx >cd058
grbpoint.pl -ncep_opn -s -4yr -lat 33.30 -lon -92.61      $flx >cd059
grbpoint.pl -ncep_opn -s -4yr -lat 31.77 -lon -94.88      $flx >cd060
grbpoint.pl -ncep_opn -s -4yr -lat 32.35 -lon -97.47      $flx >cd061
grbpoint.pl -ncep_opn -s -4yr -lat 29.38 -lon -97.37      $flx >cd062
grbpoint.pl -ncep_opn -s -4yr -lat 27.06 -lon -98.30      $flx >cd063
grbpoint.pl -ncep_opn -s -4yr -lat 30.56 -lon -100.08      $flx >cd064
grbpoint.pl -ncep_opn -s -4yr -lat 30.82 -lon -103.55      $flx >cd065
grbpoint.pl -ncep_opn -s -4yr -lat 31.04 -lon -83.18      $flx >cd066
grbpoint.pl -ncep_opn -s -4yr -lat 28.28 -lon -81.68      $flx >cd067
grbpoint.pl -ncep_opn -s -4yr -lat 25.71 -lon -80.97      $flx >cd068
grbpoint.pl -ncep_opn -s -4yr -lat 30.81 -lon -86.86      $flx >cd069
grbpoint.pl -ncep_opn -s -4yr -lat 30.42 -lon -90.71      $flx >cd070
grbpoint.pl -ncep_opn -s -4yr -lat 30.37 -lon -93.82      $flx >cd071
grbpoint.pl -ncep_opn -s -4yr -lat 48.11 -lon -117.92      $flx >cd072
grbpoint.pl -ncep_opn -s -4yr -lat 46.15 -lon -117.23      $flx >cd073
grbpoint.pl -ncep_opn -s -4yr -lat 46.50 -lon -120.09      $flx >cd074
grbpoint.pl -ncep_opn -s -4yr -lat 47.31 -lon -122.20      $flx >cd075
grbpoint.pl -ncep_opn -s -4yr -lat 47.86 -lon -123.32      $flx >cd076
grbpoint.pl -ncep_opn -s -4yr -lat 43.15 -lon -112.94      $flx >cd077
grbpoint.pl -ncep_opn -s -4yr -lat 44.60 -lon -114.40      $flx >cd078
grbpoint.pl -ncep_opn -s -4yr -lat 42.98 -lon -116.54      $flx >cd079
grbpoint.pl -ncep_opn -s -4yr -lat 43.23 -lon -120.63      $flx >cd080
grbpoint.pl -ncep_opn -s -4yr -lat 44.09 -lon -122.60      $flx >cd081
grbpoint.pl -ncep_opn -s -4yr -lat 44.50 -lon -123.74      $flx >cd082
grbpoint.pl -ncep_opn -s -4yr -lat 40.50 -lon -110.92      $flx >cd083
grbpoint.pl -ncep_opn -s -4yr -lat 38.35 -lon -111.07      $flx >cd084
grbpoint.pl -ncep_opn -s -4yr -lat 38.53 -lon -113.40      $flx >cd085
grbpoint.pl -ncep_opn -s -4yr -lat 40.50 -lon -115.48      $flx >cd086
grbpoint.pl -ncep_opn -s -4yr -lat 40.57 -lon -119.37      $flx >cd087
grbpoint.pl -ncep_opn -s -4yr -lat 39.76 -lon -121.60      $flx >cd088
grbpoint.pl -ncep_opn -s -4yr -lat 40.46 -lon -123.27      $flx >cd089
grbpoint.pl -ncep_opn -s -4yr -lat 38.06 -lon -116.23      $flx >cd090
grbpoint.pl -ncep_opn -s -4yr -lat 36.37 -lon -119.83      $flx >cd091
grbpoint.pl -ncep_opn -s -4yr -lat 36.42 -lon -121.30      $flx >cd092
grbpoint.pl -ncep_opn -s -4yr -lat 33.96 -lon -118.26      $flx >cd093
grbpoint.pl -ncep_opn -s -4yr -lat 35.21 -lon -116.55      $flx >cd094
grbpoint.pl -ncep_opn -s -4yr -lat 36.01 -lon -114.35      $flx >cd095
grbpoint.pl -ncep_opn -s -4yr -lat 33.67 -lon -112.26      $flx >cd096
grbpoint.pl -ncep_opn -s -4yr -lat 35.58 -lon -110.69      $flx >cd097
grbpoint.pl -ncep_opn -s -4yr -lat 32.27 -lon -110.52      $flx >cd098
grbpoint.pl -ncep_opn -s -4yr -lat 36.59 -lon -106.66      $flx >cd099
grbpoint.pl -ncep_opn -s -4yr -lat 34.32 -lon -103.82      $flx >cd100
grbpoint.pl -ncep_opn -s -4yr -lat 34.00 -lon -106.83      $flx >cd101
grbpoint.pl -ncep_opn -s -4yr -lat 32.30 -lon -107.19      $flx >cd102
grbpoint.pl -ncep_opn -s -4yr -lat 71.30 -lon -156.78      $flx >cd103
grbpoint.pl -ncep_opn -s -4yr -lat 66.87 -lon -162.63      $flx >cd104
grbpoint.pl -ncep_opn -s -4yr -lat 64.00 -lon -145.73      $flx >cd105
grbpoint.pl -ncep_opn -s -4yr -lat 66.90 -lon -151.52      $flx >cd106
grbpoint.pl -ncep_opn -s -4yr -lat 64.82 -lon -147.87      $flx >cd107
grbpoint.pl -ncep_opn -s -4yr -lat 62.97 -lon -155.62      $flx >cd108
grbpoint.pl -ncep_opn -s -4yr -lat 60.78 -lon -161.80      $flx >cd109
grbpoint.pl -ncep_opn -s -4yr -lat 64.50 -lon -165.43      $flx >cd110
grbpoint.pl -ncep_opn -s -4yr -lat 55.03 -lon -131.57      $flx >cd111
grbpoint.pl -ncep_opn -s -4yr -lat 58.37 -lon -134.58      $flx >cd112
grbpoint.pl -ncep_opn -s -4yr -lat 55.35 -lon -131.70      $flx >cd113
grbpoint.pl -ncep_opn -s -4yr -lat 57.07 -lon -135.35      $flx >cd114
grbpoint.pl -ncep_opn -s -4yr -lat 61.17 -lon -150.02      $flx >cd115
grbpoint.pl -ncep_opn -s -4yr -lat 60.57 -lon -151.25      $flx >cd116
grbpoint.pl -ncep_opn -s -4yr -lat 62.15 -lon -145.45      $flx >cd117
grbpoint.pl -ncep_opn -s -4yr -lat 59.63 -lon -151.50      $flx >cd118
grbpoint.pl -ncep_opn -s -4yr -lat 59.75 -lon -154.92      $flx >cd119
grbpoint.pl -ncep_opn -s -4yr -lat 51.88 -lon -176.65      $flx >cd120
grbpoint.pl -ncep_opn -s -4yr -lat 55.20 -lon -162.73      $flx >cd121
grbpoint.pl -ncep_opn -s -4yr -lat 57.15 -lon -170.22      $flx >cd122
grbpoint.pl -ncep_opn -s -4yr -lat 57.75 -lon -152.50      $flx >cd123
grbpoint.pl -ncep_opn -s -4yr -lat 60.50 -lon -145.50      $flx >cd124
grbpoint.pl -ncep_opn -s -4yr -lat 61.13 -lon -146.35      $flx >cd125
grbpoint.pl -ncep_opn -s -4yr -lat 59.52 -lon -139.67      $flx >cd126
grbpoint.pl -ncep_opn -s -4yr -lat 19.72 -lon -155.07      $flx >cd127
grbpoint.pl -ncep_opn -s -4yr -lat 20.90 -lon -156.43      $flx >cd128
grbpoint.pl -ncep_opn -s -4yr -lat 21.35 -lon -157.93      $flx >cd129
grbpoint.pl -ncep_opn -s -4yr -lat 21.98 -lon -159.35      $flx >cd130