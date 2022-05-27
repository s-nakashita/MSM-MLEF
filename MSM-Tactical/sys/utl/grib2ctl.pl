#!/usr/bin/perl -w
#   makes a GrADS control file for grib files
#
#   requires wgrib and Perl5
#
#   usage: grib2ctl [options] [grib file] [optional index file] >[control file]
#
#   note: this script does not make the index file .. you have to run gribmap
#
#   Analyses: (using initial time)
#
#      $ grib2ctl.pl example.grib >example.ctl
#      $ gribmap -i example.ctl -0
#
#   Forecasts: (using verifiation time)
#
#      $ grib2ctl.pl -verf example.grib >example.ctl
#      $ gribmap -i example.ctl
#
#   bugs:
#         many
#	  will fail under number of situations
#         finite number of NCEP grids are supported
#
# requires wgrib 1.7.4 or higher
# wesley ebisuzaki, http://wesley.ncep.noaa.gov/grib2ctl.html
#
# added output for rotated LatLon grids
#             Helmut P. Frank,  Helmut.Frank@dwd.de
#             Fri Sep 14 13:54:00 GMT 2001
# -ts, -lc options: Ag Stephens, BADC 3/2003

$version="0.9.12.5p33j beta6";
use POSIX;

# ***** if wgrib is not on path, add it here
#$wgrib='/u/wx51we/bin/wgrib';
$wgrib='/usrx/local/grads/bin/wgrib';

# **** directory of interpolation files
# $pdef_dir='/usr/local/lib/grads';
$pdef_dir='/u/wx51we/home/grads';

$wflag="";
$file="";
$index="";
$prs="prs";
$suffix="";
$z_order="prs";
$model="GSF";
$calendar="";
$lc="";
$soil_model="OSU";
$global_map="";
$timestep="";
$rr="";
$nearest_neighbor="";
$kludge="";
$template="";

foreach $_ (@ARGV) {
   SWITCH: {
      /^-verf/ && do { $wflag="$wflag -verf" ; last SWITCH; };
      /^-ncep_opn/ && do { $wflag="$wflag -ncep_opn" ; last SWITCH; };
      /^-ncep_rean/ && do { $wflag="$wflag -ncep_rean" ; last SWITCH; };
      /^-no_prs/ && do { $prs="" ; last SWITCH; };
      /^-no_suffix/ && do { $suffix="no" ; last SWITCH; };
      /^-rev_z/ && do { $z_order="theta"; last SWITCH; };
      /^-365/ && do { $calendar="365"; last SWITCH; };
      /^-ts(\d+\w+)/ && do { $timestep=$1; last SWITCH; };
      /^-lc$/ && do { $lc="on"; last SWITCH; };
      /^-rr_nn$/ && do { $rr="on"; $nearest_neighbor="on"; $model="ETA"; $soil_model="NOAH"; last SWITCH; };
      /^-rr$/ && do { $rr="on"; $model="ETA"; $soil_model="NOAH"; last SWITCH; };
      /^-eta$/ && do { $model="ETA"; $soil_model="NOAH"; last SWITCH; };
      /^-mrf$/ && do { $model="GSF"; last SWITCH; };
      /^-gsf$/ && do { $model="GSF"; last SWITCH; };
      /^-ruc$/ && do { $model="RUC"; last SWITCH; };
      /^-global$/ && do { $global_map = "on"; last SWITCH; };
      /^-noah$/ && do { $soil_model="NOAH"; $wflag="$wflag -ncep_opn"; last SWITCH; };
      /^-osu/ && do { $soil_model="OSU"; last SWITCH; };
      /^-kludge/ && do { $kludge="on"; last SWITCH; };
      /^-/ && do { print STDERR "unknown option: $_\n"; exit 8; };
      if ($file eq "") {
         $file="$_";
      }
      else {
         $index="$_";
      }
   }
}

if ("$file" eq "") {
   if ($#ARGV >= 0) {
      print STDERR "*** missing grib file ***\n\n\n";
   }
   print STDERR "$0 $version  wesley ebisuzaki\n";
   print STDERR " makes a Grads control file for grib files\n";
   print STDERR " usage: $0 [options] [grib file] [optional index file] >[ctl file]\n";
   print STDERR " -ncep_opn       .. use NCEP opn grib table for T62 NCEP fields\n";
   print STDERR " -ncep_rean      .. use NCEP reanalysis grib table for T62 NCEP fields\n";
   print STDERR " -verf           .. use forecast verification times\n";
   print STDERR " -no_prs         .. no prs suffix on variable name\n";
   print STDERR " -no_suffix      .. no suffix on variable name\n";
   print STDERR " -rev_z          .. for reversed vertical coordinates like theta\n";
   print STDERR " -365            .. 365 day calendar\n";
   print STDERR " -ts[timestep]   .. set timestep for individual time files (e.g. -ts6hr)\n";
   print STDERR " -lc             .. set lowercase option for parameter names\n";
   print STDERR " -eta            .. ETA model levels\n";
   print STDERR " -gsf            .. GSF (MRF) model level (default)\n";
   print STDERR " -noah           .. NOAH model levels\n";
   print STDERR " -osu            .. OSU model levels (default)\n";
   print STDERR " -ruc            .. RUC levels\n";
   exit 8;
}

$_ = $file;
if (/%y4/ || /%y2/ || /%m2/ || /%m1/ || /%d2/ || /%d1/ || /%h2/ ||
    /%h1/ || /%f2/ || /%f3/) { $template='on'; }

if (-d "c:\\") {
   $ListA="c:\\g$$.tmp";
   $TmpFile="c:\\h$$.tmp";
   unlink ($ListA, $TmpFile);
   $sys="win";
}
else {
   $ListA="/tmp/g$$.tmp";
   $TmpFile="/dev/null";
   unlink $ListA;
   $sys="unix";
}

# ctlfilename = name used by control file (different for template option(
# file = file name (of first matching file(

$ctlfilename=$file;

# inventory of All records
if ($template eq "on") {
   $gfile=$file;

   if ($sys eq 'win') {
      $gfile =~ s=\\=/=g;
   }
   $gfile =~ s/%y4/\\d{4}/g;
   $gfile =~ s/%y2/\\d{2}/g;
   $gfile =~ s/%m2/\\d{2}/g;
   $gfile =~ s/%m1/\\d{1,2}/g;
   $gfile =~ s/%d2/\\d{2}/g;
   $gfile =~ s/%d1/\\d{1,2}/g;
   $gfile =~ s/%h2/\\d{2}/g;
   $gfile =~ s/%h1/\\d{1,2}/g;
   $gfile =~ s/%h3/\\d{3}/g;
   $gfile =~ s/%f2/\\d{2,3}/g;
   $gfile =~ s/%f3/\\d{3}/g;
   $dir=$gfile;
   $dir =~ s=(/*)[^/]*$=$1=;

   if ($dir eq "") {
      opendir(DIR,'.');
   }
   else {
      opendir(DIR,$dir);
   }
   @allfiles = sort grep /^$gfile$/, readdir DIR;
   closedir DIR;
   $ntime=$#allfiles + 1;
   if ($ntime <= 0) {
      print STDERR "\nError: could not find any files in directory: $dir\n";
      exit 8;
   }
   $file="$dir$allfiles[0]";
   if ($sys eq 'win') {
      $file =~ s=/=\\=g;
   }
#  make inventory of first two files in order to get dt right
   system "$wgrib $wflag -v $dir$allfiles[0] >$ListA";
   system "$wgrib $wflag -v $dir$allfiles[1] >>$ListA";
}
else {
   system "$wgrib $wflag -v $file >$ListA";
}

if ( ! -s $ListA ) {
    print STDERR "Big problem:\n";
    print STDERR "  either $file is missing or not a grib file\n";
    print STDERR "  or wgrib is not on your path\n";
    exit 8;
}

# make table of dates and variables

open (FileDate, "<$ListA");
while (defined($_ = <FileDate>)) {

   # date table

   $_ =~ s/^.*D=//;
   $d=substr($_, 0, 10);
   $dates{$d}="";

   # variable/level list
   @Fld = split(':', $_, 99);
   $kpds=substr($Fld[3],5);
   ($kpds5,$kpds6,$kpds7) = split(/,/,$kpds);
   $varname = "$Fld[1]:$kpds6";
   if (defined $flevels{$varname}) {
      if (!($flevels{$varname} =~ / $kpds7 /)) {
         $flevels{$varname} .= "$kpds7 ";
      }
   }
   else {
      $flevels{$varname} = " $kpds7 ";
      $fcomments{$varname} = "$kpds5:$Fld[$#Fld]";
   }
}
close (FileDate);
@sdates=sort keys(%dates);


if ($template eq "") {
   $ntime=$#sdates + 1;
}
$time=$sdates[0];
$year = substr($time,0,4);
$mo = substr($time,4,2);
$day = substr($time,6,2);
$hour = substr($time,8,2);

if ($mo < 0 || $mo > 12) {
   print "illegal date code $time\n";
   exit 8;
}

$month=substr("janfebmaraprmayjunjulaugsepoctnovdec",$mo*3-3,3);

if ($ntime > 1) {
    $year1 = substr($sdates[1],0,4);
    $mo1 = substr($sdates[1],4,2);
    $day1 = substr($sdates[1],6,2);
    $hour1 = substr($sdates[1],8,2);
}

# ---------------intro------------------------------------

if ("$index" eq "" ) {$index="$file.idx";}
if ($sys eq "unix") {
   $caret1 = (substr($file,0,1) eq "/") ? "" : '^';
   $caret2 = (substr($index,0,1) eq "/") ? "" : '^';
}
else {
   $caret1 = (substr($file,1,1) eq ":") ? "" : '^';
   $caret2 = (substr($index,1,1) eq ":") ? "" : '^';
}

print "dset $caret1$ctlfilename\nindex $caret2$index\n";

print "undef 9.999E+20\ntitle $file\n*  produced by grib2ctl v$version\n";

# ------------------- grid -----------------------
$griddef = `$wgrib $wflag -V $file -d 1 -o $TmpFile`;
$_=$griddef;

/ center (\S*) /;
$center=$1;
/ grid=(\S*) /;
$grid=$1;

print "dtype grib $grid\n";
if ($template eq "on") {
   print "options template\n";
}

if (/  latlon: /) {
   / lat  (\S*) to (\S*) by (\S*) /;
   $lat0=$1;
   $lat1=$2;
   $dlat=$3;

   / long (\S*) to (\S*) by (\S*), \((\S*) x (\S*)\)/;
   $lon0=$1;
   # $lon1=$2;
   $dlon=$3;
   $nx =$4;
   $ny =$5;

   if ($lat0 > $lat1) {
      print "options yrev\n";
      print "ydef $ny linear $lat1 ", abs($dlat), "\n"
   }
   else {
      print "ydef $ny linear $lat0 ", abs($dlat), "\n"
   }
   print "xdef $nx linear $lon0 $dlon\n";
}
elsif ($grid == 5 && $center == 7) {
   print "pdef 53 57 nps 27 49 -105 190.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 6 && $center == 7) {
   print "pdef 53 45 nps 27 49 -105 190.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 87 && $center == 7) {
   print "pdef 81 62 nps 31.9 112.53 -105 68.513\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 96 && $kludge eq "on" && $center == 7) {
   # quick and dirty pdef for eta 12 km
   # use -kludge option
   print "pdef 606 1067 eta.u -111 50 0.17520661  0.075046904\n";
   print "xdef 1440 linear -200 0.125\n";
   print "ydef 721 linear 0 0.125\n";
}
elsif ($grid == 101 && $center == 7) {
   print "pdef 113 91 nps 58.5 92.5 -105 91.452\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 104 && $center == 7) {
   print "pdef 147 110 nps 75.5 109.5 -105 90.75464\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 105 && $center == 7) {
   print "pdef 83 83 nps 40.5 88.5  -105 90.75464\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 106 && $center == 7) {
   print "pdef 165 117 nps 80 176 -105 45.37732\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 107 && $center == 7) {
   print "pdef 120 92 nps 46 167  -105 45.37732\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 192 && $rr ne "" && $center == 7) {
   # quick and dirty pdef for RR egrid
   # need -rr option to use
   print "pdef 237 387 eta.u -111 50 0.4491525  0.2072539\n";
   print "xdef 360 linear -160 0.5\n";
   print "ydef 181 linear 0 0.5\n";
}
elsif ($grid == 201 && $center == 7) {
   print "pdef 65 65 nps 33 33 -105 381\n";
   print "xdef 180 linear -180 2\n";
   print "ydef 51 linear -10 2\n";
}
elsif ($grid == 202 && $center == 7) {
   print "pdef 65 43 nps 33 45 -105 190.5\n";
   print "xdef 91 linear -200 2\n";
   print "ydef 41 linear 10 2\n";
}
elsif ($grid == 203 && $center == 7) {
   print "pdef  45 39 nps 27 37 -150 190.5\n";
   print "xdef 103 linear -250 2\n";
   print "ydef 33 linear 26 2\n";
}
elsif ($grid == 205 && $center == 7) {
   print "pdef  45 39 nps 27 57 -60 190.5\n";
   print "xdef 50 linear -120 2\n";
   print "ydef 46 linear 0 2\n";
}
elsif ($grid == 207 && $center == 7) {
   print "pdef 49 35 nps 25 51 -150 95.25\n";
   print "xdef 51 linear -200 2\n";
   print "ydef 30 linear 45 1\n";
}
elsif ($grid == 211 && $center == 7) {
   # awips lambert conformal
   print "pdef 93 65 lcc 12.19 -133.459 1 1 25 25 -95 81270.5 81270.5\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
elsif ($grid == 212 && $center == 7) {
   # awips lambert conformal
   print "pdef 185 129 lcc 35.0 -95.0 105 49 25 25 -95 40635 40635\n";
   print "xdef 181 linear -140 0.5\n";
   print "ydef  91 linear  15 0.5\n";
}
elsif ($grid == 213 && $center == 7) {
   print "pdef  129 85 nps 65 89 -105 95.25\n";
   print "xdef 170 linear -190 1\n";
   print "ydef 81 linear 10 1\n";
}
elsif ($grid == 215 && $center == 7) {
   # lambert conformal
   print "pdef 369 257 lcc 12.19 -133.46 1 1 25 25 -95 20318 20318\n";
   print "xdef 289 linear -136 0.25\n";
   print "ydef  157 linear 18 0.25\n";
}
elsif ($grid == 216 && $center == 7) {
   print "pdef 147 110 nps 75.5 109.5 -105 91.452\n";
   print "xdef 181 linear -180 1\n";
   print "ydef 91 linear 0 1\n";
}
elsif ($grid == 221 && $nearest_neighbor eq "on" && $center == 7) {
   # awips lambert conformal nearest neighbor
   print "pdef 96673 1 file 1 sequential binary-little $pdef_dir/grib221nn.pdef\n";
   print "xdef 1111 linear -250 0.333333\n";
   print "ydef 247 linear 8 0.333333\n";
}
elsif ($grid == 221 && $center == 7) {
   # awips lambert conformal
   print "pdef 349 277 lcc 1 -145.5 1 1 50 50 -107 32463 32463\n";
   print "xdef 1111 linear -250 0.333333\n";
   print "ydef 247 linear 8 0.333333\n";
}
# old grid 240
#elsif ($grid == 240) {
#   # nps usa
#   print "pdef 1160 880 nps 441 1601 255 4.763\n";
#   print "xdef 801 linear -130 0.1\n";
#   print "ydef 401 linear 20 0.1\n";
#}
# new grid 240
elsif ($grid == 240 && $center == 7) {
   # nps usa
   print "pdef 1121 881 nps 401 1601 255 4.7625\n";
   print "xdef 1601 linear -130 0.05\n";
   print "ydef 801 linear 20 0.05\n";
}
elsif ($grid == 241 && $center == 7) {
   print "pdef 386 293 nps 147.315 534.0 -105 14.2875\n";
   print "xdef 161 linear -140 0.5\n";
   print "ydef 81 linear 20 0.5\n";
}
else {
   # unknown grid

   $_ = $griddef;
   GRD: {

      / polar stereo: Lat1 16.125000 Long1 234.983000 Orient -100.0/ && do {
         print "options yrev\n";
         print "pdef 129 86 nps 64 136 -100 60\n";
         print "xdef 720 linear  0 0.5\n";
         print "ydef 148 linear 16 0.5\n";
	 last GRD; };

      / polar stereo: Lat1 -4.860000 Long1 -122.614000 Orient -80.000000/ && do {
         print "pdef 49 51 nps 24 26 -80 381\n";
         print "xdef 144 linear 0 2.5\n";
         print "ydef 45 linear -20 2.5\n";
         last GRD; };

      / Lambert Conf:.* Lov 265.*\(151 x 113\)/s && do {
         print "options yrev\n";
         print "pdef 151 113 lcc 16.281 233.8622 1 1 25 25 265 40635 40635\n";
         print "xdef 141 linear -130 0.5\n";
         print "ydef 71 linear 20 0.5\n";
         last GRD; };

      # beta: mercator 
      # scan modes .. assumes west to east
      / Mercator: / && do {
	/lat  *(\S*) to (\S*) /;
        $lat1 = $1;
        $lat2 = $2;
	/long (\S*) to (\S*) /;
        $lon1 = $1;
        $lon2 = $2;
	/ nx (\S*) ny (\S*) /;
	$nx = $1;
	$ny = $2;

	if ($lat1 > $lat2) {
	    print "options yrev\n";
	    $t = $lat2;
	    $lat2 = $lat1;
	    $lat1 = $t;
	}
	print "ydef $ny levels\n";
	$i = 0;
	$n1 = log(tan((45+$lat1/2)*3.1415927/180));
	$n2 = log(tan((45+$lat2/2)*3.1415927/180));
	$dy = ($n2 - $n1) / ($ny - 1);

	while ($i < $ny) {
	    $nn = $n1 + $dy * $i;
            $lat = (atan(exp($nn))*180/3.1415927-45)*2;
	    printf ("%9.4f ", $lat);
	    $i++;
	    if ($i % 7 == 0) { print "\n"; }
	}
	if ($i % 7 != 0) { print "\n"; }

	$dlon = $lon2 - $lon1;
	if ($dlon < 0) {
	    $dlon = $dlon + 360;
	}
	$dlon = $dlon / ($nx - 1);
	print "xdef $nx linear $lon1 $dlon\n";

	last GRD; };


      # beta: generalized lambert conformal pdef/xdef/ydef
      #  not very good .. needs to calculate the all
      #  vertices for better xdef and ydef
      #  for improvements .. pull out code from lcgrib

      / Lambert Conf: / && do {
	/ Lat1 (\S*) Lon1 (\S*) Lov (\S*)/;
	$lat1 = $1;
	$lon1 = $2;
	$lov = $3;

	/Latin1 (\S*) Latin2 (\S*) /;
	$latin1 = $1;
	$latin2 = $2;

	/Pole \((\S*) x (\S*)\) Dx (\S*) Dy (\S*) /;
	$nx = $1;
	$ny = $2;
	$dx = 1000*$3;
	$dy = 1000*$4;

	print "pdef $nx $ny lcc $lat1 $lon1 1 1 $latin1 $latin2 $lov $dx $dy\n";
        if ($global_map eq "") {
            $dx = $dx / (110000.0 * cos($lat1*3.141592654/180.0));
	    $dy = $dy / 110000.0;
	    if ($lon1 > 180) {
		$lon1 = $lon1 - 360.0;
	    }
	    print "xdef $nx linear $lon1 $dx\n";
	    print "ydef $ny linear $lat1 $dy\n";
	}
	else {
	    print "xdef 360 linear 0 1\n";
	    print "ydef 181 linear -90 1\n";
	}

	last GRD; };

      /  gaussian:/ && do {
         / lat  (\S*) to (\S*)/;
         $lat0=$1;
         $lat1=$2;

         / long (\S*) to (\S*) by (\S*), \((\S*) x (\S*)\)/;
         $lon0=$1;
         # $lon1=$2;
         $dlon=$3;
         $nx =$4;
         $ny =$5;
         $dlon = 360 / $nx;

         if ($lat0 > $lat1) {
            print "options yrev\n";
         }
         print "xdef $nx linear $lon0 $dlon\n";
         print "ydef $ny levels\n";

	$eps = 3e-14;
	$m=int(($ny+1)/2);

	$i=1;
	while ($i <= $m) {
		$z=cos(3.141592654*($i-0.25)/($ny+0.5));
		do {
			$p1 = 1;
			$p2 = 0;
			$j = 1;
			while ($j <= $ny) {
				$p3 = $p2;
				$p2 = $p1;
				$p1=((2*$j-1)*$z*$p2-($j-1)*$p3)/$j;
				$j++;
			}
			$pp = $ny*($z*$p1-$p2)/($z*$z-1);

			$z1 = $z;
			$z = $z1 - $p1/$pp;
		} until abs($z-$z1) < $eps;
		$x[$i] = -atan2($z,sqrt(1-$z*$z))*180/3.141592654;
		$x[$ny+1-$i] = -$x[$i];
		$i++;
	}
	$i = 1;
	while ($i < $ny) {
		printf  " %7.3f", $x[$i];
		if (($i % 10) == 0) { print "\n"; }
		$i++;
	}
	printf " %7.3f\n", $x[$ny];

         last GRD; };
#     rotated LatLon grid
      / rotated LatLon grid/ && do {
         / LatLon grid  lat (\S*) to (\S*)  lon (\S*) to (\S*)/;
         $lat0 = $1;
         $lat1 = $2;
         $lon0 = $3;
         $lon1 = $4;
         /nxny \S+  \((\S*) x (\S*)\)/;
         $nx = $1;
         $ny = $2;
         / south pole lat (\S*) lon (\S*)  rot angle (\S*)/;
         $lat_sp = $1;
         $lon_sp = $2;
         $rot_angle = $3;
         print "* Rotated LatLon grid: South pole lat $lat_sp lon $lon_sp",
               "  rot angle $rot_angle\n";
         $dlon = ( $lon1-$lon0)/($nx-1);
         $dlat = ( $lat1-$lat0)/($ny-1);
         if ($lat0 > $lat1) {
            print "options yrev\n";
            print "ydef $ny linear $lat1 ", abs($dlat), "\n"
         }
         else {
            print "ydef $ny linear $lat0 ", abs($dlat), "\n"
         }
         print "xdef $nx linear $lon0 $dlon\n";
         last GRD; };

#     polar stereographic
     /  polar stereo: / && do {
        / Lat1 (\S*) Long1 (\S*) Orient (\S*)/;
        $lat1=$1;
        $lon1=$2;
        $orient=$3;

        / (\S*) pole \((\S*) x (\S*)\) Dx (\S*) Dy (\S*) scan (\S*)/;
        $pole=$1;
        $nx=$2;
        $ny=$3;
        $dx=$4;
        $dy=$5;
        $scan=$6;

        # probably only works for scan=64

        $dpr=3.14159265358979/180.0;
        $rearth=6.3712e6;

        $h=1;
        $proj="nps";
        if ($pole eq "south") {
           $h=-1;
           $proj="sps";
        }
        $hi=1;
        $hj=-1;
        if (($scan/128 % 2) == 1) {
           $hi=-1;
        }
        if (($scan/64 % 2) == 1) {
           $hj=1;
        }
        $dxs=$dx*$hi;
        $dys=$dy*$hj;
        $de=(1+sin(60*$dpr))*$rearth;
        $dr=$de*cos($lat1*$dpr)/(1+$h*sin($lat1*$dpr));
        $xp=1-$h*sin(($lon1-$orient)*$dpr)*$dr/$dxs;
        $yp=1+cos(($lon1-$orient)*$dpr)*$dr/$dys;
        $dx=$h*$dx/1000;

        printf "pdef $nx $ny $proj $xp $yp $orient $dx\n";

        # need to do a better job here
        # need to find lat/lon of end points to right domain
        # for now, just do simple stuff
        $dx=abs($dx)*1000;
        $nx = int($rearth * 3.14 / $dx + 1);
        $dx = 360/$nx;
        printf "xdef $nx linear 0 $dx\n";
        if ($proj eq 'sps') {
           $ny=int(($lat1+90)/$dx)+1;
           printf "ydef $ny linear -90 $dx\n",
        }
        else {
           $ny=int((90-$lat1)/$dx)+1;
           $l=90-($ny-1)*$dx;
           printf "ydef $ny linear $l $dx\n",
        }
        last GRD; };


      print STDERR "*** script needs to be modified ***\n";
      print STDERR "unknown user-defined grid\n";
   }
}


# make the tdef statement

&tdef;

# ------------------var-------------------------------------;

%tails =(
   '1' => 'sfc',
   '2' => 'clb',
   '3' => 'clt',
   '4' => 'zdg',
   '5' => 'adcl',
   '6' => 'mwl',
   '7' => 'trp',
   '8' => 'toa',
   '9' => 'bos',
   '10' => 'clm',
   '12' => 'lcb',
   '13' => 'lct',
   '14' => 'loc',
   '22' => 'mcb',
   '23' => 'mct',
   '24' => 'mdc',
   '32' => 'hcb',
   '33' => 'hct',
   '34' => 'hic',
   '100' => 'prs',
   '101' => 'plr',
   '102' => 'msl',
   '103' => 'hml',
   '104' => 'zlr',
   '105' => 'hag',
   '106' => 'hlr',
   '107' => 'sig',
   '108' => 'slr',
   '109' => 'hbl',
   '110' => 'blr',
   '111' => 'dpl',
   '112' => 'dlr',
   '113' => 'tht',
   '114' => 'tlr',
   '116' => 'plg',
   '121' => 'plr',
   '126' => 'pa',
   '128' => 'slr',
   '141' => 'plr',
   '160' => 'dsl',
   '200' => 'clm',
   '204' => 'htfl',
   '206' => 'gcbl',
   '207' => 'gctl',
   '209' => 'bcb',
   '210' => 'bct',
   '211' => 'bcl',
   '212' => 'lcb',
   '213' => 'lct',
   '214' => 'lcl',
   '222' => 'mcb',
   '223' => 'mct',
   '224' => 'mcl',
   '232' => 'hcb',
   '233' => 'hct',
   '234' => 'hcl',
   '242' => 'cvb',
   '243' => 'cvt',
   '244' => 'cvl',
   );
$tails{'100'} = "$prs";

$nlevelmax=0;
$levelsmax=0;

$nvar=0;
foreach $fname (sort keys(%flevels)) {
   ($name, $kpds6) = split(/:/, $fname);
   ($kpds5, $comment) = split(/:/, $fcomments{$fname});
   $comment = substr($comment,1);

   #
   # find number of levels
   #

   $_=$flevels{$fname};
   $nlev = (tr/ / /) - 1;
   $kpds7s = $_;

   # fix names to be grads compatible
   # eliminate dashes, underscores, blanks and put no in front of leading digits

   $_ = $name;
   $_ =~ tr/_\- //d;
   if ($lc) { $_ =~ s/(.*)/\L$1/gi; }
   if ( /^\d/ ) { $_ = "no$_"; }
   $name = $_;
   
   $tail = $suffix eq 'no' ? "" : $tails{$kpds6};
   if (! defined $tail) { $tail="l$kpds6"; }

   # tranlate special levels
   if ($kpds6 == 1) {
      # $var_line[$nvar++]="${name}sfc  0 $kpds5,$kpds6,0 ** surface $comment";
      $kpds7s =~ s/^ //;
      $var_line[$nvar++]="${name}sfc  0 $kpds5,$kpds6,$kpds7s ** surface $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 2) {
      $var_line[$nvar++]="${name}clb  0 $kpds5,$kpds6,0 ** cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 3) {
      $var_line[$nvar++]="${name}clt  0 $kpds5,$kpds6,0 ** cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 4) {
      $var_line[$nvar++]="${name}0deg  0 $kpds5,$kpds6,0 ** 0C isotherm level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 5) {
      $var_line[$nvar++]="${name}adcl  0 $kpds5,$kpds6,0 ** adiabatic lifting condensation level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 6) {
      $var_line[$nvar++]="${name}mwl  0 $kpds5,$kpds6,0 ** max wind level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 7) {
      $var_line[$nvar++]="${name}trp  0 $kpds5,$kpds6,0 ** tropopause $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 8) {
      $var_line[$nvar++]="${name}toa  0 $kpds5,$kpds6,0 ** top of atmos $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 200) {
      $var_line[$nvar++]="${name}clm  0 $kpds5,$kpds6,0 ** atmos column $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 204) {
      $var_line[$nvar++]="${name}htfl  0 $kpds5,$kpds6,0 ** highest trop freezing level $comment";             
      $nlev=0; 
   }
   elsif ($kpds6 == 206) {
      $var_line[$nvar++]="${name}gcbl  0 $kpds5,$kpds6,0 ** grid-scale cloud bottom level $comment";             
      $nlev=0; 
   }
   elsif ($kpds6 == 207) {
      $var_line[$nvar++]="${name}gctl  0 $kpds5,$kpds6,0 ** grid-scale cloud top level $comment";             
      $nlev=0; 
   }
   elsif ($kpds6 == 209) {
      $var_line[$nvar++]="${name}bcb  0 $kpds5,$kpds6,0 ** boundary cld base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 210) {
      $var_line[$nvar++]="${name}bct  0 $kpds5,$kpds6,0 ** boundary cld top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 211) {
      $var_line[$nvar++]="${name}bcl  0 $kpds5,$kpds6,0 ** boundary cld layer $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 212) {
      $var_line[$nvar++]="${name}lcb  0 $kpds5,$kpds6,0 ** low cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 213) {
      $var_line[$nvar++]="${name}lct  0 $kpds5,$kpds6,0 ** low cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 214) {
      $var_line[$nvar++]="${name}lcl  0 $kpds5,$kpds6,0 ** low cloud level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 222) {
      $var_line[$nvar++]="${name}mcb  0 $kpds5,$kpds6,0 ** mid-cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 223) {
      $var_line[$nvar++]="${name}mct  0 $kpds5,$kpds6,0 ** mid-cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 224) {
      $var_line[$nvar++]="${name}mcl  0 $kpds5,$kpds6,0 ** mid-cloud level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 232) {
      $var_line[$nvar++]="${name}hcb  0 $kpds5,$kpds6,0 ** high cloud base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 233) {
      $var_line[$nvar++]="${name}hct  0 $kpds5,$kpds6,0 ** high cloud top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 234) {
      $var_line[$nvar++]="${name}hcl  0 $kpds5,$kpds6,0 ** high cloud level $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 242) {
      $var_line[$nvar++]="${name}cvb  0 $kpds5,$kpds6,0 ** convective cld base $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 243) {
      $var_line[$nvar++]="${name}cvt  0 $kpds5,$kpds6,0 ** convective cld top $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 244) {
      $var_line[$nvar++]="${name}cvl  0 $kpds5,$kpds6,0 ** convective cld layer $comment";
      $nlev=0;
   }
   elsif ($kpds6 == 246) {
      $var_line[$nvar++]="${name}cvl  0 $kpds5,$kpds6,0 ** max e-pot-t level $comment";
      $nlev=0;
   }


   elsif ($kpds6 == 101) {
      if ($kpds7s =~ s/ 12900 / /) {
         $var_line[$nvar++]="${name}500_1000mb 0 $kpds5,$kpds6,12900 ** 500-1000 mb $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 70 / /) {
         $var_line[$nvar++]="${name}toa_700mb 0 $kpds5,$kpds6,70 ** TOA-700 mb $comment";
         $nlev--;
      }

   }
   elsif ($kpds6 == 103) {
      if ($kpds7s =~ s/ 1829 / /) {
         $var_line[$nvar++]="${name}1829m  0 $kpds5,$kpds6,1829 ** 1829 m $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 2743 / /) {
         $var_line[$nvar++]="${name}2743m  0 $kpds5,$kpds6,2743 ** 2743 m $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 3658 / /) {
         $var_line[$nvar++]="${name}3658m  0 $kpds5,$kpds6,3658 ** 3658 m $comment";
         $nlev--;
      }
   }
   elsif ($kpds6 == 105) {
      if ($kpds7s =~ s/ 2 / /) {
         $var_line[$nvar++]="${name}2m  0 $kpds5,$kpds6,2 ** 2 m $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 10 / /) {
         $var_line[$nvar++]="${name}10m  0 $kpds5,$kpds6,10 ** 10 m $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 30 / /) {
         $var_line[$nvar++]="${name}30m  0 $kpds5,$kpds6,30 ** 30 m $comment";
         $nlev--;
      }
   }
   elsif ($kpds6 == 106) {
      if ($kpds7s =~ s/ 7680 / /) {
         $var_line[$nvar++]="${name}0_3000m  0 $kpds5,$kpds6,7680 ** 3000-0 m above ground $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 15360 / /) {
         $var_line[$nvar++]="${name}0_6000m  0 $kpds5,$kpds6,15360 ** 6000-0 m above ground $comment";
         $nlev--;
      }
   }
   elsif ($kpds6 == 107 && ($kpds7s =~ s/ 9950 / /) && $nlev < 5) {
      $var_line[$nvar++]="${name}sig995  0 $kpds5,$kpds6,9950 ** sig=.995 $comment";
      $nlev--;
   }
   elsif ($kpds6 == 108) {
      if ($kpds7s =~ s/ 11364 / /) {
         $var_line[$nvar++]="${name}sg44_100  0 $kpds5,$kpds6,11364 ** sigma=0.44-1.00 layer $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 18526 / /) {
         $var_line[$nvar++]="${name}sg72_94  0 $kpds5,$kpds6,18526 ** sigma=0.72-0.94 layer $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 11336 / /) {
         $var_line[$nvar++]="${name}sg44_72  0 $kpds5,$kpds6,11336 ** sigma=0.44-0.72 layer $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 8548 / /) {
         $var_line[$nvar++]="${name}sg33_100  0 $kpds5,$kpds6,8548 ** sigma=0.33-1.00 layer $comment";
         $nlev--;
      }
   }
   elsif ($kpds6 == 116) {
      if ($kpds7s =~ s/ 7680 / /) {
         $var_line[$nvar++]="${name}30_0mb  0 $kpds5,$kpds6,7680 ** 30-0 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 15390 / /) {
         $var_line[$nvar++]="${name}60_30mb  0 $kpds5,$kpds6,15390 ** 60-30 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 23100 / /) {
         $var_line[$nvar++]="${name}90_60mb  0 $kpds5,$kpds6,23100 ** 90-60 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 30810 / /) {
         $var_line[$nvar++]="${name}120_90mb  0 $kpds5,$kpds6,30810 ** 120-90 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 38520 / /) {
         $var_line[$nvar++]="${name}150_120mb  0 $kpds5,$kpds6,38520 ** 150-120 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 46080 / /) {
         $var_line[$nvar++]="${name}180_0mb  0 $kpds5,$kpds6,46080 ** 180-0 mb above gnd $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 46230 / /) {
         $var_line[$nvar++]="${name}180_150mb  0 $kpds5,$kpds6,46230 ** 180-150 mb above gnd $comment";
         $nlev--;
      }
   }
   elsif ($kpds6 == 117) {
      if ($kpds7s =~ s/ 2 / /) {
         $var_line[$nvar++]="${name}pv2  0 $kpds5,$kpds6,2 ** pot vorticity = 2 units level $comment";
         $nlev--;
      }
      if ($kpds7s =~ s/ 32770 / /) {
         $var_line[$nvar++]="${name}pvneg2  0 $kpds5,$kpds6,32770 ** pot vorticity = -2 units level $comment";
         $nlev--;
      }
   }

   if ($model eq "ETA") {
      if ($kpds6 == 109 && ($kpds7s =~ s/ 1 / /)) {
         $var_line[$nvar++]="${name}hlev1  0 $kpds5,$kpds6,1 ** hybrid level 1 $comment";
         $nlev--;
      }
      if ($kpds6 == 109 && ($kpds7s =~ s/ 2 / /)) {
         $var_line[$nvar++]="${name}hlev1  0 $kpds5,$kpds6,2 ** hybrid level 2 $comment";
         $nlev--;
      }
   }
   if ($model eq "RUC") {
      if ($kpds6 == 111) {
         if ($kpds7s =~ s/ 5 / /) {
            $var_line[$nvar++]="${name}5cm  0 $kpds5,$kpds6,5 ** 5 cm underground $comment";
            $nlev--;
         }
         if ($kpds7s =~ s/ 10 / /) {
            $var_line[$nvar++]="${name}10cm  0 $kpds5,$kpds6,10 ** 10 cm underground $comment";
            $nlev--;
         }
         if ($kpds7s =~ s/ 20 / /) {
            $var_line[$nvar++]="${name}20cm  0 $kpds5,$kpds6,20 ** 20 cm underground $comment";
            $nlev--;
         }
         if ($kpds7s =~ s/ 40 / /) {
            $var_line[$nvar++]="${name}40cm  0 $kpds5,$kpds6,40 ** 40 cm underground $comment";
            $nlev--;
         }
         if ($kpds7s =~ s/ 160 / /) {
            $var_line[$nvar++]="${name}160cm  0 $kpds5,$kpds6,160 ** 160 cm underground $comment";
            $nlev--;
         }
         if ($kpds7s =~ s/ 300 / /) {
            $var_line[$nvar++]="${name}300cm  0 $kpds5,$kpds6,300 ** 300 cm underground $comment";
            $nlev--;
         }
      }
   }
   if ($soil_model eq "NOAH") {
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10 / /)) {
         $var_line[$nvar++]="${name}0_10cm  0 $kpds5,$kpds6,10 ** 0-10 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 100 / /)) {
         $var_line[$nvar++]="${name}0_100cm  0 $kpds5,$kpds6,100 ** 0-100 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 200 / /)) {
         $var_line[$nvar++]="${name}0_200cm  0 $kpds5,$kpds6,200 ** 0-200 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 2600 / /)) {
         $var_line[$nvar++]="${name}10_40cm  0 $kpds5,$kpds6,2600 ** 10-40 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10340 / /)) {
         $var_line[$nvar++]="${name}40_100cm  0 $kpds5,$kpds6,10340 ** 40-100 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 25800 / /)) {
         $var_line[$nvar++]="${name}100_200cm  0 $kpds5,$kpds6,25800 ** 100-200 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 111 && ($kpds7s =~ s/ 800 / /)) {
         $var_line[$nvar++]="${name}_800cm  0 $kpds5,$kpds6,25800 ** 800 cm undergnd $comment";
         $nlev--;
      }

   }
   if ($soil_model eq "OSU") {
      if ($kpds6 == 111 && ($kpds7s =~ s/ 300 / /)) {
         $var_line[$nvar++]="${name}SoilB  0 $kpds5,$kpds6,300 ** 300 cm underground $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 10 / /)) {
         $var_line[$nvar++]="${name}SoilT  0 $kpds5,$kpds6,10 ** 0-10 cm undergnd $comment";
         $nlev--;
      }
      if ($kpds6 == 112 && ($kpds7s =~ s/ 2760 / /)) {
         $var_line[$nvar++]="${name}SoilM  0 $kpds5,$kpds6,2760 ** 10-200 cm undergnd $comment";
         $nlev--;
      }
   }

   if ($nlev == 1) {
      $kpds7s =~ s/^ //;
      $var_line[$nvar++]="$name$tail  0 $kpds5,$kpds6,$kpds7s ** $comment";
   }
   elsif ($nlev > 1) {
      $var_line[$nvar++]="$name$tail $nlev $kpds5,$kpds6,0 ** $comment";
      if ($nlev > $nlevelmax) {
         $nlevelmax=$nlev;
         $levelsmax=$flevels{$fname};
      }
   }
}

#------------------levels-------------------------;

if ($nlevelmax == 0) {
   print "zdef 1 linear 1 1\n";
}
else {

   ($_ = $levelsmax) =~ s/.//;
   chop($_);

   if ($z_order eq "theta") {
      @levels=sort {$a <=> $b} split(/ /,$_);
   }
   else {
      @levels=sort {$b <=> $a} split(/ /,$_);
   }

   print "zdef $nlevelmax levels\n";
   for ($i = 0; $i < $nlevelmax; $i++) {
      print "$levels[$i] ";
   }
   print "\n";
}

print "vars $nvar\n";
for ($i = 0; $i < $nvar; $i++) {
   print $var_line[$i];
}
print "ENDVARS\n";

if ($sys eq "win") {
   unlink $TmpFile;
}
unlink $ListA;
exit 0;

#------------------ jday --------------------
# jday(year,mo,day) return the julian day relative to jan 0
# mo=1..12
#
sub jday {

   local($n);
   $n=substr(" 000 031 059 090 120 151 181 212 243 273 304 334",($_[1]-1)*4,4);
   $n = $n + $_[2];

   if ($_[1] > 2 && $_[0] % 4 == 0) {
      if ($_[0] % 400 == 0 || $_[0] % 100 != 0) {
         $n++;
      }
   }
   $n;
}

#------------------ write tdef statement ------------------
# still not great but better than before

sub tdef {

   local($tmp);
   if ($timestep) { $dt=$timestep }
   else { 
      if ($ntime == 1) {
         if ($timestep) { $dt=$timestep }
         else { $dt="1mo"; }
      }
      elsif ($hour != $hour1) {
         # assume that dt < 24 hours
         $tmp=$hour1-$hour;
         ($tmp < 0) && do {$tmp = $tmp + 24};
         $dt="${tmp}hr";
      }
      elsif ($day != $day1) {
         # assume that dt < 365 days
         $tmp = &jday($year1,$mo1,$day1) - &jday($year,$mo,$day);
         ($tmp < 0) && do {$tmp = $tmp + &jday($year,12,31)};
         $dt="${tmp}dy";
      }
      elsif ($mo != $mo1) {
         # assume that dt < 12 months
         $tmp = $mo1 - $mo;
         ($tmp < 0) && do {$tmp = $tmp + 12};
         $dt="${tmp}mo";
      }
      else {
         $tmp = $year1 - $year;
         $dt="${tmp}yr";
      }
   }
   if ($calendar eq "365") {
       print "options 365_day_calendar\n";
   }
   print "tdef $ntime linear ${hour}Z$day$month$year $dt\n";
}
