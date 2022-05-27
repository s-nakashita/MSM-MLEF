#!/usr/bin/perl -w
#
# perl routine to get value for a lat-long point
# 11/99 w. ebisuzaki
#
# uses perlv5, wgrib, copygb
#
# routine uses copygb to create a small grib file that is
# on the grid point in question. then wgrib is run to
# extract the data
# 
$version="v0.99.1 11/99 w. ebisuzaki";


# parse arguments

$i=0;
$wgrib_flag="";
$debug="0";
while ($i <= $#ARGV) {
   $_ = $ARGV[$i];
   $i++;
   SWITCH: {
	/^-lat/ && do { $lat=$ARGV[$i++]; last SWITCH; };
	/^-lon/ && do { $lon=$ARGV[$i++]; last SWITCH; };
	/^-ncep_opn/ && do { $wgrib_flag="$wgrib_flag -ncep_opn"; last SWITCH; };
	/^-ncep_rean/ && do { $wgrib_flag="$wgrib_flag -ncep_rean"; last SWITCH; };
	/^-verf/ && do { $wgrib_flag="$wgrib_flag -verf"; last SWITCH; };
	/^-4yr/ && do { $wgrib_flag="$wgrib_flag -4yr"; last SWITCH; };
	/^-s/ && do { $wgrib_flag="$wgrib_flag -s"; last SWITCH; };
	/^-v/ && do { $wgrib_flag="$wgrib_flag -v"; last SWITCH; };
	/^-debug/ && do { $debug="1"; last SWITCH; };
	/^-V/ && do { print STDERR "-V not supported\n"; exit 8; };

        /^-/ && do { print STDERR "unknown option: $_\n"; exit 8; };
        if (defined($file)) {
           print STDERR "can only specify one file: $file or $_\n";
           exit 8;
        }
        else {
           $file="$_";
        }
   }
}

# cleanup values

if (!defined($lat) || !defined($lon) || !defined($file)) {
   print STDERR $version;
   print STDERR "\ngrbpoint.pl (-s|-v) (-verf) (-4yr) -lat [val] -lon [val] grib_file\n";
   print STDERR "  inventory style: (none) -s or -v\n";
   exit 8;
}

if ($lat =~ s/[sS]$//) {
   $lat=-$lat;
}
else {
   $lat =~ s/[nN]$//;
}

if ($lon =~ s/[wW]$//) {
   $lon=-$lon;
}
else {
   $lon =~ s/[eE]$//;
}
$lat=1.0*$lat;
$lon=1.0*$lon;

if ($lat > 90 || $lat < -90) {
   print STDERR "error: lat should be between -90 and 90 (90S and 90N)\n";
   exit 8;
}

$tmp1="/tmp/grbpoint.1.$$";
$tmp2="/tmp/grbpoint.2.$$";
$tmp3="/tmp/grbpoint.3.$$";

# GRIB works in milli-degrees of lat/lon

$lat0=1000*$lat;
if ($lat0 == 90000) { $lat0--; }
$lat1=$lat0+1;

$lon0=1000*$lon;
if ($lon0 == 360000) { $lon0=0; }
$lon1=$lon0+1;

$grid="255 0 2 2 $lat0 $lon0 128 $lat1 $lon1 1 1 64";

# convert to new 2x2 grid

system "copygb -g\"$grid\" -x $file $tmp1 1>/dev/null";
if ($debug == 1) {
  system "wgrib -V $tmp1";
  print "copygb -g\"$grid\" -x $file $tmp1 1>/dev/null\n";
}

# make inventory and text dump
system "wgrib -d all $wgrib_flag -text -o $tmp2 $tmp1 >$tmp3";

unlink $tmp1;

if ( ! -s $tmp3 ) {
   print STDERR "no GRIB data found\n";
   exit 8;
   unlink $tmp2;
   unlink $tmp3;
   exit 8;
}

open (Dump, "<$tmp2") || die "can't open $tmp2\n";
open (Inv, "<$tmp3") || die "can't open $tmp3\n";

while (<Inv>) {
   # not sure why but -v option needs following line
   if (length > 1) {
   chomp($_);
   <Dump>;
   $val=<Dump>;
   <Dump>;
   <Dump>;
   <Dump>;
   print "$_:lat=$lat:lon=$lon:value=$val";
   }
}
unlink ($tmp2, $tmp3);

