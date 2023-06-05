#!/usr/bin/perl -w

# F2PY specific case:
# F90 files which needs to be accessible from Python should have the _f2py.F90 suffix
# They are considered as F90 codes, so they are part of rttov F90 libraries
# f2py produces .so module files which are stored in the lib directory
# flags and options for f2py:
#   in Makefile.inc add:
#      EXT_SO=.so 
#   in build/arch/$ARCH add
#      F2PY=f2py --fcompiler=gnu95
#      F2PYFLAGS_ARCH="-fPIC -O3 -fopenmp"
#      F2PYLDFLAGS_ARCH=-lgomp

package F90;

use strict;
use FileHandle;
use File::Spec;

our @SUFFIX_F90 = qw( \.F90 \.F \.f90 \.f );


sub what {

#
# Guess what the file contains, subroutine, program, module, etc...
#

  my $code = shift;

  my $what;

  ( $code->[0] =~ m/SUBROUTINE/io ) && ( $what = 'Subroutine' );
  ( $code->[0] =~ m/FUNCTION/io )   && ( $what = 'Function' );
  ( $code->[0] =~ m/MODULE/io )     && ( $what = 'Module' );
  ( $code->[0] =~ m/PROGRAM/io )    && ( $what = 'Program' );

  return $what;

}

my %WHAT;

sub what_F90
{
  my $F90 = shift;
  $WHAT{$F90} ||= &what (&code ($F90));
  return $WHAT{$F90};
}


my %ABS_PATH;

BEGIN
{
  use File::Basename qw (basename);
  %ABS_PATH = map { (basename ($_), $_)  } <*/*.F90>;
}


sub lookup_abs_path
{
  my $F90 = shift;
  $ABS_PATH{$F90} or do 
    {
      print STDERR Data::Dumper::Dumper (\%ABS_PATH);
      die ("Cannot lookup absolute path of $F90");
    };
  return $ABS_PATH{$F90};
}


my %CODE; # global cache for source code

sub code {
  my $F90 = shift;

  unless( 'File::Spec'->file_name_is_absolute( $F90 ) ) {
    $F90 = 'File::Spec'->rel2abs( $F90 );
  }
  unless( $CODE{$F90} ) {

#
# Extracts the code from the F90 file
#

    ( my $fhcode = 'FileHandle'->new( "<$F90" ) )
      or die( "Cannot open <$F90\n" );

    
    my @code = <$fhcode>;

    @code = grep { ! ( m/^\s*[!\*]\s*(?!INTF)/o or m/^\s*$/o ) } @code;
    
    $CODE{$F90} = \@code;

  }

  return $CODE{$F90};
}

sub use {

#
#  Looks for modules used in this unit
#

  my $code = shift;
  my @code = @$code;

#  return grep { 
#                (!/yomlun|f90_unix_env|f90_unix_proc|f90_unix_errno|hdf5|h5lt/o)
#                && ($::COMPILE_DRHOOK ? 1 : !m/yomhook/o)
#              } grep { m/^\s*use\s+(\w+)\b/io; $_ = lc( $1 ) } @code;

  return grep { 
                (!/yomlun|f90_unix_env|f90_unix_proc|f90_unix_errno|hdf5|h5lt|iso_c_binding|mpi|netcdf/o)
                && ($::COMPILE_DRHOOK ? 1 : !m/yomhook/o)
                && ($::RTTOV_HDF ? 1 : !m/_hdf_/o)
                && ($::RTTOV_MKL ? 1 : !m/mkl/o)
                && ($::RTTOV_MPL ? 1 : !m/mpl/o)
              } grep { m/^\s*use\s+(\w+)\b/io; $_ = lc( $1 || '') }  @code;
}

my %USE;

sub use_F90
{
  my $F90 = shift;
  $USE{$F90} ||= [ &use (&code ($F90)) ];
  return @{ $USE{$F90} };
}

sub inc {

#
# Looks for includes
#

  my $code = shift;
  my @code = @$code;
 #  my @inc = grep { ($::RTTOV_HDF ? 1 : !m/_hdf_/o) } grep { m/^\s*#include\s*\"(.*)\.interface\"/io; my $b = $1; $_ = $b } @code;
  
  my @a = grep { ($::RTTOV_HDF ? 1 : !m/_hdf_/o) } grep { m/^\s*#include\s*\"(.*)\.interface\"/io; my $b = $1; $_ = $b } @code;
  my %seen = ();
  my @inc = grep { !$seen{$_}++ } @a; 
  
 return @inc;
}

my %INC;

sub inc_F90
{
  my $F90 = shift;
  $INC{$F90} ||= [ &inc (&code ($F90)) ];
  return @{ $INC{$F90} };
}



sub has_INTF {
  my $code = shift;
  return grep { m/^!INTF/o } @$code;
}

sub fileparse {
  my $F90 = shift;
  use File::Basename qw();
  my ( $f, $d, $s ) = &File::Basename::fileparse( $F90, @SUFFIX_F90 );
  $d = 'File::Spec'->canonpath( $d );
  return ( $f, $d, $s );
}

sub glob {
  use File::Glob qw();
  my $base = shift;
  # Exclude specific source files depending on options
  if ( $::RTTOV_USER_LAPACK ) {
    return grep { !/lapack.f/ && !/lapack_lbl.f/ } sort( map( { &File::Glob::bsd_glob("$base$_") } @SUFFIX_F90 ) );
  } else {
    return sort( map( { &File::Glob::bsd_glob("$base$_") } @SUFFIX_F90 ) );
  }
}

sub OBJ {
  return '$(OBJDIR)/' . (&F90::fileparse( $_[0] ))[0] . '$(EXT_OBJ)';
}

sub BIN {
  return '$(BINDIR)/' . (&F90::fileparse( $_[0] ))[0] . '$(EXT_BIN)';
}

sub MOD {
  return '$(MODDIR)/' . (&F90::fileparse( $_[0] ))[0] . '$(EXT_MOD)';
}

sub INTERFACE {
  return '$(INCDIR)/' . (&F90::fileparse( $_[0] ))[0] . '.interface';
}

sub SO {
  return '$(LIBDIR)/' . (&F90::fileparse( $_[0] ))[0] . '$(EXT_SO)';
}

sub PY_SO {
  my $ext = '*' . '$(EXT_SO)';
  return (&F90::fileparse( $_[0] ))[0] . $ext;
}


my %USE_REC;

sub use_rec_F90 {
  my $F90 = shift;
  unless ($USE_REC{$F90})
    {
      my @use = &use_F90 ($F90);
      my @use_inc = map { &use_F90 (&lookup_abs_path ("$_.F90")) } &inc_F90 ($F90);
      my %use = map { ($_, 1) } (@use, @use_inc);
      $USE_REC{$F90} = [ keys (%use) ];
    }
  return @{ $USE_REC{$F90} };
}

1;



package C;

use strict;
use FileHandle;
use File::Spec;

our @SUFFIX_C = qw( \.C \.c );

sub glob {
  use File::Glob qw();
  my $base = shift;
  return sort( map( { &File::Glob::bsd_glob("$base$_") } @SUFFIX_C ) );
}

sub fileparse {
  my $C = shift;
  use File::Basename qw();
  my ( $f, $d, $s ) = &File::Basename::fileparse( $C, @SUFFIX_C );
  $d = 'File::Spec'->canonpath( $d );
  return ( $f, $d, $s );
}

sub OBJ {
  return '$(OBJDIR)/' . (&C::fileparse( $_[0] ))[0] . '$(EXT_OBJ)';
}

1;

package F2PY;

use strict;
use FileHandle;
use File::Spec;

our @SUFFIX_F2PY = qw( \_f2py.F90 );

sub glob {
  use File::Glob qw();
  my $base = shift;
  return sort( map( { &File::Glob::bsd_glob("$base$_") } @SUFFIX_F2PY ) );
}

#sub fileparse {
  #my $C = shift;
  #use File::Basename qw();
  #my ( $f, $d, $s ) = &File::Basename::fileparse( $C, @SUFFIX_F2PY );
  #$d = 'File::Spec'->canonpath( $d );
  #return ( $f, $d, $s );
#}

1;

package main;
use strict;
use File::Basename;
use File::Copy;
use Data::Dumper;
use Cwd;


our %OPTIONS;

my $cwd = &cwd();

for (@ARGV)
  {
    if (m/^(\w+)=(.*)$/o)
      {
        $OPTIONS{$1} = $2;
      }
  }

my $verbose = $::OPTIONS{VERBOSE};
our $RTTOV_HDF = $::OPTIONS{RTTOV_HDF};
our $RTTOV_MKL = $::OPTIONS{RTTOV_MKL};
our $RTTOV_MPL = $::OPTIONS{RTTOV_MPL};
our $RTTOV_ECMWF = $::OPTIONS{RTTOV_ECMWF};
our $RTTOV_DEV = $::OPTIONS{RTTOV_DEV};
our $RTTOV_F2PY = $::OPTIONS{RTTOV_F2PY};
our $RTTOV_USER_LAPACK = $::OPTIONS{RTTOV_USER_LAPACK};
our $USE_DRHOOK_LIB = $::OPTIONS{USE_DRHOOK_LIB};
if ( $RTTOV_DEV ) {
  $RTTOV_HDF = 1;
  $RTTOV_ECMWF = 1;
}

if ( $USE_DRHOOK_LIB ) {
  if ( -s "main/yomhook.F90" ) {
    move ("main/yomhook.F90", "main/yomhook.txt") or die "move file main/yomhook.F90: $!";
    $verbose && print "use dr_hook library\n";
  }
} else {
  if ( -s "main/yomhook.txt"  &&  !-s "main/yomhook.F90" ) {
    move ("main/yomhook.txt", "main/yomhook.F90") or die "move file main/yomhook.txt: $!";
    $verbose && print "move main/yomhook.txt\n";
  }
}

our $COMPILE_DRHOOK = <*/yomhook.F90>;  # true if we compile drhook ourselves

my @F = &F90::glob( "*/*" );
my @P = &F2PY::glob( "*/*" );

my %deps;


for my $F ( @F ) {

  my ( $f, $d ) = &F90::fileparse( $F );
  
  if ( !$RTTOV_HDF ) {
    next if ( $d eq 'hdf' || $d eq 'gui' );
  }

  if ( !$RTTOV_ECMWF ) {
    next if ( $d eq 'coef_distribute' );
  }

  if ( !$RTTOV_DEV ) {
    next if ( $d =~ /^lbl/ || $d eq 'baran' || $d eq 'dev' );
  }

  my @use = &F90::use_F90( $F );
  my @inc = &F90::inc_F90( $F );

  $deps{$d} ||= { };

  for my $use ( @use ) {
    my ( $F1 ) = &F90::glob( "*/$use" ); # TODO check $F1 is the only one
    next unless( $F1 );
    my ( $f1, $d1 ) = &F90::fileparse( $F1 );

    if ($verbose)
      {
        print "$d depends on $d1 through $F use $F1\n";
      }

    $deps{$d}{$d1}++
      unless( $d eq $d1 );
  }

  for my $inc ( @inc ) {
    my ( $F1 ) = &F90::glob( "*/$inc" ); # TODO check $F1 is the only one
    unless( $F1 ) {
      print "$F: Cannot find include $inc.interface\n";
      next;
    }
    my ( $f1, $d1 ) = &F90::fileparse( $F1 );

    if ($verbose)
      {
        print "$d depends on $d1 through $F include $F1\n";
      }

    $deps{$d}{$d1}++
      unless( $d eq $d1 );
  }
}

#
# Build full dependency graph & detect circular deps
#

sub walk {
  my $sub = shift;
  my @sub = @_;
  if( grep { $sub eq $_ } @sub ) {
    die( "Circular dependency detected: " . 
     join( ' > ', reverse( @sub ), $sub ) . "\n" );
  }
  $deps{$_}{$sub}++ for( @sub );
  for ( keys( %{ $deps{$sub} } ) ) {
    &walk( $_, $sub, @sub );
  }
}

&walk( $_ ) for( keys( %deps ) );

#
# Simplify the graph using lists
#

while( my ( $sub, $dep ) = each( %deps ) ) {
  $deps{$sub} = [ keys(%$dep) ];
}


#
# Canonical order
#

my @deps = sort { scalar( @{ $deps{$a} } ) <=> scalar( @{ $deps{$b} } ) } keys( %deps );


#
# Top level Makefile
#

my $fhm = 'FileHandle'->new( '>Makefile' );
$fhm->print( << "EOF" );
TOPDIR=..

include \$(TOPDIR)/build/Makefile.inc

EOF

my @defaults = ();
my @targets = qw( test coef_io other mw_scatt mw_scatt_coef coef_io_11 emis_atlas brdf_atlas wrapper );
if ($RTTOV_HDF) { push @targets, 'gui' }
if ($RTTOV_ECMWF) { push @targets, 'coef_distribute' }

foreach my $target ( @targets ) {
  if ( -d $target ) { push @defaults, $target }
}
$fhm->print( join " ", ("\ndefault:", join(" ", @defaults), "\n\n") );

$fhm->print( "\nall: bin\n" );

for my $what (qw (bin lib intf dir))
  {
    $fhm->print( "\n$what: " . join (' ', map { "$_/$what" } @deps) . "\n\n" );
  }
$fhm->print( "\n" );

#
# Build of individual projects from top level Makefile
#

while( my ( $sub, $dep ) = each( %deps ) ) {
  my @deps = @{$deps{$sub}};

  $fhm->print( "\n$sub: $sub/bin\n" );

  $fhm->print( "\n$sub/bin: $sub/lib\n" );
  $fhm->print( "\tcd $sub && \$(MAKE) bin\n" );

  $fhm->print( "\n$sub/lib: $sub/intf " . join (' ', map { "$_/lib" } @deps) . "\n" );
  $fhm->print( "\tcd $sub && \$(MAKE) lib\n" );

  $fhm->print( "\n$sub/intf: $sub/dir\n" );
  $fhm->print( "\tcd $sub && \$(MAKE) intf\n" );

  $fhm->print( "\n$sub/dir:\n" );
  $fhm->print( "\tcd $sub && \$(MAKE) dir\n" );

  $fhm->print( "\n$sub/clean:\n" );
  $fhm->print( "\tcd $sub && \$(MAKE) clean\n" );

}

$fhm->print( "
clean:
\t\\rm -f \$(OBJDIR)/*\$(EXT_OBJ) \$(LIBDIR)/*\$(EXT_LIB) \$(LIBDIR)/*\$(EXT_SO) \$(MODDIR)/*\$(EXT_MOD) \$(BINDIR)/*\$(EXT_BIN) \$(INCDIR)/*
\t\\rm -rf \$(TMPDIR)/*

dist:
\tcd .. ; tar cf - ReleaseNote.pdf docs gui src build data brdf_data emis_data rtcoef_rttov13 rttov_test wrapper --exclude src/dev --exclude src/lbl --exclude src/lbl-add --exclude src/baran --exclude src/coef_distribute --exclude \"src/*/*.pl\" --exclude \"src/*/parallel.sh\" --exclude \"src/*/rttov_parallel*.txt\" --exclude \"docs/*lbl*\" --exclude \"docs/rttov-compile.*\" --exclude \"data/opac_interp_rh.py\" --exclude \"rtcoef_rttov13/extract*srf*.*\" --exclude \"rtcoef_rttov13/plot_lbl_*.py\" --exclude \"docs/*.odt\" --exclude \"docs/*.doc*\" --exclude \"docs/*.bib\" --exclude \"docs/*.tex\" --exclude \"docs/rttov_developer_guide.*\" --exclude \"docs/doxygen_doc_*\" --exclude \"docs/sharp_1983_*\" --exclude gui/rttov_tests.tar.bz2 --exclude rttov_test/rttov_remote.pl --exclude \"rttov_test/*_dev*\" --exclude \"rttov_test/*.1.*\" --exclude \"rttov_test/*.tar.gz\" --exclude \"rtcoef_rttov13/*/*.H5\" --exclude \"rtcoef_rttov13/rttov*pred*/*airs*\" --exclude \"rtcoef_rttov13/rttov*pred*/*iasi*\" --exclude \"rtcoef_rttov13/rttov*pred*/*cris*\" --exclude \"rtcoef_rttov13/rttov*pred*/*iris*\" --exclude \"rtcoef_rttov13/rttov*pred*/*mtg*irs*\" --exclude \"rtcoef_rttov13/cldaer*/s*\" --exclude \"rtcoef_rttov13/mfasis_lut/rt*\" --exclude \"rtcoef_rttov13/mfasis_nn/rt*\" --exclude \"rtcoef_rttov13/hydrotable*/*.dat\" --exclude \"rtcoef_rttov13/pc/*.dat\"  --exclude \"rtcoef_rttov13/htfrtc/*.dat\" --exclude \"emis_data/*\" --exclude \"brdf_data/*\" --exclude \"wrapper/doxygen_doc_*\" --exclude \"wrapper/*_dev*\" | xz > rttov_dist.tar.xz

distdev:
\tcd .. ; tar cf - ReleaseNote.pdf docs gui src build data brdf_data emis_data rttov_test wrapper --exclude \"docs/doxygen_doc_*\" --exclude \"rttov_test/*.tar.gz\" --exclude \"emis_data/*\" --exclude \"brdf_data/*\" --exclude \"wrapper/doxygen_doc_*\" | gzip > rttov_dist_dev.tgz

distlbl:
\tcd .. ; tar cf - docs/*lbl* src/lbl src/lbl-add rttov_lbl-data --exclude rttov_lbl-data/scripts_pascal --exclude rttov_lbl-data/rttov_lbl_instrument.kcarta --exclude rttov_lbl-data/instrument-data.kcarta | gzip > rttov_dist_lbl.tgz
" );

$fhm->close();

#
# Project Makefiles
#

for my $sub ( @deps ) {
  chdir( "$cwd/$sub" );

#
# libraries to link against
#
  my @D = sort { scalar( @{ $deps{$b} } ) <=> scalar( @{ $deps{$a} } ) } 
          ( $sub, @{$deps{$sub}} );
  my @libs = map { "\$(LIBDIR)/librttov\$(RTTOV_VERSION)_$_\$(EXT_LIB)" } @D;

  my @ll   = map { "-lrttov\$(RTTOV_VERSION)_$_" } @D;

# variables suffixed by 2 are for directories without subroutines or modules
  my @D2 = sort { scalar( @{ $deps{$b} } ) <=> scalar( @{ $deps{$a} } ) } 
          ( @{$deps{$sub}} );          
  my @libs2 = map { "\$(LIBDIR)/librttov\$(RTTOV_VERSION)_$_\$(EXT_LIB)" } @D2;

  my @ll2  = map { "-lrttov\$(RTTOV_VERSION)_$_" } @D2;

  my $fhm = 'FileHandle'->new( ">Makefile" );
  $fhm->print( << "EOF" );

TOPDIR=../..
SUBDIR=$sub

EOF


#
# FFLAGS for individual routines
#

  my @F90 = &F90::glob( '*' );
  my %prefix = map( { ( $_, (&F90::fileparse( $_ ))[0] ) } @F90 );

  my ( $len ) = sort { $b <=> $a } map { length( $_ ) } keys( %prefix );
  $len += length( "FFLAGS_ARCH_" ) + 2;

  for my $f ( sort keys( %prefix ) ) {
    $fhm->printf( "\%-${len}s = \$(FFLAGS_ARCH)\n", "FFLAGS_ARCH_$prefix{$f}" );
  }

$fhm->printf( "\n" );

#
# CFLAGS for individual routines
#

  my @C = &C::glob( '*' );
  my %cprefix = map( { ( $_, (&C::fileparse( $_ ))[0] ) } @C );

  my ( $clen ) = sort { $b <=> $a } map { length( $_ ) } keys( %cprefix );
  $clen += length( "CFLAGS_ARCH_" ) + 2;

  for my $f ( sort keys( %cprefix ) ) {
    $fhm->printf( "\%-${clen}s = \$(CFLAGS_ARCH)\n", "CFLAGS_ARCH_$cprefix{$f}" );
  }

#
# F2PY Flags for individual routines
#
  my @P = &F2PY::glob( '*' );
  my %pprefix = map( { ( $_, (&F90::fileparse( $_ ))[0] ) } @P );
  if ( $RTTOV_F2PY ) {

    my ( $plen ) = sort { $b <=> $a } map { length( $_ ) } keys( %pprefix );
    $plen += length( "F2PYFLAGS_ARCH_" ) + 2;

    for my $f ( sort keys( %pprefix ) ) {
      $fhm->printf( "\%-${plen}s = \$(F2PYFLAGS_ARCH)\n", "F2PYFLAGS_ARCH_$pprefix{$f}" );
    }
  }
  
#
# Find out which files need an interface
#

  my ( @F90_intf, @intf );

  for my $F90 ( @F90 ) {
    my $code = &F90::code( $F90 );
    if( &F90::has_INTF( $code ) ) {
      push @F90_intf, $F90;
      push @intf, &F90::INTERFACE( $F90 );
    }
  }

  $fhm->print( "\nF90_INTF=@F90_intf\n" );
  $fhm->print( "\nINTF=@intf\n" );


  my ( @obj_bin, @obj_mod, @obj_sub );
  my ( @F90_bin, @F90_mod, @F90_sub );

  for my $F90 ( @F90 ) {
    my $what = &F90::what_F90( $F90 );
    my $obj = &F90::OBJ( $F90 );
    if( $what eq 'Program' ) {
      push @obj_bin, $obj;
      push @F90_bin, $F90;
    } elsif( $what eq 'Module' ) {
      push @obj_mod, $obj;
      push @F90_mod, $F90;
    } else {
      push @obj_sub, $obj;
      push @F90_sub, $F90;
    }
  }

  my @bins = map { &F90::BIN( $_ ) } @F90_bin;
  my @mods = map { &F90::MOD( $_ ) } @F90_mod;

#
# Handle any C files
#
  
  my ( @C_sub );
  
  for my $C ( @C ) {
    my $obj = &C::OBJ( $C );
    push @obj_sub, $obj;
    push @C_sub, $C;
  }
  
#
# Handle any F2PY files
#
  
  my @F2PY_so;
    
  if ( $RTTOV_F2PY ) {

    for my $P ( @P ) {
      my $SO = &F90::SO( $P );
      push @bins, $SO;
      push @F2PY_so, $P;

    }
  }

  if ( @obj_sub or @obj_mod ) {
      $fhm->print( << "EOF" );

LIBNAME=librttov\$(RTTOV_VERSION)_\$(SUBDIR)\$(EXT_LIB)
LIB=\$(LIBDIR)/\$(LIBNAME)
LIBNOVER=\$(LIBDIR)/librttov_\$(SUBDIR)\$(EXT_LIB)
EOF

  }
  else {
      @libs=@libs2;
      @ll=@ll2;
      $fhm->print( << "EOF" );

LIB=
EOF

  }
  
  $fhm->print( << "EOF" );

OBJSUB=@obj_sub 

OBJMOD=@obj_mod

OBJBIN=@obj_bin

LIBS=@libs

BINS=@bins

MODS=@mods

include \$(TOPDIR)/build/Makefile.inc

lib: dir intf \$(LIB)

bin: lib \$(BINS)

dir: 
\t\@mkdir -p \$(TMPDIR)/\$(SUBDIR) \$(BINDIR) \$(OBJDIR) \$(INCDIR) \$(MODDIR) \$(LIBDIR)

intf: 
\t\@\$(MKINTF) \$(F90_INTF) \$(INTF)
\t\@\$(CPINCH) \$(INCDIR)

\$(LIB): \$(OBJSUB) \$(OBJMOD)
\t\\rm -f \$(LIB) \$(LIBNOVER)
\t\$(AR) \$(LIB) \$(OBJSUB) \$(OBJMOD)
\t\\ln -s \$(LIBNAME) \$(LIBNOVER)

clean:
\t\\rm -f \$(INTF) \$(LIB) \$(LIBNOVER) \$(BINS) \$(OBJSUB) \$(OBJMOD) \$(OBJBIN) \$(MODS)

EOF

#
# Last chance to modify FFLAGS & LDFLAGS
#
  if( -f 'Makefile.inc' ) {
    $fhm->print( "\ninclude Makefile.inc\n" );
  }

#
# Executables files for programs
#
  for my $F90 ( @F90_bin ) {
    my $OBJ = &F90::OBJ( $F90 );
    my $BIN = &F90::BIN( $F90 );
    $fhm->print( << "EOF" );

$BIN: $OBJ \$(LIBS)
\tcd \$(TMPDIR) && \$(FC) -o $BIN $OBJ -L\$(LIBDIR) @ll \$(LDFLAGS_EXTERN) \$(LDFLAGS_ARCH)
EOF

  }
  
  if ( $RTTOV_F2PY ) {

    for my $P ( @F2PY_so ) {
      my $OBJ = (&F90::fileparse( $P ))[0];
      my $SO = &F90::SO( $P );
      my $LBIN = &F90::PY_SO( $P );

      my @INTERFACE = map { &F90::INTERFACE( $_ ) } &F90::inc_F90( $P );
      my @MOD = map { &F90::MOD( $_ ) } &F90::use_rec_F90( $P );

      $fhm->print( << "EOF" );

$SO: $P @INTERFACE @MOD \$(LIBS)
\tcd \$(TMPDIR) && cp -f \$(SRCDIR)/f2py_f2cmap .f2py_f2cmap
\tcd \$(TMPDIR) && \$(F2PY) -c \$(SRCDIR)/$P -m $OBJ \\
\t --f77flags=\$(F2PYFLAGS_ARCH_$pprefix{$P}) --f90flags=\$(F2PYFLAGS_ARCH_$pprefix{$P}) \$(FFLAGS_EXTERN) \\
\t\$(FFLAG_MOD)\$(MODDIR) -I\$(INCDIR) \\
\t-L\$(LIBDIR) @ll \$(LDFLAGS_EXTERN) \$(F2PYLDFLAGS_ARCH)
\t\@cd \$(TMPDIR) && \$(MVDMOD) $LBIN $SO
EOF
    }
  }
#
# Object files for C subroutines
#

  for my $C ( @C_sub ) {
    my $OBJ = &C::OBJ( $C );
 
    my $cc = 'CC';
 
    $fhm->print( << "EOF" );

$OBJ: $C
\tcd \$(TMPDIR) && \$($cc) -I\$(INCDIR) \$(CFLAGS_EXTERN) \$(CFLAGS_ARCH_$cprefix{$C}) -c \$(SRCDIR)/$C -o $OBJ

EOF
  }


#
# Object files for programs & subroutines
#
  for my $F90 ( @F90_sub, @F90_bin ) {
    my $OBJ = &F90::OBJ( $F90 );
 
    my @INTERFACE = map { &F90::INTERFACE( $_ ) } &F90::inc_F90( $F90 );
    my @MOD = map { &F90::MOD( $_ ) } &F90::use_rec_F90( $F90 );

    my $fc = $F90 =~ m/\.f$/o ? 'FC77' : 'FC';
 
    $fhm->print( << "EOF" );

$OBJ: $F90 @INTERFACE @MOD
\tcd \$(TMPDIR) && \$($fc) -I\$(INCDIR) \$(FFLAGS_EXTERN) \$(FFLAGS_ARCH_$prefix{$F90}) \$(FFLAG_MOD)\$(MODDIR) -c \$(SRCDIR)/$F90 -o $OBJ

EOF
  }

#
# Object files for modules
#
  for my $F90 ( @F90_mod ) {
    my $OBJ = &F90::OBJ( $F90 );
    my $MOD = &F90::MOD( $F90 );
    my $mod = (&F90::fileparse( $F90 ))[0].'$(EXT_MOD)';
    my @INTERFACE = map { &F90::INTERFACE( $_ ) } &F90::inc_F90( $F90 );
    my @MOD = map { &F90::MOD( $_ ) } &F90::use_rec_F90( $F90 );
 
    $fhm->print( << "EOF" );

$OBJ: $F90 @INTERFACE @MOD
\tcd \$(TMPDIR) && \$(FC) -I\$(INCDIR) \$(FFLAGS_EXTERN) \$(FFLAGS_ARCH_$prefix{$F90}) \$(FFLAG_MOD)\$(MODDIR) -c \$(SRCDIR)/$F90 -o $OBJ
\t\@cd \$(TMPDIR) && \$(MVDMOD) $mod $MOD

$MOD: $F90 @INTERFACE @MOD
\tcd \$(TMPDIR) && \$(FC) -I\$(INCDIR) \$(FFLAGS_EXTERN) \$(FFLAGS_ARCH_$prefix{$F90}) \$(FFLAG_MOD)\$(MODDIR) -c \$(SRCDIR)/$F90 -o $OBJ
\t\@cd \$(TMPDIR) && \$(MVDMOD) $mod $MOD

EOF
  }

  $fhm->close();
}


chdir( "$cwd" );
if ( $USE_DRHOOK_LIB && -s "main/yomhook.txt" ) {
  move ("main/yomhook.txt", "main/yomhook.F90") or die "move back file main/yomhook.txt: $!";
  $verbose && print "move back file main/yomhook.txt\n";
}
