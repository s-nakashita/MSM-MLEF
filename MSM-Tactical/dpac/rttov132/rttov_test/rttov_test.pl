#!/usr/bin/perl -w

package rttov::compare;

use strict;
use warnings "FATAL" => 'all';

sub getline {
  my $fh = shift;
  return unless( defined( my $line0 = <$fh> ) );
  chomp( $line0 );
  $line0 =~ s/(\(\s*)$//o;
  while( my $line1 = <$fh> ) {
    last if( $line1 =~ m/^\s*\)\s*/o );
    chomp( $line1 );
    $line0 .= $line1;
  }
  
  
  return $line0;
}


sub cmp_kmatrix {
  my %args = @_;

  my $d      = $args{dir};
  my $f1     = $args{f1};        # file1, relative to dir
  my $f2     = $args{f2};        # file2, relative to dir
  my $nprint = $args{nprint};    # number of figures to print
  my $bf     = $args{bf};        # brute-force comparison
  my $doreal = $args{doreal};    # comparison in real values (only applies to brute-force)
  my $ignoretiny = $args{ignoretiny}; # comparison ignores small values (mainly for user tests)
  my $tinyabs    = $args{tinyabs};    # absolute value limit with $ignoretiny
  my $tinyrel    = $args{tinyrel};    # relative difference limit with $ignoretiny

  my $F1 = $f1 =~ m,^/,o ? $f1 : "$d/$f1";
  my $F2 = $f2 =~ m,^/,o ? $f2 : "$d/$f2";
  my ( $fh1, $fh2 ) = map { 'FileHandle'->new( "<$_" ) } ( $F1, $F2 );

  ( $fh1 && $fh2 ) or die;

  $nprint ||= 10;

  my $S = $bf ? 1. : 1e10;


  my %S = (
          'PROFILES_K.*\bP\b'                  => 1e+00,
          'PROFILES_K.*\bT\b'                  => 1e+04,
          'PROFILES_K.*\bQ\b'                  => 1.60771704e+5,   # 1e+06,
          'PROFILES_K.*\bO3\b'                 => 6.03504000e+2,   # 1e+03,
          'PROFILES_K.*\bCO2\b'                => 6.58114000e+4,   # 1e+05,
          'PROFILES_K.*\bN2O\b'                => 6.58090000e+4,   # 1e+05,
          'PROFILES_K.*\bCO\b'                 => 9.67053000e+4,   # 1e+05,
          'PROFILES_K.*\bCH4\b'                => 1.80548000e+5,   # 1e+06,
          'PROFILES_K.*\bSO2\b'                => 4.52116000e+5,   # 1e+06,
          'PROFILES_K.*\bAEROSOLS\b'           => 1e+04,
          'PROFILES_K.*\bCLOUDS\b'             => 1e+02,
          'PROFILES_K.*\bCFRAC\b'              => 1e+02,
          'PROFILES_K.*\bCLWDE\b'              => 1e+06,
          'PROFILES_K.*\bICEDE\b'              => 1e+06,
          'PROFILES_K.*\bCLW\b'                => 1e-01,
          'PROFILES_K.*\bS2M%T\b'              => 1e+04,
          'PROFILES_K.*\bS2M%Q\b'              => 1.60771704e+5,
          'PROFILES_K.*\bS2M%O\b'              => 6.03504000e+2,
          'PROFILES_K.*\bS2M%P\b'              => 1e+04,
          'PROFILES_K.*\bS2M%U\b'              => 1e+04,
          'PROFILES_K.*\bS2M%V\b'              => 1e+04,
          'PROFILES_K.*\bS2M%WFETC\b'          => 1e-01,
          'PROFILES_K.*\bSKIN%T\b'             => 1e+04,
          'PROFILES_K.*\bSKIN%FASTEM\b'        => 1e+02,
          'PROFILES_K.*\bSKIN%SALINITY'        => 1e+08,
          'PROFILES_K.*\bSKIN%FOAM_FRACTION\b' => 1e+04,
          'PROFILES_K.*\bCTP\b'                => 1e+04,
          'PROFILES_K.*\bCFRACTION\b'          => 1e+02,
          'CLD_PROFILES_K.*\bHYDRO\b'          => 1e-01,
          'CLD_PROFILES_K.*\bCFRAC\b'          => 1e+02,
          'CLD_PROFILES_K.*\bHYDRO_FRAC\b'     => 1e+02,
          'CLD_PROFILES_K.*\bPH\b'             => 1e+00,
          'EMISSIVITY_K'                       => 1e+00,
          'SPECULARITY_K'                      => 1e+00,
          'TSKIN_EFF_K'                        => 1e+04,
          'REFLECTANCE_K'                      => 1e+00,
          'REFLECTANCE_DIFFUSE_K'              => 1e+00,
          );


  my @print;

  while( 1 ) {
    my $line1 = &getline( $fh1 );
    my $line2 = &getline( $fh2 );

    last unless( defined( $line1 ) && defined( $line2 ) );
  
    $line1 =~ s/^(.*?)\s*=\s*//o;
    my $prefix1 = $1;
    $line2 =~ s/^(.*?)\s*=\s*//o;
    my $prefix2 = $1;
  
    if( $prefix1 ne $prefix2 ) {
      die "$f1/$prefix1, $f2/$prefix2";
    }
  
  
    my @x1 = do { local $_ = $line1; split; };
    my @x2 = do { local $_ = $line2; split; };
  
    if( @x1 != @x2 ) {
      die ("f1=$f1, f2=$f2, prefix=$prefix1; " . sprintf ("%d != %d", scalar (@x1), scalar (@x2)));
    }
  
  
    my $scale = 1.;
  
    while( my ( $key, $val ) = each( %S ) ) {
      if( $prefix1 =~ qr/$key/ ) {
        $scale = $val;
      }
    }
  
    $scale *= $S;
  
    my ( @dx1, @dx2 );
    if( ! $bf ) {
      @dx1 = @x1;
      @dx2 = @x2;
    } else {
       if ( $doreal ) {
         @dx1 = @x1;
         @dx2 = @x2;
       } else {
         @dx1 = map { int( $scale * $_ ) } @x1; 
         @dx2 = map { int( $scale * $_ ) } @x2;
       }
    }

    if( "@dx1" ne "@dx2" ) {
      my @dx;
      for( my $i = 0; $i < @x1; $i++ ) {
        push @dx, abs( $dx1[$i] - $dx2[$i] );
        if( $ignoretiny && abs( $dx1[$i] ) < $tinyabs && abs( $dx2[$i] ) < $tinyabs ) { $dx[$i] = 0 }
      }

      my $dmin = $doreal ? 0 : 4;

      my $grep = 
        $bf ? sub { ( $dx[$_] > $dmin ) && ( .1 < 2 * $dx[$_] / ( abs( $dx1[$_] ) + abs( $dx2[$_] ) ) ) }
            : ( $ignoretiny ? sub { $tinyrel * ( abs( $dx1[$_] ) + abs( $dx2[$_] ) ) < 2 * $dx[$_] }
                            : sub { $dx[$_] > 0 } );
    
      my @i = grep { $grep->( $_ ) } ( 0 .. $#x1 );
      
      if( @i ) {
      
        ( $nprint-- <=0 ) && last;
      
        my ( $j, @j ) = ( @i > 10, splice( @i, 0, 10 ) );
      
      
        my $line0 = join( '', ' ' x 51, map( { sprintf( " %8d", $_ ) } @j ), $j ? ' ...' : '' );
        my ( $line1, $line2 );

        if( ! $bf ) {
          $line1 = join( '', sprintf( "%-50s:", $f1 ), map( { " $_" } @x1[@j] ), $j ? ' ...' : '' );
          $line2 = join( '', sprintf( "%-50s:", $f2 ), map( { " $_" } @x2[@j] ), $j ? ' ...' : '' );
        } else {
          if ( $doreal ) {
            $line1 = join( '', sprintf( "%-50s:", $f1 ), map( { sprintf( " %8e", $_ ) } @dx1[@j] ), $j ? ' ...' : '' );
            $line2 = join( '', sprintf( "%-50s:", $f2 ), map( { sprintf( " %8e", $_ ) } @dx2[@j] ), $j ? ' ...' : '' );
          } else {
            $line1 = join( '', sprintf( "%-50s:", $f1 ), map( { sprintf( " %8d", $_ ) } @dx1[@j] ), $j ? ' ...' : '' );
            $line2 = join( '', sprintf( "%-50s:", $f2 ), map( { sprintf( " %8d", $_ ) } @dx2[@j] ), $j ? ' ...' : '' );
          }
        }

        my $prefix = $prefix1;

        push @print, [ $prefix, $line0, $line1, $line2 ];
      }
    }
  }


  my $length = 0;
  for( @print ) {
    my ( $prefix, $line0, $line1, $line2 ) = @$_;
    ( $length ) = sort { $b <=> $a } ( $length, map( { length( $_ ) } ( $line0, $line1, $line2 ) ) );
  }

  my @buffer;
  for( @print ) {
    my ( $prefix, $line0, $line1, $line2 ) = @$_;
    my $len = $length - length( $prefix );
    $len /= 2;
    $len -= 2;
    $prefix = ( '=' x 10 ) . " $prefix " . ( '=' x 10 );
    push @buffer, "$prefix\n$line0\n$line1\n$line2\n";
  }

  return \@buffer;
}


package flog;

=head1 NAME

flog

=head1 DESCRIPTION

FileHandle like class to print to a color-enabled tty.

=cut

use FileHandle;
use POSIX qw( isatty );
use Fcntl qw( SEEK_CUR );

sub new { 
  my $class = shift;
  my %args = @_;
  eval "use Term::ANSIColor qw();";
  my $color = ! $@;
  my $tty = &isatty( $args{fh} );
  $args{fh}->autoflush( 1 );
  return bless { %args, tty => &isatty( $args{fh} ), color => $color && $tty }, $class;
}

sub print : method {
  my $self = shift;
  return $self->{fh}->print( $self->color( @_ ) );
}

sub printf : method {
  my $self = shift;
  return $self->{fh}->printf( $self->color( @_ ) );
}

sub back {
  my ( $self, $n ) = @_;
  if( $self->{tty} ) {
    $self->{fh}->print( "\b" x $n );
  } else {
    $self->{fh}->seek( - $n, &SEEK_CUR );
  }
}

sub color {
  my $self = shift;
  my @a;
  for( @_ ) {
    if( ref( $_ ) ) {
      push @a, 
        $self->{color} ? &Term::ANSIColor::colored( @$_ ) : $_->[0];
    } else {
      push @a, $_;
    }
  }
  return @a;
}

package flogm;

=head1 NAME

flogm

=head1 DESCRIPTION

FileHandle like class to print to several file objects.

=cut


sub new {
  my $class = shift;
  return bless { @_ }, $class;
}

sub print : method {
  my $self = shift;
  for( @{ $self->{fhs} } ) {
    $_->print( @_ );
  }
}

sub printf : method {
  my $self = shift;
  for( @{ $self->{fhs} } ) {
    $_->printf( @_ );
  }
}

sub back {
  my $self = shift;
  for( @{ $self->{fhs} } ) {
    $_->back( @_ );
  }
}



package rttov::test;

=head1 NAME

rttov::test

=head1 DESCRIPTION

This a a RTTOV test unit. We take content from a directory from tests.0
and copy it into tests.1.ARCH.

=head1 METHODS

=over 4

=cut

use strict;

use File::Copy;
use File::Path;
use File::Spec;
use File::Basename;
use File::Find;
use FileHandle;
use POSIX;

sub new {

=item new

Here we build the test object, initializing various options.

=cut 

  my ( $class, %args ) = @_;

  my $cwd = &Cwd::cwd();

  my ( $t0, $t1, $c0, $c1, $test ) = @args{ qw( base0 base1 coef0 coef1 name ) };
  
  
  ( -d "$cwd/$t0/$test/in" ) or
     die( "Cannot find any test data in $cwd/$t0/$test" );

  my $readme = '';
  if(-f "$cwd/$t0/$test/readme.txt")
    {
      $readme = &rttov::test::util::slurp ("$cwd/$t0/$test/readme.txt");
    }

  # Find the first profile so we can have any number of profiles (not just up to 999)

  my @files = <$cwd/$t0/$test/in/profiles/0*1>;
  my $prof01 = "";
  foreach my $file (@files) {
    if ($file =~ m!/(00+1$)!) { $prof01 = "$1"; }
  }
  $prof01 or die( "Cannot find profile 0..01");

  my $dims = &rttov::test::util::grok_dims( "$cwd/$t0/$test", $prof01 );

  my $aerinput = ".";
  if (-f "$cwd/$t0/$test/in/profiles/$prof01/atm/aerosl.txt") { $aerinput = "F" };
  if (-f "$cwd/$t0/$test/in/aer_opt_param.txt") { $aerinput = "P" };
  my $cldinput = ".";
  if (-f "$cwd/$t0/$test/in/profiles/$prof01/atm/cloud.txt") { $cldinput = "F" };
  if (-f "$cwd/$t0/$test/in/cld_opt_param.txt") { $cldinput = "P" };

  # If the optical parameter files are present, but empty then regenerate them

  if(-f "$cwd/$t0/$test/in/aer_opt_param.txt") {
    my $opdat = &rttov::test::util::slurp( "$cwd/$t0/$test/in/aer_opt_param.txt" );
    if( length( $opdat ) < 100) {
      my $coefs = &rttov::test::util::slurp( "$cwd/$t0/$test/in/coef.txt" );
      my ( $rtfn ) = ( $coefs =~ m{f_coef.*'(.*)'} );
      my ( $scaerfn ) = ( $coefs =~ m{f_scaer.*'(.*)'} );
      my $dimobj = bless { %$dims };
      my $nchannels = $dimobj->{nchannels};
      my $nlayers = $dimobj->{nlevels} - 1;

      if(! -f "../$::OPTIONS{BIN}/rttov_make_opt_param.exe") {
        die( "Cannot find ../$::OPTIONS{BIN}/rttov_make_opt_param.exe to generate optical properties" );
      }
      printf( "Generating $t0/$test/in/aer_opt_param.txt\n" );
      my @cmd = ();
      if( $::OPTIONS{SCHED_CMD} ) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };
      push @cmd, "../$::OPTIONS{BIN}/rttov_make_opt_param.exe",
                 "--rtcoef_file", "../$rtfn", "--scaer_file", "../$scaerfn",
                 "--test_dir", "$t0/$test", "--nchanprof", "$nchannels", "--nlayers", "$nlayers";
      system( @cmd );
    }
  }

  if(-f "$cwd/$t0/$test/in/cld_opt_param.txt") {
    my $opdat = &rttov::test::util::slurp( "$cwd/$t0/$test/in/cld_opt_param.txt" );
    if( length( $opdat ) < 100) {
      my $coefs = &rttov::test::util::slurp( "$cwd/$t0/$test/in/coef.txt" );
      my ( $rtfn ) = ( $coefs =~ m{f_coef.*'(.*)'} );
      my ( $sccldfn ) = ( $coefs =~ m{f_sccld.*'(.*)'} );
      my $dimobj = bless { %$dims };
      my $nchannels = $dimobj->{nchannels};
      my $nlayers = $dimobj->{nlevels} - 1;

      if(! -f "../$::OPTIONS{BIN}/rttov_make_opt_param.exe") {
        die( "Cannot find ../$::OPTIONS{BIN}/rttov_make_opt_param.exe to generate optical properties" );
      }
      printf( "Generating $t0/$test/in/cld_opt_param.txt\n" );
      my @cmd = ();
      if( $::OPTIONS{SCHED_CMD} ) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };
      push @cmd, "../$::OPTIONS{BIN}/rttov_make_opt_param.exe",
                 "--rtcoef_file", "../$rtfn", "--sccld_file", "../$sccldfn",
                 "--test_dir", "$t0/$test", "--nchanprof", "$nchannels", "--nlayers", "$nlayers";
      system( @cmd );
    }
  }

  my $coefs = &rttov::test::util::slurp( "$cwd/$t0/$test/in/coef.txt" );
  my ( $version ) = ( $coefs =~ m/rtcoef_rttov(\d+)/o );
  $version ||= '?';

  my ( $rttovscatt ) = ( $coefs =~ m/f_rttovscatt/ );

  my %flags = ();

  $flags{rttovscatt}             = $rttovscatt;
  $flags{radar}                  = $::OPTIONS{RADAR};
  $flags{multi_hydro_frac}       = $::OPTIONS{MULTI_HYDRO_FRAC};
  $flags{lusercfrac}             = $::OPTIONS{LUSERCFRAC};
  $flags{cc_threshold}           = $::OPTIONS{CC_THRESHOLD};
  $flags{ice_polarisation}       = $::OPTIONS{ICE_POLARISATION};
  $flags{pol_mode}               = $::OPTIONS{POL_MODE};
  $flags{hydro_cfrac_tlad}       = $::OPTIONS{HYDRO_CFRAC_TLAD};
  $flags{zero_hydro_tlad}        = $::OPTIONS{ZERO_HYDRO_TLAD};
  $flags{aerinput}               = $aerinput;
  $flags{cldinput}               = $cldinput;
  $flags{grid_box_avg_cloud}     = $::OPTIONS{GRID_BOX_AVG_CLOUD};
  $flags{cldcol_threshold}       = $::OPTIONS{CLDCOL_THRESHOLD};
  $flags{cloud_overlap}          = $::OPTIONS{CLOUD_OVERLAP};
  $flags{cc_low_cloud_top}       = $::OPTIONS{CC_LOW_CLOUD_TOP};
  $flags{ir_scatt_model}         = $::OPTIONS{IR_SCATT_MODEL};
  $flags{vis_scatt_model}        = $::OPTIONS{VIS_SCATT_MODEL};

  $flags{dom_nstreams}           = $::OPTIONS{DOM_NSTREAMS};
  $flags{dom_accuracy}           = $::OPTIONS{DOM_ACCURACY};
  $flags{dom_opdep_threshold}    = $::OPTIONS{DOM_OPDEP_THRESH};
  $flags{dom_rayleigh}           = $::OPTIONS{DOM_RAYLEIGH};
  $flags{rayleigh_depol}         = $::OPTIONS{RAYLEIGH_DEPOL};

  $flags{addsolar}               = $::OPTIONS{SOLAR};
  $flags{rayleigh_max_wavelength}= $::OPTIONS{RAYLEIGH_MAX_WAVELENGTH};
  $flags{rayleigh_min_pressure}  = $::OPTIONS{RAYLEIGH_MIN_PRESSURE};
  $flags{rayleigh_single_scatt}  = $::OPTIONS{RAYLEIGH_SINGLE_SCATT};
  $flags{do_nlte_correction}     = $::OPTIONS{DO_NLTE};
  $flags{ir_sea_emis_model}      = $::OPTIONS{IR_SEA_EMIS_MODEL};
  $flags{solar_sea_brdf_model}   = $::OPTIONS{SOLAR_SEA_BRDF_MODEL};

  $flags{fastem_version}         = $::OPTIONS{FASTEM_VERSION};
  $flags{fastem3_rwd_fix}        = $::OPTIONS{FASTEM3_RWD_FIX};
  $flags{clw_scheme}             = $::OPTIONS{MW_CLW_SCHEME};
  $flags{clw_cloud_top}          = $::OPTIONS{MW_CLW_CLOUD_TOP};
  $flags{supply_foam_fraction}   = $::OPTIONS{SUPPLY_FOAM_FRACTION};

  $flags{do_lambertian}          = $::OPTIONS{DO_LAMBERTIAN};
  $flags{lambertian_fixed_angle} = $::OPTIONS{LAMBERTIAN_FIXED_ANGLE};
  $flags{switchrad}              = $::OPTIONS{SWITCHRAD};
  $flags{addrefrac}              = $::OPTIONS{REFRACTION};
  $flags{use_t2m_opdep}          = $::OPTIONS{USE_T2M_OPDEP};
  $flags{use_q2m}                = $::OPTIONS{USE_Q2M};
  $flags{use_tskin_eff}          = $::OPTIONS{USE_TSKIN_EFF};
  $flags{plane_parallel}         = $::OPTIONS{PLANE_PARALLEL};
  $flags{rad_down_lin_tau}       = $::OPTIONS{RAD_DOWN_LIN_TAU};
  $flags{dtau_test}              = $::OPTIONS{DTAU_TEST};
  $flags{transmittances_only}    = $::OPTIONS{TRANSMITTANCES_ONLY};

  $flags{addinterp}              = $::OPTIONS{ADDINTERP};
  $flags{interp_mode}            = $::OPTIONS{INTERP_MODE};
  $flags{reg_limit_extrap}       = $::OPTIONS{REG_LIMIT_EXTRAP};
  $flags{spacetop}               = $::OPTIONS{SPACETOP};
  $flags{lgradp}                 = $::OPTIONS{LGRADP};

  $flags{fix_hgpl}               = $::OPTIONS{FIX_HGPL};
  $flags{apply_reg_limits}       = $::OPTIONS{APPLY_REG_LIMITS};
  $flags{opdep13_gas_clip}       = $::OPTIONS{OPDEP13_GAS_CLIP};
  $flags{do_checkinput}          = $::OPTIONS{DO_CHECKINPUT};
  $flags{verbose}                = $::OPTIONS{VERBOSE};
  $flags{gas_units}              = $::OPTIONS{GAS_UNITS};
  $flags{lallchans}              = $::OPTIONS{LALLCHANS};
  $flags{ignoretiny}             = $::OPTIONS{IGNORETINY};
  $flags{htfrtc}                 = $::OPTIONS{HTFRTC};
  $flags{simple_cloud}           = $::OPTIONS{HTFRTC_SIMPLE_CLOUD};
  $flags{overcast}               = $::OPTIONS{HTFRTC_OVERCAST};

  $flags{do_opdep_calc}          = $::OPTIONS{DO_OPDEP_CALC};
  $flags{no_opt_param_tladk}     = $::OPTIONS{NO_OPT_PARAM_TLADK};

  $flags{realprec}               = $::OPTIONS{REALPREC};
  $flags{emis}                   = $::OPTIONS{EMIS};
  $flags{refl}                   = $::OPTIONS{REFL};
  $flags{diffuse_refl}           = $::OPTIONS{DIFFUSE_REFL};
  $flags{surftype}               = $::OPTIONS{SURFTYPE};
  $flags{specularity}            = $::OPTIONS{SPECULARITY};
  $flags{tskin_eff_from_prof}    = $::OPTIONS{TSKIN_EFF_FROM_PROF};
  $flags{zenang}                 = $::OPTIONS{ZENANG};
  $flags{sunzenang}              = $::OPTIONS{SUNZENANG};
  $flags{fix2m}                  = $::OPTIONS{FIX2M};
  $flags{zero_cloud}             = $::OPTIONS{ZERO_CLOUD};
  $flags{zero_aerosol}           = $::OPTIONS{ZERO_AEROSOL};
  $flags{visir_clw_scheme}       = $::OPTIONS{VISIR_CLW_SCHEME};
  $flags{visir_ice_scheme}       = $::OPTIONS{VISIR_ICE_SCHEME};
  $flags{cfraction}              = $::OPTIONS{CFRACTION};
  $flags{no_flux_conv}           = $::OPTIONS{NO_FLUX_CONV};

  my @g =
    sort grep { m/^(?:so2|ch4|co2|co|n2o|o3|clw)$/o } 
    map { basename( $_, qw( .txt ) ) } <$cwd/$t0/$test/in/profiles/$prof01/atm\/*.txt>;
    
  my @do_opts = qw( direct tl ad k k_bf k_tl k_ad taylor );
  my %do = map { ( "$_", $::OPTIONS{uc($_)} ) } @do_opts;

  my $calcemis = &rttov::test::util::slurp( "$cwd/$t0/$test/in/calcemis.txt" );
  if ($calcemis) {
    $calcemis = $calcemis =~ m/T/goms;
  } else {
    $calcemis = 1;
  }
  my $calcrefl = &rttov::test::util::slurp( "$cwd/$t0/$test/in/calcrefl.txt" );
  if ($flags{addsolar}) {
    if ($calcrefl) { 
      $calcrefl = $calcrefl =~ m/T/goms;
    } else {
      $calcrefl = 1;}
  } else {
    $calcrefl = 0;
  }
  
  my $st;

  
  my $self = bless { 
    base0 => $t0, 
    base1 => $t1, 
    coef0 => $c0,
    coef1 => $c1,
    name => $test,
    prof01 => $prof01,
    readme => $readme,
    options => {
      %do,
      %flags,
      %$dims,
      gases => "@g",
      version => $version,
      calcemis => $calcemis,
      calcrefl => $calcrefl,
    },
  }, 'rttov::test';
  return $self;
  
}

sub prepare_run {

=item prepare_run

We prepare the data for running: copy data content to in directory, create namelist.

=cut


  my $self = shift;

  my ( $t0, $t1, $c0, $c1, $test ) = @{$self}{ qw( base0 base1 coef0 coef1 name) };
  
  my $cwd = &Cwd::cwd();
  my $dir = "$cwd/$t1/$test";

  
  rmtree( $dir );
  mkpath( "$dir/out" ) 
    or die( "Cannot mkpath( `$dir/out' ):$!\n" );

  system( 'cp', '-r', "$cwd/$t0/$test/in", "$dir/in" ); 
  chdir( "$dir/out" );

  my $rttov_prefix = $ENV{RTTOV_PREFIX};
  
  if( $::OPTIONS{COEF_EXTRACT} ) {

    # We have a list of channels in channels.txt which may not be in ascending order.
    # We want to renumber them so that the channels take numbers from 1...MAXVAL(channels.txt)
    # and so that the channel number order is preserved.
    # This is exactly what is done in rttov_test.F90 when processing the input channel list.

    my $renumber_channels = sub
      {
        use List::Util qw( max );
        my $f = shift;
        return unless (-f $f);
        my $chan = &rttov::test::util::slurp ($f);
        $chan =~ s/(?:^\s*|\s*$)//go;
        my @chan = split( m/\s+/o, $chan );

        # The following is not very Perl-like, but it does the job.
        # It is very similar to the code in rttov_test.F90
        my @lchan;
        for (1..max(@chan)) { push @lchan, 0 }
        for (@chan) { $lchan[$_-1] = 1 }

        my $i = 1;
        for (1..max(@chan)) {
          if ($lchan[$_-1]) { $lchan[$_-1] = $i; $i++ }
        }
        @chan = map { $lchan[$_-1] } @chan;

        'FileHandle'->new( ">$f" )->print( join ("\n", @chan) . "\n" );
      };

    $renumber_channels->( '../in/channels.txt' );
#   $renumber_channels->( '../in/channels_rec.txt' );

    $rttov_prefix = "$cwd/$c1/$test";
  }
  
  local $ENV{RTTOV_PREFIX} = $rttov_prefix;

  my $env_sh = 'env.sh';
  'FileHandle'->new( ">$env_sh" )->print( << "EOF" );
#!/bin/sh

# load arch specific environment variables

EOF

  'FileHandle'->new( ">>$env_sh" )->print( map ({ my $k = $_; "$k=$::ARCH_ENV{$k}\nexport $k\n\n" } keys (%::ARCH_ENV)) );


  
  my $run_sh = 'run.sh';
  'FileHandle'->new( ">$run_sh" )->print( << "EOF" );
#!/bin/sh

# this script can be invoked from the current directory

. ./env.sh

$::OPTIONS{SCHED_CMD} $cwd/../$::OPTIONS{BIN}/rttov_test.exe > rttov_test.log 2>&1

EOF

  chmod( 0755, $env_sh );
  chmod( 0755, $run_sh );
  

  for my $key ( qw( direct tl ad k k_bf k_tl k_ad ) ) {
    mkpath( "$key" ) if( $self->{options}{$key} );
  }

  $self->write_namelist();

  chdir( $cwd );
}


sub coef_extract {

=item coef_extract

Extract appropriate channels for this test. We use rttov_conv_coef.exe. We generate a hash value
id per channel list with Digest::Perl::MD5.

=cut



  my $self = shift;

  $self->{status} = { status => 'OK' };

  my $cwd = &Cwd::cwd();

  my ( $t0, $t1, $c0, $c1, $test ) = @{$self}{ qw( base0 base1 coef0 coef1 name ) };

  
  my $dirt1 = "$cwd/$c1/$test";
  ( -d $dirt1 ) 
    or mkpath( $dirt1 ) 
    or die( "Cannot mkpath( `$dirt1' ):$!\n" );

  my $dirc1 = "$cwd/$c1/data";
  ( -d $dirc1 ) 
    or mkpath( $dirc1 ) 
    or die( "Cannot mkpath( `$dirc1' ):$!\n" );
    
  chdir( $dirc1 );

  my $coef = &rttov::test::util::slurp( "$cwd/$t0/$test/in/coef.txt" );

  my ( $f_coef_in )       = ( $coef =~ m/f_coef\s*=\s*'(\S+)'/goms );
  my ( $f_scaer_in )      = ( $coef =~ m/f_scaer\s*=\s*'(\S+)'/goms );
  my ( $f_sccld_in )      = ( $coef =~ m/f_sccld\s*=\s*'(\S+)'/goms );
  my ( $f_mfasis_aer_in ) = ( $coef =~ m/f_mfasis_aer\s*=\s*'(\S+)'/goms );
  my ( $f_mfasis_cld_in ) = ( $coef =~ m/f_mfasis_cld\s*=\s*'(\S+)'/goms );
  my ( $f_mfasis_nn_in )  = ( $coef =~ m/f_mfasis_nn\s*=\s*'(\S+)'/goms );
  my ( $f_pccomp_in )     = ( $coef =~ m/f_pccomp\s*=\s*'(\S+)'/goms );
  my ( $f_rttovscatt_in ) = ( $coef =~ m/f_rttovscatt\s*=\s*'(\S+)'/goms );

  my $chan = &rttov::test::util::slurp( "$cwd/$t0/$test/in/channels.txt" );
  $chan =~ s/(?:^\s*|\s*$)//go;
  my @chan = split( m/\s+/o, $chan );
  my %chan = map { ( $_, 1 ) } @chan;
  @chan = sort { $a <=> $b } keys( %chan );

  for( $f_coef_in, $f_scaer_in, $f_sccld_in, $f_mfasis_aer_in, $f_mfasis_cld_in, $f_mfasis_nn_in, $f_pccomp_in, $f_rttovscatt_in ) {
    $_ && mkpath( dirname( $_ ) );
  }

  $chan = join( '-', @chan );
  
  my $hash = &Digest::Perl::MD5::md5_hex( "$chan.$::OPTIONS{COEF_FORMAT}" );
  
  my $f_coef_out       = $f_coef_in       ? "$f_coef_in.$hash"       : '';
  my $f_scaer_out      = $f_scaer_in      ? "$f_scaer_in.$hash"      : '';
  my $f_sccld_out      = $f_sccld_in      ? "$f_sccld_in.$hash"      : '';
  my $f_mfasis_aer_out = $f_mfasis_aer_in ? "$f_mfasis_aer_in.$hash" : '';
  my $f_mfasis_cld_out = $f_mfasis_cld_in ? "$f_mfasis_cld_in.$hash" : '';
  my $f_mfasis_nn_out  = $f_mfasis_nn_in  ? "$f_mfasis_nn_in.$hash"  : '';
  my $f_pccomp_out     = $f_pccomp_in     ? "$f_pccomp_in.$hash"     : '';
  my $f_rttovscatt_out = $f_rttovscatt_in ? "$f_rttovscatt_in.$hash" : '';
  
  my $c = 1;
  for( $f_coef_out, $f_scaer_out, $f_sccld_out, $f_mfasis_aer_out, $f_mfasis_cld_out, $f_mfasis_nn_out, $f_pccomp_out, $f_rttovscatt_out ) {
    next unless( $_ );
    $c &&= -f $_;
  }
  $c && goto DONE;
  
  unlink( $_ ) for( $f_coef_out, $f_scaer_out, $f_sccld_out, $f_mfasis_aer_out, $f_mfasis_cld_out, $f_mfasis_nn_out, $f_pccomp_out, $f_rttovscatt_out );

  if (! $f_rttovscatt_in) {
    my @cmd = ( 'rttov_conv_coef.log', );
    if ($::OPTIONS{SCHED_CMD}) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };
    push @cmd, "$cwd/../$::OPTIONS{BIN}/rttov_conv_coef.exe",
      '--channels' => @chan,
      '--format-out', $::OPTIONS{COEF_FORMAT},
    $f_coef_in       ? ( '--coef-in'        => "$c0/$f_coef_in",       '--coef-out'       => $f_coef_out )       : (),
    $f_scaer_in      ? ( '--scaer-in'       => "$c0/$f_scaer_in",      '--scaer-out'      => $f_scaer_out )      : (),
    $f_sccld_in      ? ( '--sccld-in'       => "$c0/$f_sccld_in",      '--sccld-out'      => $f_sccld_out )      : (),
    $f_mfasis_aer_in ? ( '--mfasis_aer-in'  => "$c0/$f_mfasis_aer_in", '--mfasis_aer-out' => $f_mfasis_aer_out ) : (),
    $f_mfasis_cld_in ? ( '--mfasis_cld-in'  => "$c0/$f_mfasis_cld_in", '--mfasis_cld-out' => $f_mfasis_cld_out ) : (),
    $f_mfasis_nn_in  ? ( '--mfasis_nn-in'   => "$c0/$f_mfasis_nn_in",  '--mfasis_nn-out'  => $f_mfasis_nn_out )  : (),
    $f_pccomp_in     ? ( '--pccoef-in'      => "$c0/$f_pccomp_in",     '--pccoef-out'     => $f_pccomp_out )     : ();
    $self->{status} = &rttov::test::util::execute( @cmd );
  } else {
    my @cmd = ( 'rttov_conv_coef.log', );
    if ($::OPTIONS{SCHED_CMD}) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };
    push @cmd, "$cwd/../$::OPTIONS{BIN}/rttov_conv_coef.exe",
      '--format-out', $::OPTIONS{COEF_FORMAT},
      ( '--coef-in'   => "$c0/$f_coef_in",    '--coef-out'   => $f_coef_out );
    $self->{status} = &rttov::test::util::execute( @cmd );

    @cmd = ( 'rttov_conv_coef.log', );
    if ($::OPTIONS{SCHED_CMD}) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };
    push @cmd, "$cwd/../$::OPTIONS{BIN}/rttov_ascii2bin_scattcoef.exe",
      ( '--coef-in'   => "$c0/$f_rttovscatt_in",    '--coef-out'   => $f_rttovscatt_out );
    $self->{status} = &rttov::test::util::execute( @cmd );
  }
DONE:

  chdir( $dirt1 );
  
  for( $f_coef_in, $f_scaer_in, $f_sccld_in, $f_mfasis_aer_in, $f_mfasis_cld_in, $f_mfasis_nn_in, $f_pccomp_in, $f_rttovscatt_in ) {
    next unless( $_ );
    unlink( $_ ) if( -f $_ );
    
    my $d = dirname( $_ );
    ( -d $d ) or mkpath( $d );
    
    symlink( 'File::Spec'->abs2rel( "$dirc1/$_.$hash", $d ), "$_" );
  }
  if ( $f_rttovscatt_in ) {
    my $d = dirname( $f_rttovscatt_in );
    symlink( "$cwd/../rtcoef_rttov13/hydrotable/ScalingFactorForBulkProperties.rssp", "$d/ScalingFactorForBulkProperties.rssp" );
  }
  
  chdir( $cwd );
  
  return $self->{status}{status} eq 'OK';
}

sub write_namelist {

=item write_namelist

Create test namelist.

=cut

  my $self = shift;

  my ( $t0, $t1, $test, $c0, $c1 ) = @{$self}{ qw( base0 base1 name coef0 coef1 ) };
  
  my $fh = 'FileHandle'->new( '>rttov_test.txt' );
  
  my @txt;

# DAR add \/\w+ below to require text file name to start with an alphanumeric char or _
# so that emacs backup files don't get added to the @txt array

  find( { wanted => sub { return if (m{profiles/\d+/}o); m/\/\w+\.txt$/o && push( @txt, $_ ); }, no_chdir => 1, follow => 1, follow_skip => 2 }, '../in' );
  find( { wanted => sub { return if (!m{profiles/$self->{prof01}/}o); m/\/\w+\.txt$/o && push( @txt, $_ ); }, no_chdir => 1, follow => 1 }, "../in/profiles/$self->{prof01}" );
  
  $fh->print( "&rttov_test_nml\n" );

  for my $key ( qw( apply_reg_limits opdep13_gas_clip do_checkinput verbose fix_hgpl ) ) {
    $fh->printf( "  defn%%opts%%config%%%-24s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( switchrad addrefrac use_t2m_opdep use_q2m use_tskin_eff plane_parallel 
                    do_lambertian lambertian_fixed_angle rad_down_lin_tau dtau_test transmittances_only ) ) {
    $fh->printf( "  defn%%opts%%rt_all%%%-24s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( addsolar rayleigh_single_scatt do_nlte_correction grid_box_avg_cloud dom_rayleigh rayleigh_depol ) ) {
    $fh->printf( "  defn%%opts%%rt_ir%%%-25s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( fastem3_rwd_fix supply_foam_fraction ) ) {
    $fh->printf( "  defn%%opts%%rt_mw%%%-25s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( addinterp spacetop lgradp reg_limit_extrap ) ) {
    $fh->printf( "  defn%%opts%%interpolation%%%-17s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }

  for my $key ( qw( rayleigh_max_wavelength rayleigh_min_pressure
                    cloud_overlap cldcol_threshold cc_low_cloud_top
                    dom_nstreams dom_accuracy dom_opdep_threshold
                    ir_scatt_model vis_scatt_model ir_sea_emis_model solar_sea_brdf_model ) ) {
    $fh->printf( "  defn%%opts%%rt_ir%%%-25s = %s\n", "$key", $self->{options}{$key} );
  }
  for my $key ( qw( fastem_version clw_scheme clw_cloud_top ) ) {
    $fh->printf( "  defn%%opts%%rt_mw%%%-25s = %s\n", "$key", $self->{options}{$key} );
  }
  for my $key ( qw( interp_mode ) ) {
    $fh->printf( "  defn%%opts%%interpolation%%%-17s = %s\n", "$key", $self->{options}{$key} );
  }

  for my $key ( qw( apply_reg_limits opdep13_gas_clip do_checkinput verbose fix_hgpl ) ) {
    $fh->printf( "  defn%%opts_scatt%%config%%%-18s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( use_t2m_opdep use_q2m use_tskin_eff addrefrac rad_down_lin_tau dtau_test fastem3_rwd_fix 
                    supply_foam_fraction reg_limit_extrap lgradp lusercfrac hydro_cfrac_tlad zero_hydro_tlad ) ) {
    $fh->printf( "  defn%%opts_scatt%%%-25s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }
  for my $key ( qw( cc_threshold ice_polarisation fastem_version interp_mode pol_mode ) ) {
    $fh->printf( "  defn%%opts_scatt%%%-25s = %s\n", "$key", $self->{options}{$key} );
  }

  for my $key ( qw( htfrtc simple_cloud overcast ) ) {
    $fh->printf( "  defn%%opts%%htfrtc_opts%%%-19s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }

  for my $key ( qw( do_opdep_calc no_opt_param_tladk ) ) {
    $fh->printf( "  defn%%opts%%dev%%%-27s = %s\n", "$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }

  for my $key ( qw( direct tl ad k k_bf k_tl k_ad taylor ) ) {
    $fh->printf( "  defn%%%-25s = %s\n", "do_$key", $self->{options}{$key} ? '.TRUE.' : '.FALSE.' );
  }

  $fh->printf( "  defn%%%-25s = %s\n", radar            => $::OPTIONS{RADAR}            ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", multi_hydro_frac => $::OPTIONS{MULTI_HYDRO_FRAC} ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", taylor_by_chan   => $::OPTIONS{TAYLOR_BY_CHAN}   ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", taylor_on_btrefl => $::OPTIONS{TAYLOR_ON_BTREFL} ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", calc_rad2        => $::OPTIONS{CALC_RAD2}        ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", calc_emis_terms  => $::OPTIONS{CALC_EMIS_TERMS}  ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", user_check_opts  => $::OPTIONS{USER_CHECK_OPTS}  ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", user_check_prof  => $::OPTIONS{USER_CHECK_PROF}  ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %d\n", input_gas_units  => $::OPTIONS{INPUT_GAS_UNITS} );
  $fh->printf( "  defn%%%-25s = %d\n", run_gas_units    => $::OPTIONS{GAS_UNITS} );
  $fh->printf( "  defn%%%-25s = %s\n", do_print         => $::OPTIONS{PRINT}            ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", print_opts       => $::OPTIONS{PRINT_OPTS}       ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", print_profiles   => $::OPTIONS{PRINT_PROFILES}   ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", print_quality    => $::OPTIONS{PRINT_QUALITY}    ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", lallchans        => $::OPTIONS{LALLCHANS}        ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %s\n", ltemp            => $::OPTIONS{TEMP_ALLOC}       ? '.TRUE.' : '.FALSE.' );
  $fh->printf( "  defn%%%-25s = %d\n", ntimes           => $::OPTIONS{NTIMES}   );
  $fh->printf( "  defn%%%-25s = %d\n", nthreads         => $::OPTIONS{NTHREADS} );
  $fh->printf( "  defn%%%-25s = %d\n", mult             => $::OPTIONS{MULT} );
  $fh->printf( "  defn%%%-25s = %s\n", scale_inc        => $::OPTIONS{SCALE_INC} );
  $fh->printf( "  defn%%%-25s = %s\n", scale_out        => $::OPTIONS{SCALE_OUT} );
  $fh->printf( "  defn%%%-25s = %s\n", savehdf5         => $::OPTIONS{SAVEHDF5}         ? '.TRUE.' : '.FALSE.' );

#   $fh->print( "!\n" );

  for my $txt ( @txt ) {
    $fh->printf( "  defn%%%-25s = '%s'\n", 'f_' . basename( $txt, qw( .txt ) ), $txt =~ m{/profiles/}o ? do { (my $txt1 = $txt) =~ s{.*/profiles/\d+}{}o; $txt1 } : $txt );
  }

#   $fh->print( "!\n" );

  $fh->printf( "  defn%%%-25s = %d\n", 'prof_ndigits', length($self->{prof01}) );

  for my $key ( qw( realprec nlevels nprofiles nchannels nchannels_rec ) ) {
    next if ($self->{options}{$key} < 0);
    $fh->printf( "  defn%%%-25s = %d\n", $key, $self->{options}{$key} );
  }

#   $fh->print( "!\n" );

  if( $::OPTIONS{COEF_EXTRACT} ) {
    $fh->printf( "  defn%%%-25s = '%s'\n",  'coef_prefix', "$::OPTIONS{CWD}/$c1/$test" );
  } else {
    $fh->printf( "  defn%%%-25s = '%s'\n",  'coef_prefix', $ENV{RTTOV_PREFIX} );
  }
  if( $::OPTIONS{PROF_BY_PROF} ) {
    $fh->printf( "  defn%%%-25s = %s\n",  'prof_by_prof', '.TRUE.' );
  }
  if( $::OPTIONS{CHAN_BY_CHAN} ) {
    $fh->printf( "  defn%%%-25s = %s\n",  'chan_by_chan', '.TRUE.' );
  }

  for my $key ( qw( ZERO_CLOUD ZERO_AEROSOL NO_FLUX_CONV TSKIN_EFF_FROM_PROF ) ) {
    if( $::OPTIONS{$key} ) {
      $fh->printf( "  defn%%%-25s = %s\n", lc $key, '.TRUE.' );
    }
  }
  if( $::OPTIONS{FIX2M} ) {
    $fh->printf( "  defn%%%-25s = %s\n",  'fixed2m', '.TRUE.' );
  }
  for my $key ( qw( EMIS REFL DIFFUSE_REFL SURFTYPE SPECULARITY ZENANG SUNZENANG VISIR_CLW_SCHEME VISIR_ICE_SCHEME CFRACTION ) ) {
    if( exists $::OPTIONS{$key} ) {
      $fh->printf( "  defn%%%-25s = %s\n", "fixed".(lc $key), $::OPTIONS{$key} );
    }
  }

  for my $key ( qw( O3_COL_INT_DU O3_COL_INT O3_MAX_PPMV CO2_COL_INT CO2_MAX_PPMV N2O_COL_INT N2O_MAX_PPMV
                    CO_COL_INT CO_MAX_PPMV CH4_COL_INT CH4_MAX_PPMV SO2_COL_INT SO2_MAX_PPMV) ) {
    if( exists $::OPTIONS{$key} ) {
      $fh->printf( "  defn%%%-25s = %s\n", lc $key, $::OPTIONS{$key} );
    }
  }

  $fh->print( "/\n" );
  $fh->close();

}



sub can_run {
  my $self = shift;

  return 1;
}

sub skip {
  my $self = shift;
  $self->{status} = { status => 'SKIP' }; 
}

sub dir {

=item dir

Base test directory in tests.1.ARCH

=cut

  my $self = shift;
  my ( $t0, $t1, $c0, $c1, $test ) = @{$self}{ qw( base0 base1 coef0 coef1 name ) };
  my $cwd = &Cwd::cwd();
  my $dir = "$cwd/$t1/$test";
  return $dir;
}

sub line {
  my ( $self, $F, $FF, %args ) = @_;
  &rttov::test::util::line( 1, 0, $F, $FF, %args, readme => $self->{readme} );
}

sub check {
  my $self = shift;

  my $dir = $self->dir() . '/out';

  my @F = qw(
    k/profiles_k.txt
    k/cld_profiles_k.txt
    k/aer_opt_param_k.txt
    k/cld_opt_param_k.txt
    k/profiles_k_pc.txt
    k/profiles_k_rec.txt
    k/emissivity_k.txt
    k/specularity_k.txt
    k/tskin_eff_k.txt
    k/reflectance_k.txt
    k/reflectance_diffuse_k.txt
    ad/profiles_ad.txt
    ad/cld_profiles_ad.txt
    ad/aer_opt_param_ad.txt
    ad/cld_opt_param_ad.txt
    ad/emissivity_ad.txt
    ad/specularity_ad.txt
    ad/tskin_eff_ad.txt
    ad/reflectance_ad.txt
    ad/reflectance_diffuse_ad.txt
    tl/radiance_tl.txt
    tl/transmission_tl.txt
    tl/reflectivity_tl.txt
    tl/pcscores_tl.txt
    tl/emissivity_out_tl.txt
    tl/specularity_tl.txt
    tl/tskin_eff_tl.txt
    tl/reflectance_out_tl.txt
    tl/reflectance_diffuse_out_tl.txt
    direct/radiance.txt
    direct/transmission.txt
    direct/reflectivity.txt
    direct/scatt_emis_terms.txt
    direct/pcscores.txt
    direct/emissivity_out.txt
    direct/specularity.txt
    direct/tskin_eff.txt
    direct/reflectance_out.txt
    direct/reflectance_diffuse_out.txt
  );
  
  $self->{status}{status} ||= 'OK';

  my ( $t0, $t1, $c0, $c1, $test ) = @{$self}{ qw( base0 base1 coef0 coef1 name ) };
  my $cwd = &Cwd::cwd();
  my $ref = $::OPTIONS{TEST_REF} ? "$::OPTIONS{TEST_REF}/$test" : undef;
  my $doreal = $::OPTIONS{DOREAL} ? 1 : 0;
  my $ignoretiny = $::OPTIONS{IGNORETINY} ? 1 : 0;
  my $tinyabs    = $::OPTIONS{TINYABS};
  my $tinyrel    = $::OPTIONS{TINYREL};

  for( @F ) {
    my $F = "$dir/$_";
    my $B = basename( $F ); 
    my $dF = basename( dirname( $F ) );
    if( -f $F ) {
      for my $G ( <$dir/*/$B> ) {
        my $dG = basename( dirname( $G ) );
        next if( $G eq $F );
        my $buffer = &rttov::compare::cmp_kmatrix(  
            dir => $dir, f1 => "$dF/$B", f2 => "$dG/$B", 
            bf => ( $dG =~ m,k_bf,o ? 1 : 0 ), 
            doreal => ( $dG !~ m,k_bf,o ? 1 : $doreal ),
            ignoretiny => $ignoretiny,
            tinyabs => $tinyabs, tinyrel => $tinyrel );
        if( @$buffer ) {
          $self->{status}{status} = 'DIFF';
          $self->{status}{log} .= "@$buffer";
          'FileHandle'->new( ">$dir/$dF-$dG-$B" )->print( "@$buffer" );
        }
      }
      if( $ref ) {
        if( -f "$cwd/$ref/out/$dF/$B" ) {
          my $buffer = &rttov::compare::cmp_kmatrix( 
              dir => $cwd, f1 => "$t1/$test/out/$dF/$B", f2 => "$ref/out/$dF/$B",
              ignoretiny => $ignoretiny,
              tinyabs => $tinyabs, tinyrel => $tinyrel );
          if( @$buffer ) {
            $self->{status}{status} = 'DIFF';
            $self->{status}{log} .= "@$buffer";
            'FileHandle'->new( ">$dir/$B.$::OPTIONS{TEST_REF}" )->print( "@$buffer" );
          }
        } else {
          $self->{status}{status} = 'DIFF';
          $self->{status}{log} .= "$ref/out/$dF/$B does not exist\n";
        }
      }
    }
  }

  # Check Taylor test results
  
  if( $::OPTIONS{TAYLOR} ) {
    
    # Taylor output is in the file .../out/taylor_test.log.
    # Report DIFF if there are not 3 consecutive iterations tending to 1.0 with the
    # final one being the fourth iteration or later and being within 1.E-5 of 1.0.
    # If there is a DIFF, all iterations are printed out for the offending profile.

    # Parameters defining success:
    my $minconsec = 3;      # Min number of consecutive iterations which must converge to 1.0
    my $minbestindex = 4;   # Index of final convergent iteration must equal or exceed this
    my $maxdiff = 1.E-5;    # Max distance from 1.0 of final convergent iteration
    
    my @lines = split /\n/, &rttov::test::util::slurp ("$dir/taylor_test.log");
    
    my ($junk, $nprof, $niter);
    ($junk, $nprof) = split /\s+/, $lines[0]; # First line gives number of records
    ($junk, $niter) = split /\s+/, $lines[1]; # Second line gives number of iterations per record
    my $outlog = $lines[2]."\n";              # Third line is the table heading
    my $i = 2;                                # Current index into lines
    
    # Remainder of output lists (prof, chan, iter, ratio) for each profile, with each profile 
    # separated by a blank line
    
    foreach (1..$nprof) {
      my ($thiscount, $maxcount, $maxindex, $stars) = (0,0,0,0);
      my $tmplog = '';
      my @ratio;
      for (my $iter = 1; $iter <= $niter; $iter++) {
        $i++;
        $tmplog .= $lines[$i]."\n";  # Store $lines of this profile in case there's a DIFF
        ($junk, $junk, $junk, $junk, $ratio[$iter]) = split /\s+/, $lines[$i];
        
        # Compare consecutive ratios
        if( $ratio[$iter] =~ /\*/ ) {
          $stars = 1;
        } elsif( $iter > 1 ) {
          if( !$stars && ((abs($ratio[$iter]-1.0) < abs($ratio[$iter-1]-1.0)) || $ratio[$iter] == 1.0) ) {
            $thiscount += 1;
            if( $thiscount >= $maxcount ) {
              ($maxcount, $maxindex) = ($thiscount, $iter);
            }
          } else {
            $thiscount = 0;
          }
        }
      }
      
      # Determine if we flag a DIFF and if so add this profile to the local output log
      if( $stars                     ||
          $maxcount < $minconsec-1   ||
          $maxindex < $minbestindex  ||
          abs($ratio[$maxindex]-1.0) > $maxdiff ) {
        $self->{status}{status} = 'DIFF';
        $outlog .= $tmplog."\n";
      }
      
      $i++; # Blank space between profiles
    }

    # If there was a DIFF append the local output log to the test log
    if( $self->{status}{status} eq 'DIFF' ) {
      $self->{status}{log} .= $outlog;
    } 
  }
}



=pod

=back

=cut


package rttov::test::pack;

=head1 NAME

rttov::test::pack

=head1 DESCRIPTION

This class implement a set of atomic tests. Here we have the possibility to
run several tests within the same execution of rttov_test.exe.

=head1 METHODS

=over 4

=cut


use File::Path;
use strict;

sub new {
  my $class = shift;
  my %args = @_;
  my @tests = map { 'rttov::test'->new( %args, name => $_ ) } split( m/\+/o, $args{name} );
  return bless { @_, tests => \@tests }, $class;
}

sub run {

=item run

Executes atomic tests in a single shot. Output is in the directory of the last test.

=cut

  my $self = shift;

  my @tests = @{ $self->{tests} };
  my @dirs;
  for my $test ( @tests ) {
    $test->prepare_run();
    push @dirs, $test->dir() . '/out';
  }

  my $cwd = &Cwd::cwd();

  if( $::OPTIONS{DR_HOOK} ) {

    $ENV{DR_HOOK_PROFILE} = "$dirs[-1]/drhook.%d";
    $ENV{DR_HOOK} = '1';
    $ENV{DR_HOOK_OPT} = 'cpuprof';
    if( -f "$dirs[-1]/drhook.1" ) {
        print STDERR "DR HOOK_PROFILE copie \n";
       move ("$dirs[-1]/drhook.1", "$dirs[-1]/drhook.1.old")  == 1 or die "failed mv $dirs[-1]/drhook.1 \n";
    }
  }

  if( $::OPTIONS{FTRACE} ) {
    $ENV{FTRACEDIR} = $dirs[-1];
  }


  # Build the @cmd array to execute

  my @cmd = ( "$dirs[-1]/rttov_test.log", );
  if( $::OPTIONS{SCHED_CMD} ) { push @cmd, split( m/\s+/o, "$::OPTIONS{SCHED_CMD}" ) };

  if( $::OPTIONS{MEMCHECK} ) {
    # User has requested valgrind/memcheck for memory leak checking.
    # Output is written to the rttov_test.log output file.

    push @cmd, "valgrind", "--leak-check=full", "--track-origins=no", "--show-reachable=yes";

  } elsif( $::OPTIONS{MASSIF} ) {
    # User has requested valgrind/massif for memory usage profiling.
    # "--pages-as-heap=yes": includes absolutely all allocations made for the program; low-level functions like mmap will
    #                        often allocate a large region of memory and provide smaller chunks of this to specific
    #                        malloc/etc calls - this option includes the whole lot. Also accounts for memory taken up by the code.
    # "--stacks=yes": excludes code and low-level system calls (e.g. mmap). The values seems to be more "realistic" given what
    #                 memory resources RTTOV actually asks for and also gives more human-readable allocation traces, but
    #                 can underestimate peak usage in some scenarios.
    #
    # Using --stacks=yes to compare two version of RTTOV (with the same compiler) should be a reasonable way to identify changes in memory requirements.
    #
    # NB When using NAG the results look odd so best to avoid if you're looking for realistic memory usage (gfortran and ifort are reasonable).
    #
    # After using this option, to view output you need to run:
    # $ ms_print SESSION.1.ARCH/test/dir/out/rttov_massif_out.dat

    # Note that this is peak usage by the test suite which is almost certainly (slightly) greater than
    # is strictly required by RTTOV. Consider running massif on example_fwd.

#     push @cmd, "valgrind", "--tool=massif", "--pages-as-heap=yes", "--massif-out-file=rttov_massif_out.dat";
    push @cmd, "valgrind", "--tool=massif", "--stacks=yes", "--massif-out-file=$dirs[-1]/rttov_massif_out.dat";
  }

  push @cmd, "$cwd/../$::OPTIONS{BIN}/rttov_test.exe", "--test-path-list", @dirs;

# print STDERR "@cmd\n";

  $self->{status} = &rttov::test::util::execute( @cmd );
    
  return $self->{status}{status} eq 'OK';

}

sub can_run {
  my $self = shift;
  my $c = 1;
  for( @{ $self->{tests} } ) {
    $c &&= $_->can_run();
  }
  return $c;
}

sub skip {
  my $self = shift;
  $self->{status} = { status => 'SKIP' }; 
  for( @{ $self->{tests} } ) {
    $_->skip();
  }
}

sub coef_extract {
  my $self = shift;
  my $c = 1;
  for( @{ $self->{tests} } ) {
    my $d = $_->coef_extract();
    $c &&= $d;
    unless( $d ) {
      $self->{status} = $_->{status};
    }
  }
  return $c;
}

sub line1 {
  my ( $self, $F ) = @_;
  my $n = $#{ $self->{tests} };
  for my $i ( 0 .. $n ) {
    my $test = $self->{tests}[$i];
    $test->line( $F, \@::F1, %::OPTIONS, %{ $test->{options} }, NAME => $test->{name} );
    $F->print( "\n" ) unless( $i == $n );
  }
}

sub line2 {
  my ( $self, $F ) = @_;
  &rttov::test::util::line( 1, 0, $F, \@::F2, %{ $self->{status} } );
}

sub check {
  my $self = shift;
  for my $test ( @{ $self->{tests} } ) {
    $test->check();
    if( $test->{status}{status} eq 'DIFF' ) {
      $self->{status}{status} = 'DIFF';
      $self->{status}{log} .= $test->{status}{log};
    }
  }
}

=pod

=back

=cut

package rttov::test::util;

use strict;

sub grok_dims {
  my $dir = shift;
  my $prof01 = shift;

  my $sub = sub {
    my ($f, $dontfail) = @_;
    my $fh = 'FileHandle'->new( "$dir/in/$f" );
    (!$fh) and $dontfail and return ();
    $fh or die( "Cannot open $f:$!\n" );
    local $/ = undef;
    local $_ = <$fh>;
    split;
  };
  my @p = $sub->( "profiles/$prof01/atm/p.txt" );
  my @l = $sub->( 'lprofiles.txt' );
  my @c = $sub->( 'channels_rec.txt', 1);
  
  return {
    nlevels       => scalar( @p ),
    nprofiles     => $l[-1],
    nchannels     => scalar( @l ),
    nchannels_rec => @c ? scalar (@c) : '-1',
  };
}

sub execute {
  my ( $log, @cmd ) = @_;
# print STDERR &Data::Dumper::Dumper (\@cmd);

# print STDERR "@_\n";

  my ( $real0, $user0 ) = (&POSIX::times())[0,3];

  my $clock_ticks;
 
  {
    local $SIG{__WARN__} = sub { };
    $clock_ticks = &POSIX::sysconf( &POSIX::_SC_CLK_TCK );
  }
  
  my $pid = fork();



  if( ! defined( $pid ) ) {
    return { status => 'FAIL' };
  } elsif( $pid > 0 ) {
    my $int = 0;
    local $SIG{INT} = sub { $int++; };    
    waitpid( $pid, 0 );
    my $c = $?;
    delete $SIG{INT};
    if( $c ) {
      my $logt = &rttov::test::util::slurp( $log );
      return { ( status => $int ? 'SKIP' : 'FAIL' ), log => $logt };
    }
  } else {
    close( STDOUT ); 
    close( STDERR ); 
    open( STDOUT, ">$log" );
    open( STDERR, ">>$log" );
    STDOUT->autoflush( 1 );
    STDERR->autoflush( 1 );
    exec( @cmd ) 
      or die( "Cannot execute @cmd:$!\n" );
  }  

  my ( $real1, $user1 ) = (&POSIX::times())[0,3];
  
  my $dt_user = ( $user1 - $user0 ) / $clock_ticks;
  my $dt_real = ( $real1 - $real0 ) / $clock_ticks;


  return { real => $dt_real, user => $dt_user, status => 'OK' };
}

sub slurp {
  my $fn = shift;
  return do { 
    my $fh = 'FileHandle'->new( "<$fn" );
    local $/ = undef;
    $fh ? <$fh> : ''
  };   
}

sub logical {
  return $_[0] ? 'X' : '.';
}

sub status {
  my $status = shift;
  my %color = qw( FAIL red OK green DIFF yellow SKIP blue );
  my $color = $color{$status};

  my $i = 0;
  while( length( $status ) != 6 ) {
    $status = $i++ % 2 ? " $status" : "$status ";
  }
  return [ $status, $color ];
}

sub format {
  return uc( substr( $_[0], 0, 1 ) );
}

sub ftime {
  return defined( $_[0] ) ? sprintf( ' %10.2f ', $_[0] ) : ' ' x 12;
}

sub center {
  my ( $str, $nc ) = @_;
  my $i = 0;
  while( length( $str ) < $nc ) {
    $str = $i++ % 2 ? " $str" : "$str ";
  }
  while( length( $str ) > $nc ) {
    chop( $str );
  }
  return $str;
}

my %H = ( 
  'NAME'                   => [ sub { &center( $_[0], 17 ) }, &center( 'TEST_ID', 17 )   ],
  'NTHREADS'               => [     '%1d ',  'T ',      'number of threads'              ],
  'NTIMES'                 => [     '%4d ',  '   N ',   'call RTTOV n times'             ],
  'nchannels'              => [     '%6d ',  '   C   ', 'number of channels'             ],
  'nprofiles'              => [     '%4d ',  '  P  ',   'number of profiles'             ],
  'nlevels'                => [     '%3d ',  '  L ',    'number of levels'               ],
  'TEMP_ALLOC'             => [  \&logical,        'T', 'temp allocation'                ],
  'do_checkinput'          => [  \&logical,        'C', 'do checkinput'                  ],
  'apply_reg_limits'       => [  \&logical,        'A', 'apply reg limits'               ],
  'reg_limit_extrap',      => [  \&logical,        'R', 'reg limit extrap'               ],
  'spacetop'               => [  \&logical,        'T', 'space top'                      ],
  'lgradp'                 => [  \&logical,        'G', 'gradients wrt pressure'         ],
  'addsolar'               => [  \&logical,        'S', 'solar radiation'                ],
  'do_nlte_correction'     => [  \&logical,        'N', 'nLTE correction'                ],
  'addrefrac'              => [  \&logical,        'R', 'refraction'                     ],
  'do_lambertian'          => [  \&logical,        'L', 'lambertian surface'             ],
  'lambertian_fixed_angle' => [  \&logical,        'F', 'lambertian fixed angle'         ],
  'use_t2m_opdep'          => [  \&logical,        'T', 'use s2m%t in opdep calc'        ],
  'use_q2m'                => [  \&logical,        'Q', 'use s2m%q'                      ],
  'rad_down_lin_tau'       => [  \&logical,        'L', 'lin-in-tau for downwelling rad' ],
  'switchrad'              => [  \&logical,        'W', 'switchrad'                      ],
  'plane_parallel'         => [  \&logical,        'P', 'plane-parallel'                 ],
  'rttovscatt'             => [  \&logical,        'S', 'rttov-scatt'                    ],
  'radar'                  => [  \&logical,        'R', 'radar'                          ],
  'lusercfrac'             => [  \&logical,        'U', 'lusercfrac'                     ],
  'aerinput'               => [  \&format,         'A', 'aerosols'                       ],
  'cldinput'               => [  \&format,         'C', 'clouds'                         ],
  'ir_scatt_model'         => [     '%1d',         'I', 'iR scattering model'            ],
  'vis_scatt_model'        => [     '%1d',         'V', 'vIS scattering model'           ],
  'ignoretiny'             => [  \&logical,        'I', 'ignore small values'            ],
  'direct'                 => [  \&logical,        'D', 'direct'                         ],
  'tl'                     => [  \&logical,        'T', 'tangent linear'                 ],
  'ad'                     => [  \&logical,        'A', 'adjoint'                        ],
  'k'                      => [  \&logical,        'K', 'k-matrix'                       ],
  'k_bf'                   => [  \&logical,        'B', 'brute force k-matrix'           ],
  'k_tl'                   => [  \&logical,        'T', 'tangent linear k-matrix'        ],
  'k_ad'                   => [  \&logical,        'A', 'adjoint k-matrix'               ],
  'taylor'                 => [  \&logical,        'T', 'taylor test'                    ],
  'gases'                  => [  ' %-21s ',   &center( 'GASES', 23 )                     ],
  'gas_units'              => [    '%1d' ,       'G' , 'gas units'                       ],
  'clw_scheme'             => [    '%1d' ,       'M' , 'mw clw scheme'                   ],
  'interp_mode'            => [    '%1d ',       'I ', 'interpolation mode'              ],
  'fastem_version'         => [    '%1d' ,       'F' , 'fastem version'                  ],
  'ir_sea_emis_model'      => [    '%1d' ,       'I' , 'iR sea emis model'               ],
  'solar_sea_brdf_model'   => [    '%1d ',       'S ', 'Solar sea brdf model'            ],
  'real'                   => [    \&ftime,   &center( ' REAL TIME', 12 )                ],
  'user'                   => [    \&ftime,   &center( ' USER TIME', 12 )                ],
  'status'                 => [   \&status,   'STATUS'                                   ],
  'why'                    => [  ' %-20s ',      'WHY'                                   ],
  'COEF_EXTRACT'           => [  \&logical,        'E', 'extract coefficients'           ],
  'COEF_FORMAT'            => [  \&format,         'F', 'format of extracted coefs'      ],
  'lallchans'              => [  \&logical,        'A', 'all coef channels read'         ],
  'MULT'                   => [     '%4d ',     '   M ', 'multiplicity'                  ],
  'SCALE_INC'              => [     '%4s ',     '   I ', 'scale TL/AD increments'        ],
  'SCALE_OUT'              => [     '%4s ',     '   O ', 'scale TL/AD output'            ],
  'version'                => [     '%2d ',      ' V ', 'version of RTTOV coefficients', ],
  'calcemis'               => [  \&logical,        'E', 'calcemis true'                  ],
  'calcrefl'               => [  \&logical,        'R', 'calcrefl true'                  ],
  'CHAN_BY_CHAN'           => [  \&logical,        'C', 'channel by channel'             ],
  'PROF_BY_PROF'           => [  \&logical,        'P', 'profile by profile'             ]
);



sub line {
  my ( $c, $d, $F, $fmt, %args ) = @_;

  if ($args{readme})
    {
      chomp (my $readme = $args{readme});
      $F->print ("[ $readme ]\n");
    }

  my @head = ();
  my $text = '';

  my $N = 0;
  if( ! $c ) { 
    for my $f ( @$fmt ) {
      my $H = $H{$f};
      if( $H->[2] ) {
        my ( $T, $C ) = ( $H->[1] =~ m/^(\s*)(\S)/o );
        push @head, [ $N + length( $T ), $H->[2], $C ];
      }
      $N += length( $H->[1] );
    }
  }


  for my $f ( @$fmt ) {
    my $H = $H{$f};
    
    if( ! $c ) {
      $text .= $H->[1];
      next;
    }
    
    if( ref( $H->[0] ) ) {
      $F->print( $H->[0]->( $args{$f} ) );
    } else {
      $F->printf( $H->[0], $args{$f} );
    }
  }

# die Data::Dumper::Dumper( [ @head[0..7] ] );

  if( ! $c ) {
    if( @head  && $d ) {
      my $N1 = $head[0][0];
      my $N2 = $head[-1][0] + 3;
      my $B = ' ' x $N1;
      for my $i ( 0 .. $#head ) {
        $F->print( $B );
        my $n = $N1;
#       print( ' ' x ( $head[$i][0] - $N1 ), '|', $head[$i][1], "\n" );
        for my $j ( 0 .. $i-1 ) {
          my $str = ' ' x ( $head[$j][0] - $n ) . '|';
          $n += length( $str );
          $F->print( $str );
        }
        $n += length( my $str = ' ' x ( $head[$i][0] - $n ) . '+' );
        $F->print( $str );
        $F->print( '-' x ( $N2 - $n ) );
        my $I = index( $head[$i][1], lc( $head[$i][2] ) );
        if( $I >= 0 ) {
          $F->print( " ", substr( $head[$i][1], 0, $I ) );
          $F->printf( [ $head[$i][2], 'magenta' ] );
          $F->print( substr( $head[$i][1], $I + 1 ) );
          $F->print( "\n" );
        } else {
          $F->print( " ", $head[$i][1], "\n" );
        }
      }
    }
    $F->print( $text );
  }
}





package main;


use strict;
use File::Find;
use File::Basename;
use Data::Dumper;
use Sys::Hostname;
      

use POSIX qw( strftime );
use Cwd;

&Digest::Perl::MD5::gen_code();

our %OPTIONS;

our @F1 = qw(
  NAME nchannels nprofiles nlevels
  calcemis calcrefl rttovscatt aerinput cldinput
  gas_units gases clw_scheme 
  lusercfrac radar 
  ir_scatt_model vis_scatt_model
  do_checkinput apply_reg_limits
  reg_limit_extrap spacetop lgradp
  interp_mode
  switchrad plane_parallel addrefrac rad_down_lin_tau use_t2m_opdep use_q2m 
  do_lambertian lambertian_fixed_angle
  addsolar do_nlte_correction
  fastem_version ir_sea_emis_model solar_sea_brdf_model
  SCALE_INC SCALE_OUT
  NTHREADS MULT NTIMES
  CHAN_BY_CHAN PROF_BY_PROF
  TEMP_ALLOC lallchans
  COEF_FORMAT ignoretiny
  direct tl ad k k_bf k_tl k_ad taylor
);

our @F2 = qw( real user status );



sub rttov_head {
  my $args = shift;

  my $F = $args->{F};

  my $version = basename( dirname( &Cwd::cwd() ) );
  my $changeset = &rttov::test::util::slurp( "../VERSION" );
  $changeset =~ s/\n/  /go;  
  my $user  = getpwuid( $< );
  chomp( my $uname = `uname -a` );
  
  if( $OPTIONS{RUN} ) {
    my $date = &POSIX::strftime( "%d/%m/%Y %T", localtime() );
    my $host  = hostname(); 
    $F->print( << "EOF" );

RTTOV: $version     $changeset
By:    $user\@$host 
On:    $uname
With:  $OPTIONS{ARCH}
Start: $date
EOF
  }

  &rttov::test::util::line( 0, 1, $F, \@F1 );
  if( $OPTIONS{RUN} ) {
    &rttov::test::util::line( 0, 1, $F, \@F2 );
  }
  $F->print( "\n" );

}

sub rttov_tail {
  my $args = shift;
  my ( $F, $status ) = @{$args}{ qw( F status ) };

  
  
  if( $OPTIONS{RUN} ) {
    my $date = &POSIX::strftime( "%d/%m/%Y %T", localtime() );
    $F->print( "\nEnd: $date\n\n" );
    my $n = 0;
    $n += $_ for( values( %$status ) );
    $F->printf( "Ran %d tests", $n );
    while( my ( $key, $val ) = each( %$status ) ) {
      $F->print( ", $val = $key" );
    }
    $F->print( "\n" );
  }

}

sub rttov_help {
   die( << "EOF" );

   + ARCH=...                         mandatory unless \$ARCH environment variable is set

   + SESSION=...                      test session name (default: tests)

   + BIN=bin                          directory where binary executables are kept;
                                      this path is relative to RTTOV top directory
                                      (default: bin)

   + SCHED_CMD=aprun                  if specified this inserts commands before each
                                      executable is called by rttov_test.pl. This includes
                                      rttov_conv_coef.exe, rttov_make_opt_param.exe and
                                      rttov_test.exe. This can be used to invoke a task
                                      scheduler for example. You can supply additional
                                      arguments by enclosing the command in quotes, e.g.
                                      SCHED_CMD="aprun --abc" (default: empty string)

   + TEST_LIST=hirs/01,airs/51,...    comma separated list of tests to be run; it
                                      is also possible to define tests such as:
                                      hirs/01+airs/51; in this case, hirs/01 and
                                      airs/51 will be run from within the same
                                      executable (default: all tests)

   + TEST_MATCH=hirs                  regex to filter the tests

   + LIST_ONLY=1                      do not run tests, show list (default: 0/false)

   + COEF_EXTRACT=1                   extract needed coefficients data; this should not
                                      be used for tests where the number of channels
                                      varies from profile to profile (default: 0/false)

   + COEF_FORMAT=formatted            format for extracting coefficient data:
                                      formatted/unformatted/hdf5; has no effect unless
                                      COEF_EXTRACT=1 (default: formatted)

   + LALLCHANS=1                      force all channels to be read from the coefficient
                                      file; ignored if COEF_EXTRACT=1 (default: 0/false)

   + TEMP_ALLOC=1                     run RTTOV with temporary data allocated
                                      outside RTTOV (default: 0/false)

   + MULT=10                          number of channels and profiles is increased by
                                      a factor of MULT (default: 1)

   + NTIMES=10                        number times to run RTTOV (default: 1)

   + NTHREADS=2                       number of threads to run RTTOV (rttov_direct
                                      rttov_tl, rttov_ad, rttov_k) with; a value of 1 or
                                      more will force RTTOV to be called via the parallel
                                      interface (default: 0)

   + PRINT=1                          print results to disk (default: 1/true)

   + PRINT_OPTS=1                     prints options structure to test log (default: 0/false)

   + PRINT_PROFILES=1                 prints profiles structures to test log (default: 0/false)

   + PRINT_QUALITY=1                  prints radiance quality flags to test log (default: 0/false)

   + DIRECT=1 TL=1, AD=1, K=1, K_BF=1, K_TL=1, K_AD=1
                                      enables direct, tangent linear, adjoint, K matrix,
                                      brute force K matrix, tangent linear K matrix,
                                      adjoint K matrix (defaults: 0/false)

   + TAYLOR=1                         performs Taylor test per profile (default: 0/false)

   + TAYLOR_BY_CHAN=1                 performs Taylor test per channel (default: 0/false)

   + TAYLOR_ON_BTREFL=1               Taylor test calculated on BTs/refls rather than
                                      radiances. If the simulation involves both visible
                                      and IR channels this should be used in conjunction
                                      with TAYLOR_BY_CHAN=1 (default: 0/false)

   + CALC_RAD2=1                      calculate secondary radiances (only applies to
                                      rttov_direct) (default: 0/false)

   + CALC_EMIS_TERMS=1                calculate RTTOV-SCATT emissivity retrieval terms
                                      structure (only applies to RTTOV-SCATT direct model)
                                      (default: 0/false)

   + TEST_REF=...                     provides reference data to check direct/tl/ad/k
                                      tests results against when CHECK=1 (default: none)

   + CHECK=1                          performs check between direct/tl/ad/k and the
                                      supplied TEST_REF reference data, performs internal
                                      consistency checks on k_bf/k_tl/k_ad/k, and checks
                                      that TAYLOR test output converges correctly
                                      (default: 1/true)

   + DOREAL=1                         performs k_bf/k comparison in real values, default
                                      is test in scaled integers (default: 0/false)

   + IGNORETINY=1                     ignores small absolute values and small relative
                                      differences when reporting differences
                                      (default: 0/false)

   + TINYABS=1.E-11                   with IGNORETINY ignore differences when values being
                                      compared are smaller than this (default: 1.E-11)

   + TINYREL=1.E-5                    with IGNORETINY ignore relative differences smaller
                                      than this (default: 1.E-5)

   + REALPREC=16                      specify number of significant figures in real output
                                      (default: 6)

   + PRINT_ERROR=0                    print error in the test listing (default: 1/true)

   + SCALE_INC=2                      scale increments for TL/AD computations by
                                      a factor of SCALE_INC (default: 1.0)

   + SCALE_OUT=2                      scale TL/AD output of TL/AD computations by
                                      a factor of SCALE_OUT (default: 1.0)

   + SWITCHRAD=0                      sets the switchrad boolean (default: 1/true)

   + REFRACTION=0                     (de)activates refraction (default: 1/true)

   + USE_T2M_OPDEP=0                  use s2m%t input profile variable in optical depth
                                      calculation (default: 1/true)

   + USE_Q2M=0                        use s2m%q input profile variable (default: 1/true)

   + USE_TSKIN_EFF=1                  use per-channel effective skin temperature inputs
                                      (default: 0/false)

   + PLANE_PARALLEL=1                 set plane_parallel boolean; NB plane parallel
                                      atmosphere is automatically used for DOM simulations
                                      (default: 0/false)

   + RAD_DOWN_LIN_TAU=0               set rad_down_lin_tau boolean (default: 1/true)

   + DTAU_TEST=1                      set dtau_test boolean (default: 0/false)

   + TRANSMITTANCES_ONLY=1            only compute transmittances, no radiances (default: 0/false)

   + SOLAR=1                          activates solar radiation (default: 0/false)

   + RAYLEIGH_MAX_WAVELENGTH=1.       set the maximum channel wavelength (microns) for which to
                                      calculate Rayleigh scattering (default: 2 microns)

   + RAYLEIGH_MIN_PRESSURE=10.        set the pressure (hPa) below which Rayleigh scattering
                                      is ignored (default: 0 hPa)

   + RAYLEIGH_SINGLE_SCATT=0          set the rayleigh_single_scatt boolean (default: 1/true)

   + DO_NLTE=1                        sets the do_nlte_correction flag to true
                                      (default: 0/false)

   + GRID_BOX_AVG_CLOUD=0             set the grid_box_avg_cloud boolean (default: 1/true)

   + CLDCOL_THRESHOLD=-1.0            set the value of cldcol_threshold (default: -1.0)

   + CLOUD_OVERLAP=1                  set the cloud overlap scheme (default: 1)

   + CC_LOW_CLOUD_TOP=1100.           set the value of cc_low_cloud_top (default: 750hPa)

   + IR_SCATT_MODEL=1                 set the IR scattering model (default: 2/Chou-scaling)

   + VIS_SCATT_MODEL=1                set the visible/near-IR scattering model (default: 1/DOM)

   + DOM_NSTREAMS=16                  set the value of dom_nstreams (default: 8)

   + DOM_ACCURACY=1.E-4               set the value of dom_accuracy (default: 0.)

   + DOM_OPDEP_THRESH=10.             set value of dom_opdep_threshold (default: 0.)

   + DOM_RAYLEIGH=1                   include full Rayleigh multiple-scattering in solar DOM
                                      simulations (default: 0/false)

   + RAYLEIGH_DEPOL=1                 include depolarisation in Rayleigh multiple-scattering
                                      phase functions (default: 0/false)

   + ZERO_CLOUD=1                     for a cloudy simulation, set cloud concentrations or
                                      user cloud optical properties to zero; intended for
                                      testing clear-sky DOM Rayleigh (default: 0/false)

   + ZERO_AEROSOL=1                   for an aerosol simulation, set aerosol concentrations or
                                      user aerosol optical properties to zero; intended for
                                      testing clear-sky DOM Rayleigh (default: 0/false)

   + DO_LAMBERTIAN=1                  computes reflected downwelling radiation lambertian
                                      reflected instead of specular (default: 0/false)

   + LAMBERTIAN_FIXED_ANGLE=0         selects fixed/parameterised effective angle for lambertian
                                      option (default: 1/true)

   + IR_SEA_EMIS_MODEL=1              set the IR sea surface emissivity model (default: 2)

   + SOLAR_SEA_BRDF_MODEL=1           set the solar sea surface BRDF model (default: 2)

   + FASTEM_VERSION=5                 set the version of FASTEM to use for MW emissivity
                                      calculations (default: 6)

   + FASTEM3_RWD_FIX=0                set the fastem3_rwd_fix boolean (default: 1/true)

   + MW_CLW_SCHEME=3                  set the MW CLW absorption scheme (default: 2)

   + MW_CLW_CLOUD_TOP=300.            set the MW CLW cloud top variable (default: 322hPa)

   + SUPPLY_FOAM_FRACTION=1           sets the supply_foam_fraction boolean which controls
                                      whether FASTEM uses the input foam fraction value in
                                      the input profile or not (default: 0/false)

   + ADDINTERP=1                      sets the addinterp boolean. NB if the input pressure
                                      levels differ to the coef file levels then the RTTOV
                                      interpolator is switched on automatically by the
                                      test suite so this switch is not usually required
                                      (default: 0/false)

   + INTERP_MODE=5                    sets the interpolation mode; see user guide for valid
                                      settings; has no effect if interpolation is off
                                      (default: 1)

   + REG_LIMIT_EXTRAP=0               sets the reg_limit_extrap boolean which, if true,
                                      extrapolates the input profile at the top of the
                                      atmosphere using the regression limits
                                      (default: 1/true)

   + LGRADP=1                         sets the lgradp boolean which is used to include
                                      variations wrt pressure if the internal RTTOV
                                      interpolation is used (default: 0/false)

   + SPACETOP=0                       sets the spacetop boolean (default: 1/true)

   + USER_CHECK_OPTS=1                run rttov_user_options_checkinput to check
                                      consistency between input options and coefs
                                      (default: 0/false)

   + USER_CHECK_PROF=1                run rttov_user_profile_checkinput to check input
                                      profiles are within limits (default: 0/false)

   + GAS_UNITS=1                      sets the gas units RTTOV is run with; the test
                                      suite will convert from INPUT_GAS_UNITS if they are
                                      different. This option sets profiles(:) % gas_units.
                                      Valid settings:
                                        2 = ppmv over moist air
                                        1 = kg/kg over moist air
                                        0 = ppmv over dry air (negative values also work)
                                      The vast majority of test suite input profiles are
                                      in units of ppmv over moist air.
                                      (default: 2/ppmv over moist air)

   + INPUT_GAS_UNITS=1                specifies the gas units of the input test suite files.
                                      If unspecified on the commandline, the value of
                                      INPUT_GAS_UNITS is taken from gas_units.txt.
                                      If this file is not present, the default is ppmv over
                                      moist air. By specifying this on the commandline
                                      any value in gas_units.txt is over-ruled.
                                      If INPUT_GAS_UNITS differs from GAS_UNITS, the test
                                      suite will convert the input profiles before
                                      calling RTTOV. Valid settings:
                                        2 = ppmv over moist air
                                        1 = kg/kg over moist air
                                        0 = ppmv over dry air (negative values also work)
                                      NB RTTOV is always run with units GAS_UNITS so, for
                                      example, Jacobians are in units of GAS_UNITS rather
                                      than the units of the input files.
                                      (default: defined by gas_units.txt, otherwise
                                      2/ppmv over moist air)

   + FIX_HGPL=0                       sets the fix_hgpl boolean (default: 1/true)

   + DO_CHECKINPUT=0                  sets the do_checkinput boolean (default: 1/true)

   + APPLY_REG_LIMITS=1               sets the apply_reg_limits boolean (default: 0/false)

   + OPDEP13_GAS_CLIP=0               sets the opdep13_gas_clip boolean (default: 1/true)

   + VERBOSE=0                        sets the verbose boolean (default: 1/true)

   + RADAR=1                          activate an RTTOV-SCATT radar simulation (default: 0/false)

   + MULTI_HYDRO_FRAC=1               run test with per-hydrometeor cloud fraction profiles
                                      instead of a single cloud fraction profile
                                      (default: 0/false)

   + LUSERCFRAC=1                     sets lusercfrac boolean for RTTOV-SCATT (default: 0/false)

   + CC_THRESHOLD=0.01                sets the cc_threshold option for RTTOV-SCATT (default: 0.001)

   + ICE_POLARISATION=-1.             sets the ice_polarisation option for RTTOV-SCATT
                                      (default: 1.40) 

   + POL_MODE=2                       sets the polarisation mode for RTTOV-SCATT
                                      (default: 1/empirical scaling) 

   + HYDRO_CFRAC_TLAD=0               sets hydro_cfrac_tlad boolean for RTTOV-SCATT
                                      (default: 1/true)

   + ZERO_HYDRO_TLAD=1                sets zero_hydro_tlad boolean for RTTOV-SCATT
                                      (default: 0/false)

   + NO_FLUX_CONV=1                   sets flux_conversion to zero for RTTOV-SCATT
                                      (default: 0/false)

   + HTFRTC=1                         run HTFRTC model (default: 0/false)

   + HTFRTC_SIMPLE_CLOUD=1            sets the simple_cloud boolean for HTFRTC (default: 0/false)

   + HTFRTC_OVERCAST=1                sets the overcast boolean for HTFRTC (default: 0/false)

   + DO_OPDEP_CALC=0                  enable/disable RTTOV gas optical depth parameterisation
                                      (default: 1/true)

   + NO_OPT_PARAM_TLADK=1             run TL/AD/K simulations without cld/aer_opt_param as
                                      active TL/AD/K variables (default: 0/false)

   + PROF_BY_PROF=1                   run RTTOV a single profile at a time, NTHREADS is
                                      ignored (default: 0/false)

   + CHAN_BY_CHAN=1                   run RTTOV a single channel at a time, NTHREADS is
                                      ignored (default: 0/false)

   + EMIS=0.8                         if supplied sets calcemis to FALSE and all input
                                      surface emissivities are set to the given value
                                      (i.e. overrides emissivity.txt input file)

   + REFL=0.3                         if supplied sets calcrefl to FALSE and all input
                                      surface reflectances are set to the given value
                                      (i.e. overrides reflectance.txt input file)

   + DIFFUSE_REFL=0.3                 should be used with REFL, if supplied surface
                                      diffuse reflectances are set to the given value
                                      (i.e. overrides reflectance_diffuse.txt input file)
                                      Note that calcrefl is not affected.

   + SURFTYPE=0                       if supplied sets surftype to the given value
                                      for all profiles (i.e. overrides values from input
                                      profile data)

   + SPECULARITY=0.5                  if supplied sets specularity parameter to the given
                                      value for all profiles and channels (i.e. overrides
                                      values from input specularity.txt if present)

   + TSKIN_EFF_FROM_PROF=1            if true tskin_eff is taken from corresponding profile
                                      tskin (for testing purposes), if false tskin_eff
                                      is computed differently for each channel by
                                      perturbing the profile tskin; only applies when
                                      use_tksin_eff is true (default: 0/false)

   + ZENANG=50.                       if supplied sets zenith angle to the given value
                                      for all profiles (i.e. overrides values from input
                                      profile data)

   + SUNZENANG=50.                    if supplied sets solar zenith angle to the given
                                      value for all profiles (i.e. overrides values from
                                      input profile data)

   + FIX2M=1                          if supplied sets the 2m p, T, q and o3 values to
                                      those from the bottom level in the input profile
                                      (i.e. overrides 2m values from input profile data)

   + VISIR_CLW_SCHEME=1               if supplied sets profile clw_scheme variable to the
                                      given value for all profiles (i.e. overrides values
                                      from input profile data)

   + VISIR_ICE_SCHEME=1               if supplied sets profile ice_scheme variable to the
                                      given value for all profiles (i.e. overrides values
                                      from input profile data)

   + CFRACTION=0.                     if supplied sets cfraction (simple cloud scheme cloud
                                      fraction) to the given  value for all profiles (i.e.
                                      overrides values from input profile data)

   + {GAS}_MAX_PPMV=400.              specify max ppmv scaling for optional trace gases.
                                      Uses rttov_scale_ref_gas_prof subroutine to create
                                      gas profile(s) from RTTOV background profiles.

   + {GAS}_COL_INT=5.E-3              specify column-integrated scaling in kg/m^2 for
                                      optional trace gases. Uses rttov_scale_ref_gas_prof
                                      subroutine to create gas profile(s) from RTTOV
                                      background profiles.

   + O3_COL_INT_DU=300.               specify column-integrated scaling in Dobson units for
                                      ozone profile. Uses rttov_scale_ref_gas_prof subroutine
                                      to create ozone profile(s) from RTTOV background profile.

   + PACK=directory-name

   + UNPACK=directory-name

   + DR_HOOK=1                        activates DR_HOOK (RTTOV has to be compiled with
                                      DR_HOOK library, default: 0/false)

   + FTRACE=1                         activates FTRACE (RTTOV has to be compiled with
                                      -ftrace option, default: 0/false)

   + MEMCHECK=1                       run RTTOV through valgrind's memcheck tool with
                                      some default options for memory leak checking.
                                      Valgrind must be installed on your system
                                      (default: 0/false)

   + MASSIF=1                         run RTTOV through valgrind's massif tool with
                                      some default options for testing peak memory
                                      usage. Valgrind must be installed on your
                                      system (default: 0/false)

   + ALLMOD=1                         shortcut: run all models
                                      (same as DIRECT=1 TL=1 AD=1 K=1)

   + KCONS=1                          shortcut: run K consistency tests
                                      (same as K=1 K_TL=1 K_AD=1)

   + CEU=1                            shortcut: extract coefficients to an unformatted
                                      file (same as COEF_EXTRACT=1, COEF_FORMAT=unformatted)

   + CEH=1                            shortcut: extract coefficients to an HDF5
                                      file (same as COEF_EXTRACT=1, COEF_FORMAT=hdf5)

   + CPU=1                            shortcut: turn off output for timing testing
                                      (same as PRINT=0, VERBOSE=0)

EOF

}

#
# These two routines unpack test data from a single file
#

sub test_unpack {
  use Cwd;
  use File::Path;
  use Data::Dumper;
  use File::Basename;

  my $dir = shift;
  my $cwd = &cwd();

  mkpath( $dir );
  chdir( $dir );

  my $F = do "$cwd/$dir.pack";
  $@ && die( "Cannot parse $cwd/$dir.pack: $@\n" );

  while( my ( $f, $txt ) = splice( @$F, 0, 2 ) ) {
    mkpath( dirname( $f ) );
    'FileHandle'->new( ">$f" )->print( $txt );
  }

  chdir( $cwd );

  unlink( "$dir.pack" );

}

sub rttov_unpack {
  my $args = shift;

  my ( $t0, $cwd ) = ( $OPTIONS{UNPACK}, &cwd() );

  my @packs;
  if( $OPTIONS{TEST_LIST} ) {
    @packs = split( m/,/o, $OPTIONS{TEST_LIST} );
  } else {
    my $wanted = sub {
      return unless( m/\.pack$/o );
      s,^\./,,o;
      s,\.pack$,,o;
      push @packs, $_;
    };
    chdir( $t0 );
    find( { wanted => $wanted, no_chdir => 1 }, '.' );
    chdir( $cwd );
    @packs = sort @packs;
  }

  for my $pack ( @packs ) {
     &test_unpack( "$t0/$pack" );
  }
  
  exit( 0 );
}


#
# These two routines pack test data into a single file
#

sub test_pack {
  use Cwd;
  use File::Path;
  use Data::Dumper;

  my $dir = shift;
  my $cwd = &cwd();

  chdir( $dir );

  my @f;
  find( { wanted => sub { 
      my $f = $File::Find::name;
      ( -f $f ) && push( @f, $f );
    }, no_chdir => 1 }, '.' );

  my @F;
  for my $f ( sort @f ) {
    push @F, ( $f, do { my $fh = 'FileHandle'->new( "<$f" ); local $/ = undef; <$fh> } );
    unlink( $f );
  }
  chdir( $cwd );

  rmtree( $dir );
  'FileHandle'->new( ">$dir.pack" )->print( Dumper( \@F ) );

}

sub rttov_pack {
  my $args = shift;

  my ( $t0, $cwd ) = ( $OPTIONS{PACK}, &cwd() );

  my @packs;
  if( $OPTIONS{TEST_LIST} ) {
    @packs = split( m/,/o, $OPTIONS{TEST_LIST} );
  } else {
    my $wanted = sub {
      return unless( -d && -d "$_/in" );
      s,^\./,,o;
      push @packs, $_;
    };
    chdir( $t0 );
    find( { wanted => $wanted, no_chdir => 1 }, '.' );
    chdir( $cwd );
    @packs = sort @packs;
  }

  for my $pack ( @packs ) {
    &test_pack( "$t0/$pack" );
  }
  
  exit( 0 );
}



sub rttov_setup {
  my $args = shift;

  $OPTIONS{CWD} = &Cwd::cwd();
  $ENV{RTTOV_PREFIX} = dirname( my $cwd = cwd() );
  $OPTIONS{ARCH} ||= $ENV{ARCH};
  $OPTIONS{SESSION} ||= 'tests';
  $OPTIONS{SCHED_CMD} ||= '';

  my $ARCH = $OPTIONS{ARCH};
  my $session = $OPTIONS{SESSION};
  
  my ( $t1, $c1 );
  


  $args->{F} = my $F = 'flogm'->new( fhs => [ 'flog'->new( fh => \*STDOUT ) ] );

  if( ! $OPTIONS{LIST_ONLY} ) {
    $ARCH or die( "ARCH environment variable is not set\n" );
    ( $t1, $c1 ) = ( "$session.1.$ARCH", "coefs.1.$ARCH" );
  
    mkdir( $t1 );
  
    unless( $args->{F} ) {
      $F = 'flogm'->new( fhs => [ 
        'flog'->new( fh => \*STDOUT ),
        'flog'->new( fh => 'FileHandle'->new( ">$t1/test.log" ) ),
      ] );
      $args->{F} = $F;
    }
  
  
    if( ! exists( $OPTIONS{BIN} ) ) {
      if( -f "$ENV{RTTOV_PREFIX}/bin/rttov_test.exe" ) {
        $OPTIONS{BIN} = "bin";
      }
    } elsif( -f "$ENV{RTTOV_PREFIX}/$OPTIONS{BIN}/rttov_test.exe" ) {
    } else {
      die( "Cannot find rttov_test.exe\n" );
    }
  
    unless( $OPTIONS{BIN} && ( -f "$ENV{RTTOV_PREFIX}/$OPTIONS{BIN}/rttov_test.exe" ) ) {
      die( "Cannot find rttov_test.exe\n" );
    }
  
  }

  # load ARCH environment variables
  my $env = "$ENV{RTTOV_PREFIX}/rttov_test/arch/$ARCH";
  if (-f $env)
    {
      my %env = do $env;
      my $c = $@;
      if ($c)
        {
          die ("Cannot load `arch/$ARCH' : $@");
        }
      %ENV = (%ENV, %env);
      %::ARCH_ENV = %env;
    }

  $OPTIONS{RUN} = ! $OPTIONS{LIST_ONLY};

  unless ( exists $OPTIONS{PRINT_ERROR} )             { $OPTIONS{PRINT_ERROR} = 1 }
  unless ( exists $OPTIONS{PRINT} )                   { $OPTIONS{PRINT} = 1 }
  unless ( exists $OPTIONS{CHECK} )                   { $OPTIONS{CHECK} = 1 }

  unless ( exists $OPTIONS{CLOUD_OVERLAP} )           { $OPTIONS{CLOUD_OVERLAP} = 1 }
  unless ( exists $OPTIONS{CLDCOL_THRESHOLD} )        { $OPTIONS{CLDCOL_THRESHOLD} = -1.0 }
  unless ( exists $OPTIONS{CC_LOW_CLOUD_TOP} )        { $OPTIONS{CC_LOW_CLOUD_TOP} = 750.0 }
  unless ( exists $OPTIONS{DOM_ACCURACY} )            { $OPTIONS{DOM_ACCURACY} = 0. }
  unless ( exists $OPTIONS{DOM_OPDEP_THRESH})         { $OPTIONS{DOM_OPDEP_THRESH} = 0. }
  unless ( exists $OPTIONS{GRID_BOX_AVG_CLOUD} )      { $OPTIONS{GRID_BOX_AVG_CLOUD} = 1 }

  unless ( exists $OPTIONS{RAYLEIGH_MAX_WAVELENGTH} ) { $OPTIONS{RAYLEIGH_MAX_WAVELENGTH} = 2. }
  unless ( exists $OPTIONS{RAYLEIGH_MIN_PRESSURE} )   { $OPTIONS{RAYLEIGH_MIN_PRESSURE} = 0. }
  unless ( exists $OPTIONS{RAYLEIGH_SINGLE_SCATT} )   { $OPTIONS{RAYLEIGH_SINGLE_SCATT} = 1 }
  unless ( exists $OPTIONS{SOLAR_SEA_BRDF_MODEL} )    { $OPTIONS{SOLAR_SEA_BRDF_MODEL} = 2 }
  unless ( exists $OPTIONS{IR_SEA_EMIS_MODEL} )       { $OPTIONS{IR_SEA_EMIS_MODEL} = 2 }
  unless ( exists $OPTIONS{FASTEM_VERSION} )          { $OPTIONS{FASTEM_VERSION} = 6 }
  unless ( exists $OPTIONS{FASTEM3_RWD_FIX} )         { $OPTIONS{FASTEM3_RWD_FIX} = 1 }
  unless ( exists $OPTIONS{MW_CLW_SCHEME} )           { $OPTIONS{MW_CLW_SCHEME} = 2 }
  unless ( exists $OPTIONS{MW_CLW_CLOUD_TOP} )        { $OPTIONS{MW_CLW_CLOUD_TOP} = 322.0 }
  unless ( exists $OPTIONS{USE_T2M_OPDEP} )           { $OPTIONS{USE_T2M_OPDEP} = 1 }
  unless ( exists $OPTIONS{USE_Q2M} )                 { $OPTIONS{USE_Q2M} = 1 }
  unless ( exists $OPTIONS{RAD_DOWN_LIN_TAU} )        { $OPTIONS{RAD_DOWN_LIN_TAU} = 1 }
  unless ( exists $OPTIONS{REFRACTION} )              { $OPTIONS{REFRACTION} = 1 }
  unless ( exists $OPTIONS{REG_LIMIT_EXTRAP} )        { $OPTIONS{REG_LIMIT_EXTRAP} = 1 }
  unless ( exists $OPTIONS{LAMBERTIAN_FIXED_ANGLE} )  { $OPTIONS{LAMBERTIAN_FIXED_ANGLE} = 1 }
  unless ( exists $OPTIONS{OPDEP13_GAS_CLIP} )        { $OPTIONS{OPDEP13_GAS_CLIP} = 1 }
  unless ( exists $OPTIONS{FIX_HGPL} )                { $OPTIONS{FIX_HGPL} = 1 }
  unless ( exists $OPTIONS{SPACETOP} )                { $OPTIONS{SPACETOP} = 1 }
  unless ( exists $OPTIONS{VERBOSE} )                 { $OPTIONS{VERBOSE} = 1 }
  unless ( exists $OPTIONS{DO_CHECKINPUT} )           { $OPTIONS{DO_CHECKINPUT} = 1 }
  unless ( exists $OPTIONS{CC_THRESHOLD} )            { $OPTIONS{CC_THRESHOLD} = 0.001 }
  unless ( exists $OPTIONS{ICE_POLARISATION} )        { $OPTIONS{ICE_POLARISATION} = 1.40 }
  unless ( exists $OPTIONS{POL_MODE} )                { $OPTIONS{POL_MODE} = 1 }
  unless ( exists $OPTIONS{HYDRO_CFRAC_TLAD} )        { $OPTIONS{HYDRO_CFRAC_TLAD} = 1 }

  unless ( exists $OPTIONS{SWITCHRAD} )               { $OPTIONS{SWITCHRAD} = 1 }

  unless ( exists $OPTIONS{INPUT_GAS_UNITS} )         { $OPTIONS{INPUT_GAS_UNITS} = -1 }
  unless ( exists $OPTIONS{GAS_UNITS}       )         { $OPTIONS{GAS_UNITS} = 2 }

  unless ( exists $OPTIONS{DO_OPDEP_CALC} )           { $OPTIONS{DO_OPDEP_CALC} = 1 }

  $OPTIONS{DOM_NSTREAMS}      ||= 8;
  $OPTIONS{IR_SCATT_MODEL}    ||= 2;
  $OPTIONS{VIS_SCATT_MODEL}   ||= 1;

  $OPTIONS{INTERP_MODE} ||= 1;
  $OPTIONS{NTHREADS}    ||= 0;
  $OPTIONS{NTIMES}      ||= 1;
  $OPTIONS{MULT}        ||= 1;
  $OPTIONS{SCALE_INC}   ||= 1;
  $OPTIONS{SCALE_OUT}   ||= 1;
  $OPTIONS{TEST_MATCH}  ||= '^.*$';

  $OPTIONS{TINYABS}     ||= 1.e-11;
  $OPTIONS{TINYREL}     ||= 1.e-5;

  if( $OPTIONS{PROF_BY_PROF} || $OPTIONS{CHAN_BY_CHAN} ) { $OPTIONS{NTHREADS} = 0 }

  # Define short cut options
  if( $OPTIONS{ALLMOD} ) {                  # Run direct, TL, AD and K
    $OPTIONS{DIRECT} = 1;
    $OPTIONS{TL} = 1;
    $OPTIONS{AD} = 1;
    $OPTIONS{K} = 1;
  }
  if( $OPTIONS{KCONS} ) {                   # Run K consistency tests (K, K_TL, K_AD)
    $OPTIONS{K_TL} = 1;
    $OPTIONS{K_AD} = 1;
    $OPTIONS{K} = 1;
  }
  if( $OPTIONS{CEU} ) {                     # Extract coefs to unformatted file
    $OPTIONS{COEF_EXTRACT} = 1;
    $OPTIONS{COEF_FORMAT} = 'unformatted';
  }
  if( $OPTIONS{CEH} ) {                     # Extract coefs to HDF5 file
    $OPTIONS{COEF_EXTRACT} = 1;
    $OPTIONS{COEF_FORMAT} = 'hdf5';
  }
  if( $OPTIONS{CPU} ) {                     # Run a timing test - no printed output
    $OPTIONS{PRINT} = 0;
    $OPTIONS{VERBOSE} = 0;
  }

  # Some options imply other option settings
  if( $OPTIONS{TAYLOR_BY_CHAN} || $OPTIONS{TAYLOR_ON_BTREFL} ) {
    $OPTIONS{TAYLOR} = 1;
  }

  if( $OPTIONS{COEF_EXTRACT} ) {
    $OPTIONS{LALLCHANS} = 0;
    $OPTIONS{COEF_FORMAT} ||= 'formatted';
  } else {
    $OPTIONS{COEF_FORMAT} = '.';            # Not relevant if COEF_EXTRACT is 0
  }

  if( $OPTIONS{HTFRTC} ) {                  # Some options are not compatible/relevant with HTFRTC
    $OPTIONS{COEF_EXTRACT} = 0;
    $OPTIONS{COEF_FORMAT} = '.';
  }

  $OPTIONS{REALPREC} ||= 6;

  @{$args}{ qw( t1 c1 ) } = ( $t1, $c1 );
}


sub rttov_run {
  my $args = shift;


  my ( $F, $t1, $c1 ) = @{$args}{ qw( F t1 c1 ) };

  my $cwd = &Cwd::cwd();
  my ( $t0, $c0 ) = ( 'tests.0', $ENV{RTTOV_PREFIX} );
  
  my @packs;
  
  
  my $wanted = sub {
    return unless( -d && -d "$_/in" );
    s,^\./,,o;
    push @packs, $_;
  };

  
  my $test_match = qr/$OPTIONS{TEST_MATCH}/o;
  
  if( $OPTIONS{TEST_LIST} ) {
    @packs = split( m/,/o, $OPTIONS{TEST_LIST} );
  } else {
    chdir( $t0 );
    find( { wanted => $wanted, no_chdir => 1 }, '.' );
    chdir( $cwd );
    @packs = sort @packs;
  }
  
  
  my @T;
  
  TEST_LOOP: for my $pack ( @packs ) {
  
    my @t = split( m/\+/o, $pack );
    for my $t ( @t ) {
      next TEST_LOOP unless( $t =~ $test_match );
    }
    my $T = 'rttov::test::pack'->new( 
      name => $pack, 
      base0 => $t0, base1 => $t1, 
      coef0 => $c0, coef1 => $c1 
    );
    
    $T->line1( $F );
  
    if( $::OPTIONS{RUN} ) {
    
      $F->print( "     ..." );
  
      if( $T->can_run() ) {
        if( $::OPTIONS{COEF_EXTRACT} ) {
          $T->coef_extract() && $T->run() && ( ( ! $OPTIONS{CHECK} ) || $T->check() );
        } else {
          $T->run() && ( ( ! $OPTIONS{CHECK} ) || $T->check() );
        }
      } else {
        $T->skip();
      }
      
      $F->back( 8 );
      
      $T->line2( $F );
      
      
    }
    $F->print( "\n" );
  
    if( $OPTIONS{PRINT_ERROR} && defined( $T->{status}{status} ) 
    && ( $T->{status}{status} =~ m/^(?:FAIL|DIFF)$/o ) ) {
  
      $F->print( "\n" );
  
      use Text::Wrap;
      my ( $w, $m ) = ( 90, 20 );
      my $ma = ( ' ' x $m ) . '| ';
      local $Text::Wrap::columns = $w + $m;
      my $text = $T->{status}{log};
      chomp( $text );
      $F->print( wrap( $ma, $ma, $text ) );
      $F->print( "\n" );
      $F->print( ( ' ' x $m ) . '+' . ( '-' x ( $w - 1 ) ) );
      $F->print( "\n\n" );
  
      if( $pack ne $packs[-1] ) {
        &rttov::test::util::line( 0, 0, $F, \@F1 );
        &rttov::test::util::line( 0, 0, $F, \@F2 );
        $F->print( "\n" );
      }
  
    }
    
    push @T, $T;
  }
  
  

  if( $OPTIONS{RUN} ) {
    $args->{status} ||= { };

    my $status = $args->{status};
    for my $T ( @T ) {
      $status->{$T->{status}{status}}++;
    }
  }
  
}


sub setoptions {
  for my $argv ( @_ ) {
    my ( $var, $val ) = split( m/=/o, $argv );
    $OPTIONS{$var} = $val;
  }
}

&setoptions( @ARGV );

my $args = { };

if( $OPTIONS{HELP} || ! scalar @ARGV ) {
  &rttov_help( $args );
}

if( $OPTIONS{PACK} ) {
  &rttov_pack( $args );
}

if( $OPTIONS{UNPACK} ) {
  &rttov_unpack( $args );
}


my %options = %OPTIONS;

&rttov_setup( $args );


&rttov_head( $args );

if( $ARGV[-1] eq '--' ) {
  @ARGV = ();
  while( defined( my $line = <> ) ) {
    chomp( $line );
    $line =~ s/(?:^\s*|\s*$)//o;
    %OPTIONS = %options;
    setoptions( split( m/\s+/o, $line ) );
    &rttov_setup( $args );
    &rttov_run( $args );
  }
} else {
  &rttov_run( $args );
}

&rttov_tail( $args );


# this is the MD5 code; stolen from CPAN

package Digest::Perl::MD5;
use strict;
use integer;
use Exporter;
use vars qw($VERSION @ISA @EXPORTER @EXPORT_OK);

@EXPORT_OK = qw(md5 md5_hex md5_base64);

@ISA = 'Exporter';
$VERSION = '1.6';

# I-Vektor
sub A() { 0x67_45_23_01 }
sub B() { 0xef_cd_ab_89 }
sub C() { 0x98_ba_dc_fe }
sub D() { 0x10_32_54_76 }

# for internal use
sub MAX() { 0xFFFFFFFF }

# padd a message to a multiple of 64
sub padding {
    my $l = length (my $msg = shift() . chr(128));    
    $msg .= "\0" x (($l%64<=56?56:120)-$l%64);
    $l = ($l-1)*8;
    $msg .= pack 'VV', $l & MAX , ($l >> 16 >> 16);
}


sub rotate_left($$) {
	#$_[0] << $_[1] | $_[0] >> (32 - $_[1]);
	#my $right = $_[0] >> (32 - $_[1]);
	#my $rmask = (1 << $_[1]) - 1;
	($_[0] << $_[1]) | (( $_[0] >> (32 - $_[1])  )  & ((1 << $_[1]) - 1));
	#$_[0] << $_[1] | (($_[0]>> (32 - $_[1])) & (1 << (32 - $_[1])) - 1);
}

sub gen_code {
  # Discard upper 32 bits on 64 bit archs.
  my $MSK = ((1 << 16) << 16) ? ' & ' . MAX : '';
#	FF => "X0=rotate_left(((X1&X2)|(~X1&X3))+X0+X4+X6$MSK,X5)+X1$MSK;",
#	GG => "X0=rotate_left(((X1&X3)|(X2&(~X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
  my %f = (
	FF => "X0=rotate_left((X3^(X1&(X2^X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
	GG => "X0=rotate_left((X2^(X3&(X1^X2)))+X0+X4+X6$MSK,X5)+X1$MSK;",
	HH => "X0=rotate_left((X1^X2^X3)+X0+X4+X6$MSK,X5)+X1$MSK;",
	II => "X0=rotate_left((X2^(X1|(~X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
  );
  #unless ( (1 << 16) << 16) { %f = %{$CODES{'32bit'}} }
  #else { %f = %{$CODES{'64bit'}} }

  my %s = (  # shift lengths
	S11 => 7, S12 => 12, S13 => 17, S14 => 22, S21 => 5, S22 => 9, S23 => 14,
	S24 => 20, S31 => 4, S32 => 11, S33 => 16, S34 => 23, S41 => 6, S42 => 10,
	S43 => 15, S44 => 21
  );

  my $insert = "";
  while(<DATA>) {
	chomp;
	next unless /^[FGHI]/;
	my ($func,@x) = split /,/;
	my $c = $f{$func};
	$c =~ s/X(\d)/$x[$1]/g;
	$c =~ s/(S\d{2})/$s{$1}/;
        $c =~ s/^(.*)=rotate_left\((.*),(.*)\)\+(.*)$//;

	#my $rotate = "(($2 << $3) || (($2 >> (32 - $3)) & (1 << $2) - 1)))"; 
	$c = "\$r = $2;
        $1 = ((\$r << $3) | ((\$r >> (32 - $3))  & ((1 << $3) - 1))) + $4";
	$insert .= "\t$c\n";
  }
  close DATA;
  
  my $dump = '
  sub round {
	my ($a,$b,$c,$d) = @_[0 .. 3];
	my $r;

	' . $insert . '
	$_[0]+$a' . $MSK . ', $_[1]+$b ' . $MSK . 
        ', $_[2]+$c' . $MSK . ', $_[3]+$d' . $MSK . ';
  }';
  eval $dump;
  #print "$dump\n";
  #exit 0;
}

#########################################
# Private output converter functions:
sub _encode_hex { unpack 'H*', $_[0] }
sub _encode_base64 {
	my $res;
	while ($_[0] =~ /(.{1,45})/gs) {
		$res .= substr pack('u', $1), 1;
		chop $res;
	}
	$res =~ tr|` -_|AA-Za-z0-9+/|;#`
	chop $res; chop $res;
	$res
}

#########################################
# OOP interface:
sub new {
	my $proto = shift;
	my $class = ref $proto || $proto;
	my $self = {};
	bless $self, $class;
	$self->reset();
	$self
}

sub reset {
	my $self = shift;
	delete $self->{_data};
	$self->{_state} = [A,B,C,D];
	$self->{_length} = 0;
	$self
}

sub add {
	my $self = shift;
	$self->{_data} .= join '', @_ if @_;
	my ($i,$c);
	for $i (0 .. (length $self->{_data})/64-1) {
		my @X = unpack 'V16', substr $self->{_data}, $i*64, 64;
		@{$self->{_state}} = round(@{$self->{_state}},@X);
		++$c;
	}
	if ($c) {
		substr $self->{_data}, 0, $c*64, '';
		$self->{_length} += $c*64;
	}
	$self
}

sub finalize {
	my $self = shift;
	$self->{_data} .= chr(128);
    my $l = $self->{_length} + length $self->{_data};
    $self->{_data} .= "\0" x (($l%64<=56?56:120)-$l%64);
    $l = ($l-1)*8;
    $self->{_data} .= pack 'VV', $l & MAX , ($l >> 16 >> 16);
	$self->add();
	$self
}

sub addfile {
  	my ($self,$fh) = @_;
	if (!ref($fh) && ref(\$fh) ne "GLOB") {
	    require Symbol;
	    $fh = Symbol::qualify($fh, scalar caller);
	}
	# $self->{_data} .= do{local$/;<$fh>};
	my $read = 0;
	my $buffer = '';
	$self->add($buffer) while $read = read $fh, $buffer, 8192;
	die __PACKAGE__, " read failed: $!" unless defined $read;
	$self
}

sub add_bits {
}

sub digest {
	my $self = shift;
	$self->finalize();
	my $res = pack 'V4', @{$self->{_state}};
	$self->reset();
	$res
}

sub hexdigest {
	_encode_hex($_[0]->digest)
}

sub b64digest {
	_encode_base64($_[0]->digest)
}

sub clone {
	my $self = shift;
	my $clone = { 
		_state => [@{$self->{_state}}],
		_length => $self->{_length},
		_data => $self->{_data}
	};
	bless $clone, ref $self || $self;
}

#########################################
# Procedural interface:
sub md5 {
	my $message = padding(join'',@_);
	my ($a,$b,$c,$d) = (A,B,C,D);
	my $i;
	for $i (0 .. (length $message)/64-1) {
		my @X = unpack 'V16', substr $message,$i*64,64;	
		($a,$b,$c,$d) = round($a,$b,$c,$d,@X);
	}
	pack 'V4',$a,$b,$c,$d;
}
sub md5_hex { _encode_hex &md5 } 
sub md5_base64 { _encode_base64 &md5 }


1;

=head1 NAME

Digest::MD5::Perl - Perl implementation of Ron Rivests MD5 Algorithm

=head1 DISCLAIMER

This is B<not> an interface (like C<Digest::MD5>) but a Perl implementation of MD5.
It is written in perl only and because of this it is slow but it works without C-Code.
You should use C<Digest::MD5> instead of this module if it is available.
This module is only usefull for

=over 4

=item

computers where you cannot install C<Digest::MD5> (e.g. lack of a C-Compiler)

=item

encrypting only small amounts of data (less than one million bytes). I use it to
hash passwords.

=item

educational purposes

=back

=head1 SYNOPSIS

 # Functional style
 use Digest::MD5  qw(md5 md5_hex md5_base64);

 $hash = md5 $data;
 $hash = md5_hex $data;
 $hash = md5_base64 $data;
    

 # OO style
 use Digest::MD5;

 $ctx = Digest::MD5->new;

 $ctx->add($data);
 $ctx->addfile(*FILE);

 $digest = $ctx->digest;
 $digest = $ctx->hexdigest;
 $digest = $ctx->b64digest;

=head1 DESCRIPTION

This modules has the same interface as the much faster C<Digest::MD5>. So you can
easily exchange them, e.g.

	BEGIN {
	  eval {
	    require Digest::MD5;
	    import Digest::MD5 'md5_hex'
	  };
	  if ($@) { # ups, no Digest::MD5
	    require Digest::Perl::MD5;
	    import Digest::Perl::MD5 'md5_hex'
	  }		
	}

If the C<Digest::MD5> module is available it is used and if not you take
C<Digest::Perl::MD5>.

You can also install the Perl part of Digest::MD5 together with Digest::Perl::MD5
and use Digest::MD5 as normal, it falls back to Digest::Perl::MD5 if it
cannot load its object files.

For a detailed Documentation see the C<Digest::MD5> module.

=head1 EXAMPLES

The simplest way to use this library is to import the md5_hex()
function (or one of its cousins):

    use Digest::Perl::MD5 'md5_hex';
    print 'Digest is ', md5_hex('foobarbaz'), "\n";

The above example would print out the message

    Digest is 6df23dc03f9b54cc38a0fc1483df6e21

provided that the implementation is working correctly.  The same
checksum can also be calculated in OO style:

    use Digest::MD5;
    
    $md5 = Digest::MD5->new;
    $md5->add('foo', 'bar');
    $md5->add('baz');
    $digest = $md5->hexdigest;
    
    print "Digest is $digest\n";

The digest methods are destructive. That means you can only call them
once and the $md5 objects is reset after use. You can make a copy with clone:

	$md5->clone->hexdigest

=head1 LIMITATIONS

This implementation of the MD5 algorithm has some limitations:

=over 4

=item

It's slow, very slow. I've done my very best but Digest::MD5 is still about 100 times faster.
You can only encrypt Data up to one million bytes in an acceptable time. But it's very usefull
for encrypting small amounts of data like passwords.

=item

You can only encrypt up to 2^32 bits = 512 MB on 32bit archs. But You should
use C<Digest::MD5> for those amounts of data anyway.

=back

=head1 SEE ALSO

L<Digest::MD5>

L<md5(1)>

RFC 1321

tools/md5: a small BSD compatible md5 tool written in pure perl.

=head1 COPYRIGHT

This library is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

 Copyright 2000 Christian Lackas, Imperia Software Solutions
 Copyright 1998-1999 Gisle Aas.
 Copyright 1995-1996 Neil Winton.
 Copyright 1991-1992 RSA Data Security, Inc.

The MD5 algorithm is defined in RFC 1321. The basic C code
implementing the algorithm is derived from that in the RFC and is
covered by the following copyright:

=over 4

=item

Copyright (C) 1991-1992, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software.

=back

This copyright does not prohibit distribution of any version of Perl
containing this extension under the terms of the GNU or Artistic
licenses.

=head1 AUTHORS

The original MD5 interface was written by Neil Winton
(<N.Winton (at) axion.bt.co.uk>).

C<Digest::MD5> was made by Gisle Aas <gisle (at) aas.no> (I took his Interface
and part of the documentation).

Thanks to Guido Flohr for his 'use integer'-hint.

This release was made by Christian Lackas <delta (at) lackas.net>.

=cut

__DATA__
FF,$a,$b,$c,$d,$_[4],7,0xd76aa478,/* 1 */
FF,$d,$a,$b,$c,$_[5],12,0xe8c7b756,/* 2 */
FF,$c,$d,$a,$b,$_[6],17,0x242070db,/* 3 */
FF,$b,$c,$d,$a,$_[7],22,0xc1bdceee,/* 4 */
FF,$a,$b,$c,$d,$_[8],7,0xf57c0faf,/* 5 */
FF,$d,$a,$b,$c,$_[9],12,0x4787c62a,/* 6 */
FF,$c,$d,$a,$b,$_[10],17,0xa8304613,/* 7 */
FF,$b,$c,$d,$a,$_[11],22,0xfd469501,/* 8 */
FF,$a,$b,$c,$d,$_[12],7,0x698098d8,/* 9 */
FF,$d,$a,$b,$c,$_[13],12,0x8b44f7af,/* 10 */
FF,$c,$d,$a,$b,$_[14],17,0xffff5bb1,/* 11 */
FF,$b,$c,$d,$a,$_[15],22,0x895cd7be,/* 12 */
FF,$a,$b,$c,$d,$_[16],7,0x6b901122,/* 13 */
FF,$d,$a,$b,$c,$_[17],12,0xfd987193,/* 14 */
FF,$c,$d,$a,$b,$_[18],17,0xa679438e,/* 15 */
FF,$b,$c,$d,$a,$_[19],22,0x49b40821,/* 16 */ 
GG,$a,$b,$c,$d,$_[5],5,0xf61e2562,/* 17 */
GG,$d,$a,$b,$c,$_[10],9,0xc040b340,/* 18 */
GG,$c,$d,$a,$b,$_[15],14,0x265e5a51,/* 19 */
GG,$b,$c,$d,$a,$_[4],20,0xe9b6c7aa,/* 20 */
GG,$a,$b,$c,$d,$_[9],5,0xd62f105d,/* 21 */
GG,$d,$a,$b,$c,$_[14],9,0x2441453,/* 22 */
GG,$c,$d,$a,$b,$_[19],14,0xd8a1e681,/* 23 */
GG,$b,$c,$d,$a,$_[8],20,0xe7d3fbc8,/* 24 */
GG,$a,$b,$c,$d,$_[13],5,0x21e1cde6,/* 25 */
GG,$d,$a,$b,$c,$_[18],9,0xc33707d6,/* 26 */
GG,$c,$d,$a,$b,$_[7],14,0xf4d50d87,/* 27 */
GG,$b,$c,$d,$a,$_[12],20,0x455a14ed,/* 28 */
GG,$a,$b,$c,$d,$_[17],5,0xa9e3e905,/* 29 */
GG,$d,$a,$b,$c,$_[6],9,0xfcefa3f8,/* 30 */
GG,$c,$d,$a,$b,$_[11],14,0x676f02d9,/* 31 */
GG,$b,$c,$d,$a,$_[16],20,0x8d2a4c8a,/* 32 */
HH,$a,$b,$c,$d,$_[9],4,0xfffa3942,/* 33 */
HH,$d,$a,$b,$c,$_[12],11,0x8771f681,/* 34 */
HH,$c,$d,$a,$b,$_[15],16,0x6d9d6122,/* 35 */
HH,$b,$c,$d,$a,$_[18],23,0xfde5380c,/* 36 */
HH,$a,$b,$c,$d,$_[5],4,0xa4beea44,/* 37 */
HH,$d,$a,$b,$c,$_[8],11,0x4bdecfa9,/* 38 */
HH,$c,$d,$a,$b,$_[11],16,0xf6bb4b60,/* 39 */
HH,$b,$c,$d,$a,$_[14],23,0xbebfbc70,/* 40 */
HH,$a,$b,$c,$d,$_[17],4,0x289b7ec6,/* 41 */
HH,$d,$a,$b,$c,$_[4],11,0xeaa127fa,/* 42 */
HH,$c,$d,$a,$b,$_[7],16,0xd4ef3085,/* 43 */
HH,$b,$c,$d,$a,$_[10],23,0x4881d05,/* 44 */
HH,$a,$b,$c,$d,$_[13],4,0xd9d4d039,/* 45 */
HH,$d,$a,$b,$c,$_[16],11,0xe6db99e5,/* 46 */
HH,$c,$d,$a,$b,$_[19],16,0x1fa27cf8,/* 47 */
HH,$b,$c,$d,$a,$_[6],23,0xc4ac5665,/* 48 */
II,$a,$b,$c,$d,$_[4],6,0xf4292244,/* 49 */
II,$d,$a,$b,$c,$_[11],10,0x432aff97,/* 50 */
II,$c,$d,$a,$b,$_[18],15,0xab9423a7,/* 51 */
II,$b,$c,$d,$a,$_[9],21,0xfc93a039,/* 52 */
II,$a,$b,$c,$d,$_[16],6,0x655b59c3,/* 53 */
II,$d,$a,$b,$c,$_[7],10,0x8f0ccc92,/* 54 */
II,$c,$d,$a,$b,$_[14],15,0xffeff47d,/* 55 */
II,$b,$c,$d,$a,$_[5],21,0x85845dd1,/* 56 */
II,$a,$b,$c,$d,$_[12],6,0x6fa87e4f,/* 57 */
II,$d,$a,$b,$c,$_[19],10,0xfe2ce6e0,/* 58 */
II,$c,$d,$a,$b,$_[10],15,0xa3014314,/* 59 */
II,$b,$c,$d,$a,$_[17],21,0x4e0811a1,/* 60 */
II,$a,$b,$c,$d,$_[8],6,0xf7537e82,/* 61 */
II,$d,$a,$b,$c,$_[15],10,0xbd3af235,/* 62 */
II,$c,$d,$a,$b,$_[6],15,0x2ad7d2bb,/* 63 */
II,$b,$c,$d,$a,$_[13],21,0xeb86d391,/* 64 */
