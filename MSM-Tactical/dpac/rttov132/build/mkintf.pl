#!/usr/bin/perl -w

use strict;
use FileHandle;
use File::stat;

( scalar( @ARGV ) % 2 == 0 )
  or die( "Usage: $0 .F90 ... .interface ... \n" );

my $NARGV = scalar( @ARGV ) / 2;

for my $IARGV ( 1 .. $NARGV ) {
  my ( $F90, $intf ) = ( $ARGV[$IARGV-1], $ARGV[$NARGV+$IARGV-1] );

  my $F90_st  = stat ($F90);
  my $intf_st = (-f $intf) && stat ($intf); 

  next if( $intf_st && ( $intf_st->mtime () > $F90_st->mtime () ) );

  
  ( my $fhcode = 'FileHandle'->new( "<$F90" ) )
    or die( "Cannot open <$F90\n" );
   
  my @code;

  while( my $code = <$fhcode> ) {
    push @code, $code;
    last if( $code =~ m/^!INTF_END\s*$/o );
  }
  $fhcode->close();
  
  ( $code[-1] =~ m/^!INTF_END\s*$/o )
    or die( "$F90 does not contain any !INTF_END marker\n" );
  
  my @intf0;
  
  if( my $fhintf = 'FileHandle'->new( "<$intf" ) ) {
    @intf0 = <$fhintf>;
  } 
  
  
  my @intf1;
  
  for( @code ) {
    push @intf1, $_;
    last if( m/^\s*!\s*INTF_END\s*$/io );
  }
  

  my @intf2;

  my $on = 1;
  for( @intf1 )
    {
      if (m/^!INTF_ON\s$/o)
        {
          $on = 1;
          next;
        }
      if (m/^!INTF_OFF\s$/o)
        {
          $on = 0;
          next;
        }
      next unless ($on);
      push @intf2, $_;
    }

  @intf1 = grep { ! ( m/^\s*!\s*/o or m/^\s*$/o ) } @intf2;
  
  my $what;
  
  ( $intf1[0] =~ m/SUBROUTINE/io ) && ( $what = 'Subroutine' );
  ( $intf1[0] =~ m/FUNCTION/io ) && ( $what = 'Function' );
  
  $what 
    or die( "Cannot guess whether $F90 contains a function or a subroutine\n" );
  
  @intf1 = ( "Interface\n", @intf1, "End $what\n", "End Interface\n" );

  next if ("@intf1" eq "@intf0");
  
  ( my $fhintf = 'FileHandle'->new( ">$intf" ) )
    or die( "Cannot open >$intf\n" );
    
    
  $fhintf->print( join( '', @intf1 ) );

}




