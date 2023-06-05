#!/usr/bin/perl -w

use strict;
use FileHandle;
use File::Copy;
use File::Basename;

sub r {
  ( my $fh = 'FileHandle'->new( "<$_[0]" ) ) or return '';
  local $/ = undef; 
  return <$fh>;
}


my ( $mod1, $mod2 ) = @ARGV;
my ( $MOD1, $MOD2 ) = ( $mod1, $mod2 );

$MOD1 =~ s/^(\w+\.)/uc($1)/eo;
$MOD2 = dirname( $mod2 ) . '/' . basename( $MOD1 );

my $UC = 0;

my $bin1 = r( $mod1 );
my $bin2 = r( $mod2 );

if( $bin1 eq '' ) {
  $UC = 1;
  $bin1 = r( $MOD1 );
  $bin2 = r( $MOD2 );
}

if( $UC ) {
  &copy( $MOD1, $MOD2 );
  &copy( $MOD1, $mod2 );
  unlink( $MOD1 );
} else {
  &copy( $mod1, $mod2 );
  unlink( $mod1 );
}


