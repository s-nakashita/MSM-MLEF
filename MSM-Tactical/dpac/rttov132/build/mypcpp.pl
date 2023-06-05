#!/usr/bin/perl -w
#
# use only to extract rttov_parallel routines
#
# macro starting with double # are ignored 
# by the extraction system and are
# written with only one # sign
# This is usefull for inserting excutive compiler macros

use strict;

my $D = shift;

$D =~ s/^-D//o;


my $p = 1;

while (<>)
  {
    m/^#ifdef\s+$D\s*$/o && do {
      $p = 1; 
      next;
    };
    m/^#ifdef/o && do {
      $p = 0;
      next;
    };
    m/^#ifndef\s+$D\s*$/o && do {
      $p = 0;
      next;
    };
    m/^#ifndef/o && do {
      $p = 1;
      next;
    };
    m/^#endif/o && do {
      $p = 1;
      next;
    };
    m/^##ifdef/o && do {
      s/##/#/o;
    };
    m/^##ifndef/o && do {
      s/##/#/o;
    };
    m/^##endif/o && do {
      s/##/#/o;
    };

    $p && print;
  }
