#!/usr/bin/perl -w

use strict;
use File::Copy;

my $INCDIR = shift;

for (<*.h>)
  {
    copy ($_, "$INCDIR/$_");
  }

