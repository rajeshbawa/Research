#!/usr/bin/perl

######################################################################
#This perl script runs R script written to generate plots for all 20 variant
#files and one intitial file. 
##
#Written by Rajesh K Bawa and Sandesh Shreshta
######################################################################

# running R file through system
use warnings;
use strict;
my $Rpath = '"C:\Program Files\R\R-2.15.0\bin\x64\R.exe"';
my $Rscript = "plotting_script.r";
my @args = ("$Rpath", "<", "$Rscript", "--no-save");
system(@args) == 0 or die "system @args failed: $?"
