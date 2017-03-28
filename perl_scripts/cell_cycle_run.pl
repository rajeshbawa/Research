######################################################################
#This script runs the third party software through perl, and uses 10% noise 
#to produce 20 variable files from intitial input file
#
#Written by Rajesh K Bawa and Sandesh Shreshta
######################################################################

use warnings;
use strict;
	
# if javac and java are in the PATH we don't need to specify the full path,
# if that's not the case these two variables can be changed according to the local path 
my $JavaRun = 'java';
#this assumes that the input file is in the same directory of this perl script
my $className = "FinalExam_dataFile.csv";
my $software = "ThirdPartySoftware.jar";	
my $no_output = 20; #no of files one needs to generate
my @args_excecution = ("$JavaRun", "-jar", "$software", "$className", "10", "$no_output"); # 10% noise
system(@args_excecution) == 0 or die "system @args_excecution failed: $?";
	