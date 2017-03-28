#!/usr/bin/perl

######################################################################
#Main Script, which runs using CellCycleStats module and prints out basic 
#Cell cycle parameters and general statistics of each cell cycle. 
#
#Written by Rajesh K Bawa and Sandesh Shreshta
######################################################################

use warnings;
use strict;

use CellCycleStats; #calling the module

my $file = "FinalExam_dataFile.csv"; 
open(IN, "<$file") or die "Could not open the file!\n";

###########part one of first question
my $header = <IN>; #read the header of the file
chomp $header; 
#print $header, "\n";
my @line1 = split(/,/ , $header); #split header by commas and put it into array
my $columns = scalar(@line1); #count number of elements in an array to find number of columns
print "The number of columns in the file are $columns\n";

my @data = <IN>; #read rest of the data except header
chomp @data; #remove extra spaces in the array
my $rows = scalar(@data); #find the length of data to fing number of rows
print "The number of rows with data in the file are $rows\n";

############part2 of first question

$header =~ tr/a-z/A-Z/; #translate small alphabets with large ones
#print $header, "\n";
$header =~ s/ /_/g; #substitute all empty spaces globally with underscore
print $header, "\n";


###########second question
my $line; #declaring all variables to be used 
my $line1;
my @entry;
my @cellvol;
my @time;
my @cln3;
my @index_cycle;
my (@cln3_1, @cln3_2, @cln3_3, @cln3_4, @cln3_5, @cln3_6, @cln3_7, @cln3_8, @cln3_9, @cln3_10, @cln3_11, @cln3_12, @cln3_13, @cln3_14, @cln3_15, @cln3_16, @cln3_17, @cln3_18, @cln3_19, @cln3_20);
my (@time1, @time2, @time3, @time4, @time5, @time6, @time7, @time8, @time9, @time10, @time11, @time12, @time13, @time14, @time15, @time16, @time17, @time18, @time19, @time20);

foreach $line(@data) #read the data and split it by /,/ and then read the specific values into array
{
	chomp $line;
	@entry = split(/,/ , $line);
	push(@cellvol, $entry[60]); #array having all cell volumes
	push(@time, $entry[0]); #array with all time series
	push(@cln3, $entry[3]); #array with all cln3 values
}

my $length = scalar(@cellvol);
for(my $i =0; $i < $length; $i++) # loop to find index where cell cycle starts, 
{								  # based on cell volume levels and store those indexes into array
	if($cellvol[$i] > $cellvol[$i+1])
	{
		push(@index_cycle, $i+1);
	}
}

#print $time[3], "\n";
#print $cln3[0], "\n";

#based on those indexs of cell cycle store cell cycle specific time and cln3 values in array
for(my $j = 0; $j < $index_cycle[0]; $j++)
{
	push(@cln3_1, $cln3[$j]);
	push(@time1, $time[$j]);
}
for(my $ab = $index_cycle[0]; $ab < $index_cycle[1]; $ab++)
{
	push(@cln3_2, $cln3[$ab]);
	push(@time2, $time[$ab]);
}
for(my $ac = $index_cycle[1]; $ac < $index_cycle[2]; $ac++)
{
	push(@cln3_3, $cln3[$ac]);
	push(@time3, $time[$ac]);
}
for(my $ad = $index_cycle[2]; $ad < $index_cycle[3]; $ad++)
{
	push(@cln3_4, $cln3[$ad]);
	push(@time4, $time[$ad]);
}
for(my $ae = $index_cycle[3]; $ae < $index_cycle[4]; $ae++)
{
	push(@cln3_5, $cln3[$ae]);
	push(@time5, $time[$ae]);
}
for(my $af = $index_cycle[4]; $af < $index_cycle[5]; $af++)
{
	push(@cln3_6, $cln3[$af]);
	push(@time6, $time[$af]);
}
for(my $ag = $index_cycle[5]; $ag < $index_cycle[6]; $ag++)
{
	push(@cln3_7, $cln3[$ag]);
	push(@time7, $time[$ag]);
}
for(my $ah = $index_cycle[6]; $ah < $index_cycle[7]; $ah++)
{
	push(@cln3_8, $cln3[$ah]);
	push(@time8, $time[$ah]);
}
for(my $ai = $index_cycle[7]; $ai < $index_cycle[8]; $ai++)
{
	push(@cln3_9, $cln3[$ai]);
	push(@time9, $time[$ai]);
}
for(my $aj = $index_cycle[8]; $aj < $index_cycle[9]; $aj++)
{
	push(@cln3_10, $cln3[$aj]);
	push(@time10, $time[$aj]);
}
for(my $ak = $index_cycle[9]; $ak < $index_cycle[10]; $ak++)
{
	push(@cln3_11, $cln3[$ak]);
	push(@time11, $time[$ak]);
}
for(my $al = $index_cycle[10]; $al < $index_cycle[11]; $al++)
{
	push(@cln3_12, $cln3[$al]);
	push(@time12, $time[$al]);
}
for(my $am = $index_cycle[11]; $am < $index_cycle[12]; $am++)
{
	push(@cln3_13, $cln3[$am]);
	push(@time13, $time[$am]);
}
for(my $an = $index_cycle[12]; $an < $index_cycle[13]; $an++)
{
	push(@cln3_14, $cln3[$an]);
	push(@time14, $time[$an]);
}
for(my $ap = $index_cycle[13]; $ap < $index_cycle[14]; $ap++)
{
	push(@cln3_15, $cln3[$ap]);
	push(@time15, $time[$ap]);
}
for(my $aq = $index_cycle[14]; $aq < $index_cycle[15]; $aq++)
{
	push(@cln3_16, $cln3[$aq]);
	push(@time16, $time[$aq]);
}
for(my $ar = $index_cycle[15]; $ar < $index_cycle[16]; $ar++)
{
	push(@cln3_17, $cln3[$ar]);
	push(@time17, $time[$ar]);
}
for(my $as = $index_cycle[16]; $as < $index_cycle[17]; $as++)
{
	push(@cln3_18, $cln3[$as]);
	push(@time18, $time[$as]);
}
for(my $at = $index_cycle[17]; $at < $index_cycle[18]; $at++)
{
	push(@cln3_19, $cln3[$at]);
	push(@time19, $time[$at]);
}
for(my $au = $index_cycle[18]; $au < $index_cycle[19]; $au++)
{
	push(@cln3_20, $cln3[$au]);
	push(@time20, $time[$au]);
}

#create cell_cycle objects and input all values for assigned attributes in the module
my $cell_cycle1 = CellCycleStats->new(start_time_instant=>$time[0],
										end_time_instant=>$time[$index_cycle[0]-1], 
										CLN3=>\@cln3_1, TIME=>\@time1);
my $cycle_stats1 = $cell_cycle1->cellCycle(); #calling the method cellCycle on the object

my $cell_cycle2 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[0]],
										end_time_instant=>$time[$index_cycle[1]-1], 
										CLN3=>\@cln3_2, TIME=>\@time2);
my $cycle_stats2 = $cell_cycle2->cellCycle();

my $cell_cycle3 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[1]],
										end_time_instant=>$time[$index_cycle[2]-1], 
										CLN3=>\@cln3_3, TIME=>\@time3);
my $cycle_stats3 = $cell_cycle3->cellCycle();

my $cell_cycle4 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[2]],
										end_time_instant=>$time[$index_cycle[3]-1], 
										CLN3=>\@cln3_4, TIME=>\@time4);
my $cycle_stats4 = $cell_cycle4->cellCycle();

my $cell_cycle5 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[3]],
										end_time_instant=>$time[$index_cycle[4]-1], 
										CLN3=>\@cln3_5, TIME=>\@time5);
my $cycle_stats5 = $cell_cycle5->cellCycle();

my $cell_cycle6 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[4]],
										end_time_instant=>$time[$index_cycle[5]-1], 
										CLN3=>\@cln3_6, TIME=>\@time6);
my $cycle_stats6 = $cell_cycle6->cellCycle();

my $cell_cycle7 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[5]],
										end_time_instant=>$time[$index_cycle[6]-1], 
										CLN3=>\@cln3_7, TIME=>\@time7);
my $cycle_stats7 = $cell_cycle7->cellCycle();

my $cell_cycle8 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[6]],
										end_time_instant=>$time[$index_cycle[7]-1], 
										CLN3=>\@cln3_8, TIME=>\@time8);
my $cycle_stats8 = $cell_cycle8->cellCycle();

my $cell_cycle9 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[7]],
										end_time_instant=>$time[$index_cycle[8]-1], 
										CLN3=>\@cln3_9, TIME=>\@time9);
my $cycle_stats9 = $cell_cycle9->cellCycle();

my $cell_cycle10 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[8]],
										end_time_instant=>$time[$index_cycle[9]-1], 
										CLN3=>\@cln3_10, TIME=>\@time10);
my $cycle_stats10 = $cell_cycle10->cellCycle();

my $cell_cycle11 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[9]],
										end_time_instant=>$time[$index_cycle[10]-1], 
										CLN3=>\@cln3_11, TIME=>\@time11);
my $cycle_stats11 = $cell_cycle11->cellCycle();

my $cell_cycle12 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[10]],
										end_time_instant=>$time[$index_cycle[11]-1], 
										CLN3=>\@cln3_12, TIME=>\@time12);
my $cycle_stats12 = $cell_cycle12->cellCycle();

my $cell_cycle13 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[11]],
										end_time_instant=>$time[$index_cycle[12]-1], 
										CLN3=>\@cln3_13, TIME=>\@time13);
my $cycle_stats13 = $cell_cycle13->cellCycle();

my $cell_cycle14 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[12]],
										end_time_instant=>$time[$index_cycle[13]-1], 
										CLN3=>\@cln3_14, TIME=>\@time14);
my $cycle_stats14 = $cell_cycle14->cellCycle();

my $cell_cycle15 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[13]],
										end_time_instant=>$time[$index_cycle[14]-1], 
										CLN3=>\@cln3_15, TIME=>\@time15);
my $cycle_stats15 = $cell_cycle15->cellCycle();

my $cell_cycle16 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[14]],
										end_time_instant=>$time[$index_cycle[15]-1], 
										CLN3=>\@cln3_16, TIME=>\@time16);
my $cycle_stats16 = $cell_cycle16->cellCycle();

my $cell_cycle17 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[15]],
										end_time_instant=>$time[$index_cycle[16]-1], 
										CLN3=>\@cln3_17, TIME=>\@time17);
my $cycle_stats17 = $cell_cycle17->cellCycle();

my $cell_cycle18 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[16]],
										end_time_instant=>$time[$index_cycle[17]-1], 
										CLN3=>\@cln3_18, TIME=>\@time18);
my $cycle_stats18 = $cell_cycle18->cellCycle();

my $cell_cycle19 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[17]],
										end_time_instant=>$time[$index_cycle[18]-1], 
										CLN3=>\@cln3_19, TIME=>\@time19);
my $cycle_stats19 = $cell_cycle19->cellCycle();

my $cell_cycle20 = CellCycleStats->new(start_time_instant=>$time[$index_cycle[18]],
										end_time_instant=>$time[$index_cycle[19]-1], 
										CLN3=>\@cln3_20, TIME=>\@time20);
my $cycle_stats20 = $cell_cycle20->cellCycle();

#store all the objects and cellcycle stats into an array
my @cell_stats = ($cycle_stats1, $cycle_stats2, $cycle_stats3, $cycle_stats4, $cycle_stats5,
				   $cycle_stats6, $cycle_stats7, $cycle_stats8, $cycle_stats9, $cycle_stats10, 
				   $cycle_stats11, $cycle_stats12, $cycle_stats13, $cycle_stats14, $cycle_stats15, 
				   $cycle_stats16, $cycle_stats17, $cycle_stats18, $cycle_stats19, $cycle_stats20);

my @cell_cycles = ($cell_cycle1, $cell_cycle2, $cell_cycle3, $cell_cycle4, $cell_cycle5, $cell_cycle6, 
					$cell_cycle7, $cell_cycle8, $cell_cycle9, $cell_cycle10, $cell_cycle11, $cell_cycle12,
					$cell_cycle13, $cell_cycle14, $cell_cycle15, $cell_cycle16, $cell_cycle17, $cell_cycle18, 
					$cell_cycle19, $cell_cycle20);

#print elements of the aforementioned arrays
for(my $pq = 0; $pq < scalar(@cell_cycles); $pq++)					
{
	my $y = $pq+1;
	print "Cell cycle $y has: \n";
	print "$cell_cycles[$pq]";
}

print "\n\n";

for(my $mn = 0; $mn < scalar(@cell_stats); $mn++)
{
	my $x = $mn+1;
	print "Stats for the cellCycle $x is: \n";
	print "$cell_stats[$mn]\n ";
}

# and as the mean, min and max of the cln3 protein remains the same, and the 
#elapsed time (difference b/w start and end time) remains the same, it is a 
#deterministic simulation.










