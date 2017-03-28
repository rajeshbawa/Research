#!/usr/bin/perl

######################################################################
#This script generates a pdf file, printing name of user, time, when 
#report was generated, some cell cycle parameters, and a plot of 21 files
##
#Written by Rajesh K Bawa and Sandesh Shreshta
######################################################################

use warnings;
use strict;

use PDF::API2;
use CellCycleStats;

my $pdf = PDF::API2->new(-file => "CellCycleStats.pdf"); #create the pdf file
$pdf->mediabox(2500,2000); #size of the page
my $page = $pdf->page; # new page
my $fnt = $pdf->corefont('Arial',-encode => 'latin1'); #font selection
my $text = $page->text(); #text object
$text->font($fnt, 50); #font size
$text->translate(10, 1900); #cursor position
$text->text('Name: Rajesh K Bawa '); #print the name
$text->translate(10, 1850);
$text->text("The script was executed on: ".localtime()); #print time the report was generated
my $page1 = $pdf->page; # new page
my $gfx = $page1->gfx;
my $image=$pdf->image_jpeg('timeSeries.jpeg'); #include image
$gfx->image( $image, 10, 500 );

### print some more stats from the file using CellCycleStats module

use warnings;
use strict;

use CellCycleStats; #calling the module

my $file = "FinalExam_dataFile.csv"; 
open(IN, "<$file") or die "Could not open the file!\n";

###########part one of first question
my $header = <IN>; #read the header of the file
chomp $header; 
my @data = <IN>; #read rest of the data except header
chomp @data; #remove extra spaces in the array

my $line; #declaring all variables to be used 
my $total;
my @entry;
my @cellvol;
my @time;
my @cln3;
my @index_cycle;
my @cln3_1;
my @time1;

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
for(my $j = 0; $j < $index_cycle[0]; $j++) #stats of first cell cycle into array
{
	push(@cln3_1, $cln3[$j]);
	push(@time1, $time[$j]);
}
for(my $x = 0; $x < length(@cln3_1); $x++) #mean of cln3 protein for first cell cycle
	{
		$total += $cln3_1[$x]; 
	}
my $cln3_mean = sprintf("%.3f", $total/(length(@cln3_1)+1));
my $number_cellCycles = scalar(@index_cycle); #number of cell cycles
my @cln3_sorted = sort @cln3_1; #sort 
my $cln3_last = sprintf("%.3f", pop(@cln3_sorted)); #max value in cln3_1
my $first_cln3 = sprintf("%.3f", $cln3_sorted[0]); #min value in cln3_1

#printing rest of the stats
$text->translate(10, 1700);
$text->text("The stats from the intial input file are as followed: ");
$text->font($fnt, 30);
$text->translate(100, 1650);
$text->text("Number of Cell Cycles = $number_cellCycles");
$text->translate(100, 1600);
$text->text("Mean of cln3 = $cln3_mean");
$text->translate(100, 1550);
$text->text("Min of cln3 = $first_cln3");
$text->translate(100, 1500);
$text->text("Max of cln3 = $cln3_last");
$text->font($fnt, 50);
$text->translate(10, 1350);
$text->text("Being a deterministic simulation all 20 cell cycles have approx. same mean, min and max.");
$text->fillcolor('red');
$text->translate(10, 1200);
$text->text("Note: Please find all 20 time series plots on the next page.");
###############

$text->textend;
$pdf->save;
$pdf->end();
# so if the % of both outcomes is  not comparable and != 50% then its unbalanced
