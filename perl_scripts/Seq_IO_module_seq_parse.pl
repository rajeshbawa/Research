#!/usr/bin/perl

# This script describes the use of SEQIO module of Bioperl, and covers most important features of the module
use warnings;
use strict;

use Bio::SeqIO;
#use Bio::Seq::SeqBuilder;

#first test
#reading the file, and fetching different defined variables from it.
my $infile   = "FASTA_sequences.txt";

my $inseq = Bio::SeqIO->new(							## constructor class 'new' which reads the mentioned file of specific format
                            -file   => "<$infile",
                            -format => 'fasta',
							);
							
while (my $seq = $inseq->next_seq)  # 'next_seq: reads the next sequence object in the file and assign it to $seq
{
    print $seq->id,"\n";
}

#second test
#converting one file format to another
my $infile_1   = "FASTA_sequences.txt"; #input file
my $outfile_1   = "GB_seq.txt"; #output file

my $seq_in = Bio::SeqIO->new(    #input can be taken as the -file flag, where one can mention the name of the file, and can use '-fh' flag and use \*ARGV i.e. globbing for reading strough console
								-file => "<$infile_1", #-fh     => \*ARGV ### read from standard input or the input filenames
								-format => 'fasta',						
							);  
							
my $seq_out = Bio::SeqIO->new(
                              -file => ">$outfile_1", #-fh     => \*ARGV ### read from standard input or the input filenames
                              -format => 'genbank',
                              );
							  
while (my $inseq = $seq_in->next_seq) 
{
    $seq_out->write_seq($inseq);
}							  

#third test
#selective parsing of the file using seq_builder
my $infile_2 = "GB_seq.txt";
 

my $in = Bio::SeqIO->new(-file => "<$infile_2", -format => 'Genbank');
my $builder;
my $builder = $in->sequence_builder();  #use the Bio::Factory::ObjectBuilderI to create a sequence object, and this will allow selective parsing of the file

$builder->want_none(); #this can be replaced by function 'want_all' where you want to fetch everything except things mentioned in flag 'add_unwanted_slot'
$builder->add_wanted_slot('display_id'); ## this restricts the parsing to id's only

print "After the implementation of seq_builder, we can only access ID and not seq as it wasn't parsed\n";
while(my $seq = $in->next_seq()) 
{
	print " The id of sequence is ", $seq->id, "\n"; #print sequence id, which was parsed
	if ("$seq->seq" ne defined) #if we try to print anything else which wasn't parsed, we will get error, as shown
	{
	print "Sequence not parsed\n";
	}
	else
	{
	print $seq->seq;
	}	
	## so when you try to fetch a sequence it won't work as wasn't parsed
	## therefore seq builder helps in selective parsing, and faster results
}
