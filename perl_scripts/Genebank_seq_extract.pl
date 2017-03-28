#!/usr/bin/perl

use warnings;
use strict;

use FASTA;
use Bio::DB::GenBank;
use Bio::SeqIO;
use Data::Dumper;

my $db_obj = Bio::DB::GenBank->new;
my @accessions = ('NP_009302.2', 'NP_009303.1', 'NP_009304.2', 'NP_009305.1', 'NP_009310.1', 'NP_009309.1', 'NP_009308.1', 'NP_009307.1', 'NP_009306.1', 'NP_009311.2');

my $outfile_obj = Bio::SeqIO->new( -file => '>FASTA_sequences.txt',
								   -format => 'fasta');
foreach my $acc(@accessions)
{
	chomp $acc;
	#print$acc,"\n";
	my $seq_obj;
	eval {$seq_obj = $db_obj->get_Seq_by_version($acc);
		};
		if ($@)
		{
			print "$acc not found\n";
		}
		else
		{
		$outfile_obj->write_seq($seq_obj);
		}
	#print Dumper($seq_obj); die;
	#print $seq_obj->desc . "\n";
	#print $seq_obj->seq ."\n";
}
$/=">"; #read and seperate the file
open(my $fh, 'FASTA_sequences.txt') or die "Cannot open the file: $!\n";
my @data = <$fh>; #read the 10 objects after the splitting the file into array
#print $data[1];
close $fh; 

my $element = 'Q';

foreach my $line(@data){
	my($id,$des,$seq) = $line =~ /(gi|\S+|)\s(.+)\n([\w\W]+)/i; 
   #$seq =~ s/[>\s]//g; #replace > sign with nothing
   my $self = FASTA->new($id,$des,$seq);
   $self->get_identifier;
   my $count = $self->count_element_in_sequence($element);
   print "The $element appears $count in the sequence\n";
   }