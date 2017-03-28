#!usr/bin/perl -w

#split sequences after reterieving them from genebank
use strict;
use warnings;
 
use Bio::DB::GenBank;
 
open (INPUT_FILE, 'accnumber.txt');
open (OUTPUT_FILE, 'sequence_dwnl.fa');
 
while(my $seq =  $seqio->next_seq)
{
    chomp;
    my $line = $_;
    my @acc_no = split(",", $line);
    my $counter = 0;
 
    while ($acc_no[$counter])
    {
        $acc_no[$counter] =~ s/\s//g;
 
        if ($acc_no[$counter] =~ /^$/)
        {
            exit;
        }
 
        my $db_obj = Bio::DB::GenBank->new;
        my $seq_obj = $db_obj->get_Seq_by_acc($acc_no[$counter]);
        my $sequence1 = $seq_obj->seq;
 
        print OUTPUT_FILE "&gt;"."$acc_no[$counter]","\n";
        print OUTPUT_FILE $sequence1,"\n";
        print "Sequence Downloaded:", "\t", $acc_no[$counter], "\n";
 
        $counter++;
    }
}
 
close OUTPUT_FILE;
close INPUT_FILE;
 
exit;
