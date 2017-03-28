############################################################
#Narrative of the module: This module can be divided into three subparts:
#Part1: Translate the protein sequence to aminoacid sequence(three letter codes).
#it requires the Bio::PrimarySeqI object and then uses a hash (predefined) to translate the sequence
#it makes sure to remove the gaps in translated sequence and code for stop and undefined aminoacid

#Part2: Gives the valid list of amino acids, depending upon the code provided(0,1,2)
#It uses %Onecode, and retreives values, keys, or both respectively based on code 0,1, or 2
#It sorts the amino acid, and keeps B,Z,X aa at the end of an array

#Part3: It concatenates the sequence, move the annotation to top and adjusts the features of sequence
#it requires the Bio::PrimarySeqI object, and both concatenating sequences to be its objects and belonging to same -alphabet class before catenation
#it requires the Bio::AnnotaionI, to see the presence of annotation in cat sequences, and move the annotation to top of the catenated sequence.
#it uses Bio::SeqI to get sequence features(top level only) and adjust them by moving to top, and replacing length with length of concatenated sequence.
##############################################################


# $Id: SeqUtils.pm 16123 2009-09-17 12:57:27Z cjfields $
#
# BioPerl module for Bio::SeqUtils
#
# Please direct questions and support issues to <bioperl-l@bioperl.org> 
#
# Cared for by Heikki Lehvaslaiho <heikki-at-bioperl-dot-org>
#
# Copyright Heikki Lehvaslaiho
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

#[...] OMITTED CODE [...]

package Bio::SeqUtils;
use vars qw(%ONECODE %THREECODE); #predeclaring of global variables
use strict;
use Carp;

use base qw(Bio::Root::Root);
# new inherited from RootI

## %ONECODE is a hash for aminoacid names to their IUPAC symbols and #THREECODE is a vice versa
BEGIN {
    # Note : Ambiguity code 'J' = I/L (used for ambiguities in mass-spec data)
    %ONECODE =
    ('Ala' => 'A', 'Asx' => 'B', 'Cys' => 'C', 'Asp' => 'D',
     'Glu' => 'E', 'Phe' => 'F', 'Gly' => 'G', 'His' => 'H',
     'Ile' => 'I', 'Lys' => 'K', 'Leu' => 'L', 'Met' => 'M',
     'Asn' => 'N', 'Pro' => 'P', 'Gln' => 'Q', 'Arg' => 'R',
     'Ser' => 'S', 'Thr' => 'T', 'Val' => 'V', 'Trp' => 'W',
     'Xaa' => 'X', 'Tyr' => 'Y', 'Glx' => 'Z', 'Ter' => '*',
     'Sec' => 'U', 'Pyl' => 'O', 'Xle' => 'J'
     );

    %THREECODE =
    ('A' => 'Ala', 'B' => 'Asx', 'C' => 'Cys', 'D' => 'Asp',
     'E' => 'Glu', 'F' => 'Phe', 'G' => 'Gly', 'H' => 'His',
     'I' => 'Ile', 'K' => 'Lys', 'L' => 'Leu', 'M' => 'Met',
     'N' => 'Asn', 'P' => 'Pro', 'Q' => 'Gln', 'R' => 'Arg',
     'S' => 'Ser', 'T' => 'Thr', 'V' => 'Val', 'W' => 'Trp',
     'Y' => 'Tyr', 'Z' => 'Glx', 'X' => 'Xaa', '*' => 'Ter',
     'U' => 'Sec', 'O' => 'Pyl', 'J' => 'Xle'
     );
}

=head2 seq3

 Title   : seq3
 Usage   : $string = Bio::SeqUtils->seq3($seq)
 Function: Read only method that returns the amino acid sequence as a
           string of three letter codes. alphabet has to be
           'protein'. Output follows the IUPAC standard plus 'Ter' for
           terminator. Any unknown character, including the default
           unknown character 'X', is changed into 'Xaa'. A noncoded
           aminoacid selenocystein is recognized (Sec, U).

 Returns : A scalar
 Args    : character used for stop in the protein sequence optional,
           defaults to '*' string used to separate the output amino
           acid codes, optional, defaults to ''

=cut

sub seq3 {
   my ($self, $seq, $stop, $sep ) = @_; #obtains values from the implemented script

   $seq->isa('Bio::PrimarySeqI') ||
       $self->throw('Not a Bio::PrimarySeqI object but [$self]'); #get a Bio::PrimarySeqI sequence object or give an error
   $seq->alphabet eq 'protein' ||
       $self->throw('Not a protein sequence'); #if sequence (flag -alphabet) is not a protein, give error

   if (defined $stop) {
       length $stop != 1 and $self->throw('One character stop needed, not [$stop]'); #one letter stop, or error, default being '*'
       $THREECODE{$stop} = "Ter";
   }
   $sep ||= ''; 

   my $aa3s;
   #split the protein sequence by each letter, and replace it with a THREECODE value (defined or undef) and concatenate with a no space b/w them
   foreach my $aa  (split //, uc $seq->seq) {
       $THREECODE{$aa} and $aa3s .= $THREECODE{$aa}. $sep, next;
       $aa3s .= 'Xaa'. $sep;
   }
   $sep and substr($aa3s, -(length $sep), length $sep) = '' ; #remove gaps in the aminoacid sequence
   return $aa3s;
}

#[...] OMITTED CODE [...]

=head2 valid_aa

 Title   : valid_aa
 Usage   : my @aa = $table->valid_aa
 Function: Retrieves a list of the valid amino acid codes.
           The list is ordered so that first 21 codes are for unique 
           amino acids. The rest are ['B', 'Z', 'X', '*'].
 Returns : array of all the valid amino acid codes
 Args    : [optional] $code => [0 -> return list of 1 letter aa codes,
				1 -> return list of 3 letter aa codes,
				2 -> return associative array of both ]

=cut

sub valid_aa{
   my ($self,$code) = @_;

   if( ! $code ) { #if $code = 0 or not defined
       my @codes;
       foreach my $c ( sort values %ONECODE ) { #sort all one letter codes and push into @codes, and then push the undef B,Z, X ones at end
	   push @codes, $c unless ( $c =~ /[BZX\*]/ );
       }
       push @codes, qw(B Z X *); # so they are in correct order ?
       return @codes;
  }
   elsif( $code == 1 ) { 
       my @codes;
       foreach my $c ( sort keys %ONECODE ) { #sort and push all three letter codes(keys in %one code) in @ codes and at end push the undefined ones. 
	   push @codes, $c unless ( $c =~ /(Asx|Glx|Xaa|Ter)/ );
       }
       push @codes, ('Asx', 'Glx', 'Xaa', 'Ter' );
       return @codes;
   }
   elsif( $code == 2 ) { 
       my %codes = %ONECODE;
       foreach my $c ( keys %ONECODE ) # return a hash with both keys(three lettered codes) and values (one lettered code)
	   my $aa = $ONECODE{$c};
	   $codes{$aa} = $c;
       }
       return %codes;
   } else {
       $self->warn("unrecognized code in ".ref($self)." method valid_aa()");
       return ();
   }
}

#[...] OMITTED CODE [...]



=head2 cat

  Title   : cat
  Usage   : my $catseq = Bio::SeqUtils->cat(@seqs)
  Function: Concatenates an array of Bio::Seq objects, using the first sequence
            as a target. Annotations and sequence features are copied over 
            from any additional objects. Adjusts the coordinates of copied 
            features.
  Returns : a boolean
  Args    : array of sequence objects

Note that annotations have no sequence locations. If you concatenate
sequences with the same annotations they will all be added.

=cut

sub cat {
    my ($self, $seq, @seqs) = @_;
    $self->throw('Object [$seq] '. 'of class ['. ref($seq).
                 '] should be a Bio::PrimarySeqI ')
        unless $seq->isa('Bio::PrimarySeqI'); # if $seq is not object of Bio::PrimarySeqI, throw an error 
    

    for my $catseq (@seqs) {
        $self->throw('Object [$catseq] '. 'of class ['. ref($catseq).
                     '] should be a Bio::PrimarySeqI ')
            unless $catseq->isa('Bio::PrimarySeqI'); #make sure that $catseq has same features as $seq in terms of being an object of Bio::PrimarySeqI

        $self->throw('Trying to concatenate sequences with different alphabets: '.
                     $seq->display_id. '('. $seq->alphabet. ') and '. $catseq->display_id.
                     '('. $catseq->alphabet. ')')
            unless $catseq->alphabet eq $seq->alphabet; #make sure that the concatenated sequence have same values for '-alphabet flag' i.e. dna, rna or prt or give error


        my $length=$seq->length; #find length of sequence
        $seq->seq($seq->seq.$catseq->seq); #concatenate the sequences and store them as $seq object

        # move annotations
        if ($seq->isa("Bio::AnnotatableI") and $catseq->isa("Bio::AnnotatableI")) { #if $seq and $catseq are objects of Bio::AnnotatableI, i.e. they have  base interface that all annotatable objects must implement
            foreach my $key ( $catseq->annotation->get_all_annotation_keys() ) {

                foreach my $value ( $catseq->annotation->get_Annotations($key) ) {
                    $seq->annotation->add_Annotation($key, $value); #move the annotation of the $catseq(both keys and values) to the start of sequence i.e at $seq start
                }
            } 
        }
        
        # move SeqFeatures
        if ( $seq->isa('Bio::SeqI') and $catseq->isa('Bio::SeqI')) {
            for my $feat ($catseq->get_SeqFeatures) {
                $seq->add_SeqFeature($self->_coord_adjust($feat, $length)); #adjust the coordinates of sequence, and substitute the length with concatenated sequence length
            }
        }

    }
    1;
}


