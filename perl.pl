#!/usr/bin/perl

use strict;
use warnings;

$" = '';    # print arrays like strings

my @target = split //, 'METHINKS IT IS LIKE A WEASEL';
my @gene_pool     = ( ' ', 'A' .. 'Z' );
my $litter_size   = 100;
my $mutation_rate = 0.04;

my $weasel = [ map { rand_gene() } 0 .. $#target ];
my $weasel_score = score(@$weasel);

print "1: '@$weasel' ($weasel_score)\n";
for ( my $generation = 2; $weasel_score < @target; ++$generation ) {
    my @litter = sort { $a->[1] <=> $b->[1] }
                 map { [ $_, score(@$_) ] }
                 map { [ reproduce(@$weasel) ] } 1 .. $litter_size;
    ( $weasel, $weasel_score ) = @{ $litter[-1] };
    print "$generation: '@$weasel' ($weasel_score)\n";
}

sub rand_gene { $gene_pool[ rand(@gene_pool) ] }
sub reproduce { map { rand(1) < $mutation_rate ? rand_gene() : $_ } @_ }
sub score { scalar grep { $target[$_] eq $_[$_] } 0 .. $#target }
