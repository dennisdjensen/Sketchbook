#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 31 October 2015
# Purpose: Sum of non-abundant sums
# Tectonics: perl6 euler0023.p6
use v6;

=begin comment
A perfect number is a number for which the sum of its proper
divisors is exactly equal to the number. For example, the sum
of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28,
which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors
is less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 =
16, the smallest number that can be written as the sum of
two abundant numbers is 24. By mathematical analysis, it can
be shown that all integers greater than 28123 can be written
as the sum of two abundant numbers. However, this upper limit
cannot be reduced any further by analysis even though it is
known that the greatest number that cannot be expressed as the
sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be
written as the sum of two abundant numbers.
=end comment

constant $limit = 28123;

sub aliquot-sum(Int $n) {
	[+] (1..^($n div 2 + 1)).grep($n %% *)
}

my @abundant = (1..$limit).map({ $_ if aliquot-sum($_) > $_ });
# => .elems == 6965

say '#Abundant numbers (d(n) > n): ', @abundant.elems;
say 'The first 10: ', @abundant[0..9];

my $sums = (1..$limit).SetHash;
for @abundant[*]:kv -> $j, $a {
	for @abundant[$j..*-1] -> $b {
		$sums{$a + $b}:delete;
	}
}
# => .elems == 1456

say '#Non-abundant sums of 2 numbers: ', $sums.elems;
say 'Sum of non-abundant sums: ', [+] $sums.keys;
# => 4112737 is incorrect
# => 4179871 is correct

