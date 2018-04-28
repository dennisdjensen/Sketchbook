#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 30 October 2015
# Purpose: Calculate sum of amicable numbers under 10000
# Tectonics: perl6 euler021.p6
use v6;

sub aliquot-sum(Int $n) {
	[+] (1..($n div 2 + 1)).grep($n %% *)
}

sub sum-amicable-numbers(Int $n) {
	say "Calculating aliquot sums 1..^$n";
	my %aliquot-sums;
	%aliquot-sums{$_} = aliquot-sum($_) for 1..^$n;

	say "Calculating amicable number sum under $n";
	my $sum = 0;
	for 1..^$n -> $a {
		my $b = %aliquot-sums{$a};
		next unless %aliquot-sums{$b}:exists;
		next unless %aliquot-sums{$a} == $b;
		next unless %aliquot-sums{$b} == $a;
		next unless $a != $b;
		#say "$a $b -> $sum";
		$sum += $a;
	}
	return $sum;
}

sub MAIN {
	#say sum-amicable-numbers(1000); # => 504
	say sum-amicable-numbers(10000); # => 31626
}
