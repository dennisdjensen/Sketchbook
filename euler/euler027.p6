# Copyright 2015 Dennis Decker Jensen
# Date: 01 November 2015
# Purpose: maximum number of quadratic primes
use v6;

sub quadratic(Int $a, Int $b) {
	sub (Int $n) { $n ** 2 + $a * $n + $b};
}

=comment
my &q = quadratic(-79, 1601);
my @primes = (0 ... { not q($^n).is-prime })[0..*-2];
say @primes;
say @primes.elems;
exit(0);

constant $limit = 1000;
my @coefficients = |(-$limit^..-1),|(1..^$limit);
my @top = 0 xx 4;
for @coefficients -> $a {
	for @coefficients -> $b {
		my &q = quadratic($a, $b);
		my @primes = (0 ... { not q($^n).is-prime })[0..*-2];
		@top max= (@primes.elems, $a * $b, $a, $b);
	}
}
say @top;

