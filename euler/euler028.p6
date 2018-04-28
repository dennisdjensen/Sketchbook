# Copyright 2015 Dennis Decker Jensen
# Date: 01 November 2015
# Purpose: Sum of spiral diagonals
use v6;

#constant $side = 5; # Diagonal spiral sum => 101
constant $side = 1001;
constant $size = $side * $side;

# The start center with number 1 is excluded from the diagonal sums
my $diag1 = 0; # \ upper left to lower right
my $diag2 = 0; # / lower left to upper right
my $start = 1; # Center of the spiral
my $step  = 2; # First/innermost ring grows by 2, the first side step
loop {
	last if $start == $size;
	# This really is a loop unrolled, but it's silly not to!
	$diag1 += ($start+1*$step) + ($start+3*$step); # \
	$diag2 += ($start+2*$step) + ($start+4*$step); # /
	$start += 4 * $step; # Jump one ring out of the spiral
			     # - to the upper right corner
	$step  += 2; # Each side grows by 2 more for each ring
}

say 'Sum of diagonal numbers (1) \\ : ', $diag1 + 1;
say 'Sum of diagonal numbers (2) / : ',  $diag2 + 1;
say 'Sum of spiral diagonal numbers including centre: ', $diag1 + $diag2 + 1;

