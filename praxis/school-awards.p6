# Copyright 2015 Dennis Decker Jensen
# Date: 03 November 2015
# http://programmingpraxis.com/2015/11/03/school-awards/
# Purpose: Can a student win an award?

=comment
There is a school that awards students who, during a given
period, are never late more than once and who are never absent
for three or more consecutive days. How many permutations of
on-time, late and absent, with repetition, are possible for
a given timeframe that will result in an award being given to
a particular student. For instance, for a three-day timeframe
there are 19 ways in which a student can win an award.

# Given the example, a student can be both late and absent twice,
# and still be given an award, without ever being on time!

# Instead of using tribonacci numbers, we are going to fake it,
# by using a radix 3 number system and string matching.
# This means we cannot handle anymore than a 36 days period.

my %translation = 
	'0' => 'O', # On-time
	'1' => 'L', # Late
	'2' => 'A', # Absent
;

sub student-award(:$days) {
	my $from = '0' x $days;
	my $to   = '2' x $days;
	my $pad  = '%0' ~ $days ~ 'd';
	my @good-days = (:3($from) .. :3($to))
		.map(*.base(3).fmt($pad).trans(%translation))
		.grep({ $_ !~~ /AAA/ })         # Absent at most twice
		.grep({ $_ !~~ /'L' .* 'L'/ }); # Late at most once
	return (@good-days.elems, @good-days);
}

say 'For 3 days:  ', student-award(:3days);
#say 'For 20 days: ', student-award(days => 20); # Just CTRL-C me!

# A little too brusque!
# Let's try something else.

sub combos($n, $L, $A) {
	if $A|$L < 0 { return 0 }
	if $n == 0   { return 1 }
	return  combos($n-1, $L,    2) +  # On-time
		combos($n-1, $L, $A-1) +  # Absent
		combos($n-1, $L-1,  2);   # Late
}
# Oups! Caching is not going to help...

say 'Possibilities for  3 days: ', combos(3, 1, 2);
#say 'Possibilities for 20 days: ', combos(20, 1, 2);
# => 2947811

# I give in! Tribonacci numbers, here we come!
# Read Scott @ http://math.stackexchange.com/questions/1279610/

#| Tribonacci numbers
my @tribs = 1, 2, 4, * + * + * ... *;
sub award(:$days) {
	if $days == 1 { return 3 } # Because @tribs is too long...?
	my @seq  = @tribs[0..$days];
	my @init = @seq[0..*-2];
	@seq[*-1] + [+] (@init <<*>> @init.reverse);
}

say 'Award for  3 days: ', award(:3days);
say 'Award for 20 days: ', award(:20days);
# => 2947811

