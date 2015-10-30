#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 30 October 2015
# Purpose: Total score of names
# Tectonics: perl6 euler0022.p6

my $A = 'A'.encode('ascii')[0];

sub alpha-value($name) {
	[+] $name.encode('ascii').map(* - $A + 1);
}

my @names = 'p022_names.txt'.IO.slurp.subst('"', '', :g).split(',').sort;

say [+] (1..Inf Z* @names.map({ alpha-value($_) })); # => 871198282

