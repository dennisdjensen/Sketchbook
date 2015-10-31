#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 31 October 2015
# Purpose: The 1 millionth lexicographic permutation
# Tectonics: perl6 euler0024.p6
use v6;

say permutations(10)[999_999].join
# => 2783915460
