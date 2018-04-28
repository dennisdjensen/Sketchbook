#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 01 November 2015
# Purpose: Longest recurring decimal fraction size
# Tectonics: perl6 euler025.p6
use v6;

constant $limit = 1000;
my $f = (2..^$limit).map({ FatRat.new(1, $_) }).max(*.base-repeating[1].chars);
say "1/$f.denominator() has base cycle { $f.base-repeating[1] }";

