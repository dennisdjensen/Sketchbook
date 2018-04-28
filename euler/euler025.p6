#!/usr/bin/env perl6
# Copyright 2015 Dennis Decker Jensen
# Date: 01 November 2015
# Purpose: The first fibonacci term containing 1000 digits
# Tectonics: perl6 euler025.p6
use v6;

my @fibs = 1, 1, * + * ... *;
say @fibs.first(:k, *.chars >= 1000) + 1;
# => 4782
