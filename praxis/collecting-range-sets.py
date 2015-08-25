#!/usr/bin/env python3
# Copyright 2015 Dennis Decker Jensen

# Collecting sets of ranges sets
# http://programmingpraxis.com/2015/08/25/collect-sets-of-ranges/

# It's a classic problem that Jackson suggested a simple solution for.
# You structure the control flow after the data,
# i.e. one outer loop for the break points,
# and one inner loop for a range.
# The point is to have the state implicitly coded into
# the control flow, rather than manipulating an explicit state
# in a single loop.
# In this case this can be done by "reading ahead" before the loop,
# and then "reading" at the end of the loop.
# Follow the structure of the data.
# This makes the loops very simple and easy to understand.

def collect(seq):
    result = []
    it = iter(seq)
    e = next(it, None)
    while e is not None:
        fst, snd = e, e
        e = next(it, None)
        while e is not None and e == snd + 1:
            snd = e
            e = next(it, None)
        range = (fst, snd)
        result.append(range)
    return result

print(collect([0, 1, 2, 7, 21, 22, 108, 109]))
print(collect([]))
print(collect([0, 1, 2, 3, 4, -1, 0, 1, 10]))
print(collect([0, 2, 3, 4, -3, -2, -1, 10]))

