#!/usr/bin/env python3
# Copyright 2015 Dennis Decker Jensen
# Date: 31 October 2015
# Purpose: Sum of non-abundant sums
# Tectonics: python3 euler023.py

limit = 28123

def aliquot_sum(n):
	return sum(d for d in range(1, n // 2 + 1) if n % d == 0)

abundant = list(n for n in range(1, limit+1) if aliquot_sum(n) > n)
# => len == 6965

print('#Abundant numbers (d(n) > n): ', len(abundant))
print('The first 9: ', abundant[:10])

sums = set(range(1, limit+1))
for j, a in enumerate(abundant):
	for b in abundant[j:]:
		s = a + b
		if s in sums:
			sums.remove(s)
# => len == 1456

print('#Non-abundant sums of 2 numbers: ', len(sums))
print('Sum of non-abundant sums: ', sum(sums))
# => 4112737 is incorrect
# => 4179871 is correct

