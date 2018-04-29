// Copyright 2018 Dennis Decker Jensen
// Date: 29 April 2018
// Purpose: Minimum difference of pentagon numbers.
// Tectonics: cc -std=c11 -pedantic -Wall -O2 -o euler044 euler044.c
//

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

enum { npentagons = 10000 }; // N.B! Trial and error
unsigned pentagon[npentagons];

int
cmp(const void *a, const void *b)
{
	unsigned ua = *(unsigned *)a, ub = *(unsigned *)b;
	if (ua < ub) return -1;
	else if (ua > ub) return 1;
	else return 0;
}

int
main()
{
	size_t i, j, m = 0, n = 0, si = 0, di = 0;
	unsigned sum = 0, difference = 0, min = UINT_MAX, s, *b;

	for (i = 0; i < npentagons; ++i)
		pentagon[i] = i * (3 * i - 1) / 2;

	for (i = 1; i < npentagons; ++i)
		for (j = i + 1; j < npentagons
			&& (s = pentagon[i] + pentagon[j])
				<= pentagon[npentagons - 1]; ++j) {
			b = bsearch(&s, pentagon, npentagons,
					sizeof(unsigned), cmp);
			if (b == NULL)
				continue;
			si = b - pentagon;

			difference = pentagon[j] - pentagon[i];
			b = bsearch(&difference, pentagon, npentagons,
					sizeof(unsigned), cmp);
			if (b == NULL)
				continue;
			di = b - pentagon;

			if (difference < min) {
				min = difference;
				sum = s;
				m = i;
				n = j;
				printf(">P%zu - P%zu = %u\n", m, n, min);
				printf(">P%zu + P%zu = %u\n", m, n, sum);
			}
		}
	printf("Minimum, P%zu - P%zu = P%zu (%u)\n", m, n, di, min);
	printf("         P%zu + P%zu = P%zu (%u)\n", m, n, si, sum);
	return 0;
}

