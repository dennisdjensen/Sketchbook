// Copyright 2018 Dennis Decker Jensen
// Date: 29 April 2018
// Purpose: Triangular, p, and h.
// Tectonics: cc -std=c11 -pedantic -Wall -O2 -o euler045 euler045.c
//

#include <stdio.h>

int
main()
{
	unsigned long long int t, p, h; // triangle, pentagonal & hexagonal
	unsigned long long int i, j, k;

	// n(n + 1) / 2 grows by n + 1
	// n(3n - 1) / 2 grows by 3n + 1
	// n(2n - 1) grows by 4n + 1

	t = p = h = 40755;
	i = 285; j = 165; k = 143;
	do {
		h += 4 * k++ + 1;

		while (p < h)
			p += 3 * j++ + 1;
		if (p != h)
			continue;

		while (t < p)
			t += i++ + 1;
		printf("T_%llu = %llu; P_%llu = %llu; H_%llu = %llu\n",
			i, t, j, p, k, h);
		if (t != p)
			continue;


	} while (t != p || p != h);

	printf("Next t number:\n");
	printf("T_%llu = %llu\n", i, t);
	printf("P_%llu = %llu\n", j, p);
	printf("H_%llu = %llu\n", k, h);
	return 0;
}

