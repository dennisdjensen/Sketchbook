// Copyright 2018 Dennis Decker Jensen
// Date: 5 May 2018
// Purpose: Goldbach's other conjecture.
//   The smallest odd composite that cannot be written
//   as the sum of a prime and twice a square
// Tectonics: cc -std=c11 -pedantic -Wall -O2 -lm -o euler046 euler046.c
//

#include <math.h>
#include <stdio.h>
#include <string.h>

// Zero base indexed bitset function-like macros.
#define bitword (8 * sizeof(unsigned char) * sizeof(unsigned int))
#define bitidx(i) ((i) / bitword)
#define getbit(set, i) ((set[bitidx(i)] >>        ((bitword - 1) & i)) & 1)
#define clrbit(set, i)  (set[bitidx(i)] &= ~(1 << ((bitword - 1) & i)))
#define setbit(set, i)  (set[bitidx(i)] |=  (1 << ((bitword - 1) & i)))
#define invbit(set, i)  (set[bitidx(i)] ^=  (1 << ((bitword - 1) & i)))

enum constants
{
	sieve_max = 10000U, // trial and error ...
	high = bitidx(sieve_max) + 1
};

// 9 = 7 + 2×1^2
// 15 = 7 + 2×2^2
// 21 = 3 + 2×3^2
// 25 = 7 + 2×3^2
// 27 = 19 + 2×2^2
// 33 = 32 + 2×1^2

int
main()
{
	unsigned sieve[high]; // erasthotene
	unsigned n, m, c = 0, d = 0, r = 0, yes;

	memset(sieve, -1, high * sizeof(unsigned));
	clrbit(sieve, 0);
	clrbit(sieve, 1);

	for (n = 2; n < sieve_max; ++n)
		if (getbit(sieve, n))
			for(m = n + n; m < sieve_max; m += n)
				clrbit(sieve, m);

//	for (n = 0; n < sieve_max; ++n)
//		if (getbit(sieve, n))
//			printf("%u ", n);
//	printf("\n");

	for (n = 3; n < sieve_max; n += 2) {
		if (getbit(sieve, n) == 0) {
			yes = 1;
			for (m = 1; m < n; ++m) {
				if (getbit(sieve, m) == 0)
					continue;
				c = n - m;
				if ((c & 1) == 1)
					continue;
				d = c / 2;
				r = (unsigned)sqrt((double)d);
				if (r * r != d) {
					//printf("%u <> %u + 2*%u^2"
						//" <> %u + 2*%u"
						//" <> %u + %u"
						//"\n"
						//, n, m, r, m, d, m, c);
					yes = 0;
				} else {
					//printf("%u = %u + 2*%u^2"
						//" = %u + 2*%u"
						//" = %u + %u"
						//"\n"
						//, n, m, r, m, d, m, c);
					yes = 1;
					break;
				}
			}
			if (!yes) {
				printf("Smallest composite "
					"falsifying Goldbach's "
					"conjecture: %u\n", n);
				break;
			}
		}
	}
	return 0;
}

