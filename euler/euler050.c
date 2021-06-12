// Copyright 2021 Dennis Blondell Decker
// Date: 30 May 2021
// Purpose: Consecutive prime sum
// Tectonics: cc -o euler050 euler050.c

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Zero base indexed bitset function-like macros.
#define bitword (8 * sizeof(unsigned char) * sizeof(unsigned int))
#define bitidx(i) ((i) / bitword)
#define getbit(set, i) ((set[bitidx(i)] >>        ((bitword - 1) & (i))) & 1)
#define clrbit(set, i)  (set[bitidx(i)] &= ~(1 << ((bitword - 1) & (i))))
#define setbit(set, i)  (set[bitidx(i)] |=  (1 << ((bitword - 1) & (i))))
#define invbit(set, i)  (set[bitidx(i)] ^=  (1 << ((bitword - 1) & (i))))

typedef unsigned long ulong;

int
main(int argc, char *argv[])
{
	const ulong mx = 1000000;
	const ulong mxsieve = bitidx(mx)+1;
	ulong sieve[mxsieve];
	ulong *primes, *cumarr, *arr;
	ulong i, j, k, l, m, n, mxprimes, sum, mxsofar;
	ulong sqrtmx;

	primes=calloc(mx,sizeof(ulong));
	arr=calloc(mx+1,sizeof(ulong));
	cumarr=arr+1;

	// Sieve of Erasthothene for 0..mx
	memset(sieve,-1,mxsieve*sizeof(ulong));
	clrbit(sieve,0);
	clrbit(sieve,1);
	sqrtmx = (ulong)sqrt((double)mx);
	for (n=2; n<sqrtmx; ++n)
		if (getbit(sieve,n))
			for (m=n+n; m<mx; m+=n)
				clrbit(sieve,m);

	// Collect primes into a contiguous array
	for (j=0,i=0; i<mx; ++i)
		if (getbit(sieve,i))
			primes[j++]=i;
	mxprimes=j;
	printf("\nmxprimes=%lu\n", mxprimes);

	//printf("Primes: ");
	//for (i=0; i<mxprimes; ++i)
	//	printf(" %d", primes[i]);
	//printf("\n");

	// treat this problem like a maximum sum subarray problem.
	// Bentley's quadratic variation of Kandane's Algorithm.
	// Instead of the maximum sum, we find the maximum length.

	cumarr[-1]=0;
	for (i=0; i<mxprimes; ++i)
		cumarr[i]=*(cumarr+i-1)+primes[i];

	mxsofar=k=l=0;
	for (i=0; i<mxprimes; ++i)
		for (j=i; j<mxprimes; ++j)
			if ((sum=cumarr[j]-*(cumarr+i-1))<mx)
				if (getbit(sieve,sum))
					if ((l-k)<(j-i))
						k=i,l=j,mxsofar=sum;

	printf("Sum: %6lu with %6lu primes.\n",mxsofar,l-k+1);
	//for (i=k; i<=l; ++i) printf(" %6lu", i);
	//printf("\n");
}

