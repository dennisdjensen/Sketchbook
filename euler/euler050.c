// Copyright 2021 Dennis Blondell Decker
// Date: 30 May 2021
// Purpose: Consecutive prime sum
// Tectonics: cc -o euler050 euler050.c

#include <stdio.h>
//#include <string.h>

// Zero base indexed bitset function-like macros.
#define bitword (8 * sizeof(unsigned char) * sizeof(unsigned int))
#define bitidx(i) ((i) / bitword)
#define getbit(set, i) ((set[bitidx(i)] >>        ((bitword - 1) & (i))) & 1)
#define clrbit(set, i)  (set[bitidx(i)] &= ~(1 << ((bitword - 1) & (i))))
#define setbit(set, i)  (set[bitidx(i)] |=  (1 << ((bitword - 1) & (i))))
#define invbit(set, i)  (set[bitidx(i)] ^=  (1 << ((bitword - 1) & (i))))

int
main(int argc, char *argv[])
{
	const unsigned long mx = 1000000;
	const unsigned long mxsieve = bitidx(mx)+1;
	unsigned long sieve[mxsieve], primes[mx], arr[mx+1];
	unsigned long *cumarr = arr+1;
	unsigned long i, j, k, l, m, n, mxprimes, sum, mxsofar;

	return 0;
	printf("-----\n");
	for (i=0; i<mx; ++i) primes[i]=arr[i]=0;
	arr[mx]=0;
	//memset(primes,0,mx*sizeof(unsigned long));
	//memset(arr,0,(mx+1)*sizeof(unsigned long));

	printf("-----\n");
	// Sieve of Erasthothene for 0..mx
//	memset(sieve,-1,mxsieve*sizeof(unsigned));
	for (i=0; i<mxsieve; ++i) sieve[i]=-1;
	clrbit(sieve,0);
	clrbit(sieve,1);
	for (n=2; n<mx; ++n)
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
	{
		//printf("%d %d %d\n",i-1,*(cumarr+i-1),primes[i]);
		cumarr[i]=*(cumarr+i-1)+primes[i];
	}

	//for (i=0; i<mxprimes; ++i)
	//	printf("%6lu %6lu %6lu %6lu\n",
	//		i,primes[i],cumarr[i],*(cumarr+i-1));

	mxsofar=k=l=0;
	for (i=0; i<mxprimes; ++i)
		for (j=i; j<mxprimes; ++j)
			if ((sum=cumarr[j]-*(cumarr+i-1))<mx)
			{
				//printf("i=%lu,j=%lu,sum=%lu\n",i,j,sum);
				if (getbit(sieve,sum))
					if ((l-k)<(j-i))
						k=i,l=j,mxsofar=sum;
			}
	printf("Sum: %6lu with %6lu primes:",mxsofar,l-k+1);
	for (i=k; i<=l; ++i) printf(" %6lu", i);
	printf("\n");
}

