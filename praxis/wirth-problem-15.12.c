/* Copyright 2012 Dennis Decker Jensen

http://programmingpraxis.com/2012/12/07/wirth-problem-15-12/

In his 1973 book "Systematic Programming: An Introduction", Niklaus Wirth
gives the following problem as exercise 15.12:

Develop a program that generates in ascending order the least 100 numbers
of the set M, where M is defined as follows:

a) The number 1 is in M.
b) If x is in M, then y = 2 * x + 1 and z = 3 * x + 1 are also in M
c) No other numbers are in M

Wirth also gives the first six numbers in the result sequence:
1, 3, 4, 7, 9, 10, ...

Your task is to write a program that finds the first N numbers in Wirth's
sequence.

*/

#include <stdio.h>

void
wirth(unsigned int n)
{
	unsigned int y, z, iy, iz, i, m[n], min;

	m[0] = 1;
	printf("  1:   1\n");
	iy = iz = 0;
	for (i = 1; i < 100; ++i) {
		y = 2 * m[iy] + 1;
		z = 3 * m[iz] + 1;
		min = z < y ? z : y;
		m[i] = min;
		printf("%3d: %3d\n", i+1, min);
		if (y <= m[i]) ++iy;
		if (z <= m[i]) ++iz;
	}

	return;
}

int
main(int argc, char *argv[])
{
	wirth(100);
	return 0;
}
