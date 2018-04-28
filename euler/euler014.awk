BEGIN {
	for (i = 1; i < 1000000; ++i) {
		n = i;
		k = 1
		while (n != 1) {
			if (n % 2 == 0)
				n /= 2
			else
				n = n * 3 + 1
			if (n in table) {
				k += table[n]
				break
			} else {
				++k
			}
		}
		table[i] = k
		if (k > maxk) {
			startnum = i
			maxk = k
		}
	}
	printf("Starting number %d has %d terms in the Collatz chain.\n",
		startnum, maxk)
}

