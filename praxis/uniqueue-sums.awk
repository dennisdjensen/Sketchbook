BEGIN {
	for (a = 100; a < 500; ++a)
		for (b = a+1; b < 1000; ++b) {
			# to avoid redundant solutions
			c = a + b
			if (c > 100 && c < 1000 && unique_digits(a, b, c))
				printf("%3d + %3d = %3d\n", a, b, c)
		}
}

function unique_digits(a, b, c,    	s, i, digits)
{
	s = sprintf("%03d%03d%03d", a, b, c)
	for (i = 1; i <= length(s); ++i)
		if (++digits[substr(s, i, 1)] > 1)
			return 0
	return 1
}

