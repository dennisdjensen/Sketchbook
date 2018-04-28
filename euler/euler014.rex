numeric digits 20
startnum = 0
maxk = 0
do i = 1 to 1000000
	n = i
	k = 1
	do while n <> 1
		if n // 2 == 0 then
			n = n / 2
		else
			n = n * 3 + 1
		if datatype(memo.n) == 'NUM' then do
			k = k + memo.n
			leave
		end
		k = k + 1
	end
	memo.i = k
	if k > maxk then do
		startnum = i
		maxk = k
	end
end
say "Starting number" startnum "has" maxk "terms in the Collatz chain."

