
ones = "one two three four five six seven eight nine"
teens = "ten eleven twelve thirteen fourteen fifteen",
	"sixteen seventeen eighteen nineteen"
decems = "twenty thirty forty fifty sixty seventy eighty ninety"
hundred_len = length("hundred")
and_len = length("and")
thousand_len = length(word(ones,1) || "thousand")

ones_len = 0
do i = 1 to words(ones)
	ones_len = ones_len + length(word(ones,i))
end

teens_len = 0
do i = 1 to words(teens)
	teens_len = teens_len + length(word(teens,i))
end

decems_len = 0
do i = 1 to words(decems)
	decems_len = decems_len + 10 * length(word(decems,i))
	decems_len = decems_len + ones_len
end

below_100 = ones_len + teens_len + decems_len
hundreds_len = 0
do i = 1 to words(ones)
	hundred_prefix = length(word(ones,i)) + hundred_len
	hundreds_len = hundreds_len + 100 * hundred_prefix
	hundreds_len = hundreds_len + 99 * and_len
	hundreds_len = hundreds_len + below_100
end

total_len = below_100 + hundreds_len + thousand_len
say "Length of spelled out numbers 1..1000:" total_len

