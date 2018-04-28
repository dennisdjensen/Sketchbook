% Copyright 2018 Dennis Decker Jensen
% Date: 20 April 2018
% Purpose: the sum of the only eleven primes
%          that are both truncatable from left to right and right to left.
%          2, 3, 5, and 7 are not considered to be truncatable primes.
% Tectonics: mmc --make euler037
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler037.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module set.
:- import_module tree_bitset.
:- import_module string.
:- import_module list.
:- import_module solutions.

main(!IO) :-
	print_line("Truncatable primes:", !IO),
	% Yes, I just used trial and error to go up to one million.
	Truncatable_primes = truncatable_primes_upto(1000000),
	foldl(print_line, Truncatable_primes, !IO),
	print("Sum of truncatable primes: ", !IO),
	print(sum(Truncatable_primes), !IO),
	nl(!IO).

:- func sum(tree_bitset(int)) = int.
sum(Numbers) = foldl(plus, Numbers, 0).

:- func truncatable_primes_upto(int) = tree_bitset(int).
truncatable_primes_upto(N) = Truncatable_primes :-
	Primes = primes_upto(N),
	filter((pred(P::in) is semidet :- truncatable_prime(Primes, P)),
		Primes) = Truncatable_primes.

:- pred truncatable_prime(tree_bitset(int)::in, int::in) is semidet.
truncatable_prime(Primes, N) :-
	not(member(N, [2, 3, 5, 7])),
	member(N, Primes),
	Suffices = map(det_to_int, delete_all(suffices(int_to_string(N)), "")),
	subset(list_to_set(Suffices), Primes),
	Prefixes = map(det_to_int, delete_all(prefixes(int_to_string(N)), "")),
	subset(list_to_set(Prefixes), Primes).

:- func prefixes(string) = list(string).
prefixes(S) = map(reverse_string, suffices(reverse_string(S))).

:- func suffices(string) = list(string).
suffices(S) = Suffices :-
	solutions((pred(Suffix::out) is multi :- suffix(S, Suffix)), Suffices).

:- func reverse_string(string) = string.
reverse_string(S) = from_char_list(reverse(to_char_list(S))).

:- func primes_upto(int) = tree_bitset(int).
primes_upto(High) = erasthotene_sieve(sorted_list_to_set(2 `..` High), 2, High).

:- func erasthotene_sieve(tree_bitset(int), int, int) = tree_bitset(int).
erasthotene_sieve(Candidates, Low, High) =
	(if Low < High then
		erasthotene_sieve(
			remove_multipla_between(Low + Low, High, Low, Candidates),
			Low + 1, High)
	else
		Candidates).

:- func remove_multipla_between(int, int, int, tree_bitset(int)) = tree_bitset(int).
remove_multipla_between(Low, High, Step, Candidates) =
	(if Low < High then
		remove_multipla_between(Low + Step, High, Step, delete(Candidates, Low))
	else
		Candidates).

