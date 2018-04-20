% Copyright 2018 Dennis Decker Jensen
% Date: 19 April 2018
% Purpose: No of circular primes below one million.
% Tectonics: mmc --make euler0035
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0035.

% This one is deterministic and functional, because we use sets.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module set.
:- import_module tree_bitset.
:- import_module string.
:- import_module list.

main(!IO) :-
	print("Primes < 100: ", !IO),
	print_line(tree_bitset.to_sorted_list(primes_upto(100)) `with_type` list(int), !IO),
	print("Circular primes < 100: ", !IO),
	print_line(tree_bitset.to_sorted_list(circular_primes_upto(100)) `with_type` list(int), !IO),
	print("No of circular primes < 1,000,000: ", !IO),
	write_int(count(circular_primes_upto(1_000_000)), !IO),
	nl(!IO).

:- func circular_primes_upto(int) = tree_bitset(int).
circular_primes_upto(High) = Primes :- circular_primes_upto(High, Primes).

:- pred circular_primes_upto(int::in, tree_bitset(int)::out) is det.
circular_primes_upto(High, Circular_primes) :-
	primes_upto(High) = Primes,
	tree_bitset.foldl((pred(Prime::in, In::in, Out::out) is det :-
		if member(Prime, In) then
			In = Out
		else
			circle(Prime) = Circle,
			(if subset(Circle, Primes) then
				union(In, Circle) = Out
			else
				In = Out
			)
	), Primes, tree_bitset.init, Circular_primes).

%:- pred circular_prime(int::int, tree_bitset(int)::in, tree_bitset(int)::out) is det.
%circular_prime(Prime, In, Out) :-

:- func primes_upto(int) = tree_bitset(int).
primes_upto(High) = Primes :- primes_upto(High, Primes).

:- pred primes_upto(int::in, tree_bitset(int)::out) is det.
primes_upto(High, Primes) :-
	Candidates = sorted_list_to_set(2 `..` High),
	erasthotene_sieve(Candidates, 2, High, Primes).

:- pred erasthotene_sieve(tree_bitset(int)::in, int::in, int::in, tree_bitset(int)::out) is det.
erasthotene_sieve(Candidates, Low, High, Primes) :-
	if Low < High then
		remove_multipla_between(Low + Low, High, Low, Candidates, Candidates1),
		erasthotene_sieve(Candidates1, Low + 1, High, Primes)
	else
		Primes = Candidates.

:- pred remove_multipla_between(int::in, int::in, int::in,
	tree_bitset(int)::in, tree_bitset(int)::out) is det.
remove_multipla_between(Low, High, Step, !Candidates) :-
	if Low < High then
		!:Candidates = delete(!.Candidates, Low),
		remove_multipla_between(Low + Step, High, Step, !Candidates)
	else
		!:Candidates = !.Candidates.

:- func circle(int) = tree_bitset(int).
circle(N) = tree_bitset.insert_list(tree_bitset.init, set.to_sorted_list(set.map(string.det_to_int, circle_string(int_to_string(N))))).

:- func circle_string(string) = set(string).
circle_string(S) = Circle :- circle_string(S, length(S), set.init, Circle).

:- pred circle_string(string::in, int::in, set(string)::in, set(string)::out) is det.
circle_string(S, Len, !Circle) :-
    (   if S = "" then
        	!:Circle = !.Circle
        else if Len = 0 then
        	!:Circle = !.Circle
        else
		split(S, 1, First, Rest),
		!:Circle = insert(!.Circle, S),
		circle_string(Rest ++ First, Len - 1, !Circle)
    ).

