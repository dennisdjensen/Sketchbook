% Copyright 2018 Dennis Decker Jensen
% Date: 23 April 2018, well strictly speaking, 30 minutes into 24 April
% Purpose: Largest pandigital prime.
% Tectonics: mmc --make euler041
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler041.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.
:- import_module char.
:- import_module list.
:- import_module float.
:- import_module math.
:- import_module solutions.

main(!IO) :-
	print("Largest pandigital prime: ", !IO),
	nl(!IO),
	pandigital_prime(Prime),
	print_line(Prime, !IO),
	%print_line(length(filter(prime, 1000 `..` 10000)) `with_type` int, !IO),
	%print_line(length(filter(prime1, 1000 `..` 10000)) `with_type` int, !IO),
	%print_line(length(filter(prime2, 1000 `..` 10000)) `with_type` int, !IO),
	nl(!IO).

:- pred pandigital_prime(int::out) is det.
pandigital_prime(Prime) :-
	fold_down((pred(N::in, M0::in, M1::out) is det :-
			(if
				M0 = 0,
				solutions(pandigital_primes(N), Sorted_list),
				last(Sorted_list, Maybe)
			then
				M1 = max(Maybe, M0)
			else
				M1 = M0)
		),
		% We begin with 87654321,
		% because a 9-digit pandigital number
		% is always divisible by 9, and hence not a prime.
		2, 8,
		0, Prime).

:- pred pandigital_primes(int::in, int::out) is nondet.
pandigital_primes(N, Prime) :-
	Chars = map(det_int_to_decimal_digit, 1 `..` N),
	perm(Chars, Ds),
	Prime = det_to_int(from_char_list(Ds)),
	%prime(Prime).  % Wrong answer!
	%prime1(Prime). % Wrong answer!
	prime2(Prime). % Right answer!

:- pred prime(int::in) is semidet.
prime(N) :-
	N = 2; N = 3; N = 5; N = 7; N = 11; N = 13;
	N rem 2 \= 0,
	N rem 3 \= 0,
	N rem 5 \= 0,
	N rem 7 \= 0,
	N rem 11 \= 0,
	N rem 13 \= 0,
	N > 13,
	Root = floor_to_int(sqrt(float(N))),
	0 \= fold_up(func(M, Product) = Product * (N rem M), 17, Root, 1).

:- pred prime1(int::in) is semidet.
prime1(N) :-
	N >= 2,
	Root = floor_to_int(sqrt(float(N))),
	0 \= fold_up(func(M, Product) = Product * (N rem M), 2, Root, 1).

:- pred prime2(int::in) is semidet.
prime2(N) :-
	N >= 2,
	prime(N, 2, floor_to_int(sqrt(float(N)))).

:- pred prime(int::in, int::in, int::in) is semidet.
prime(_, Low, High) :- Low > High.
prime(N, Low, High) :-
	N rem Low \= 0,
	prime(N, Low + 1, High).

