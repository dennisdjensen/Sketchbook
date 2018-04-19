% Copyright 2018 Dennis Decker Jensen
% Date: 19 April 2018
% Purpose: Find four non-trivial digit cancelling fractions.
%          Give the lowest common denominator of their product.
% Tectonics: mmc --make euler0033
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0033.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module rational.
:- import_module solutions.
:- import_module list.

main(!IO) :-
	unsorted_solutions(fraction, Fractions), % cc_multi
	Product = list.foldl(func({A, B}, Acc) = rational(A, B) * Acc,
		Fractions, rational.one),
	print("Fractions: ", !IO),
	print_line(Fractions, !IO),
	print("Product: ", !IO),
	print(numer(Product), !IO),
	print("/", !IO),
	print(denom(Product), !IO),
	nl(!IO).

:- pred fraction({int, int}::out) is nondet.
fraction({A, B}) :-
	nondet_int_in_range(10, 98, A),		% Fraction A/B < 1.0
	nondet_int_in_range(A + 1, 99, B),
	divide_with_rem(A, 10, A10, A1),	% Tens and ones.
	divide_with_rem(B, 10, B10, B1),
	A1 \= 0, B1 \= 0,			% No simple fractions.
	non_trivial(rational(A, B), A10, A1, B10, B1).

:- pred non_trivial(rational::in, int::in, int::in, int::in, int::in) is semidet.
non_trivial(AB_fraction, A10, A1, B10, B1) :-
    (
        A10 = B1,
        rational(A1, B10) = AB_fraction
    ;
        A1 = B10,
        rational(A10, B1) = AB_fraction
    ).

:- pred divide_with_rem(int::in, int::in, int::out, int::out) is det.
divide_with_rem(A, B, Q, R) :-
	Q = A div B,
	R = A rem B.

