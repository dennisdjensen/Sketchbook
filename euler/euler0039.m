% Copyright 2018 Dennis Decker Jensen
% Date: 22 April 2018
% Purpose: Solutions for integer right rectangles.
% Tectonics: mmc --make euler0039
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0039.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
	print("Solutions for 120 < perimeter =< 1000:", !IO),
	nl(!IO),
	unsorted_aggregate(right_triangle_sides, print_line, !IO),
	nl(!IO).

:- pred right_triangle_sides({int, list({int, int, int})}::out) is nondet.
right_triangle_sides({Perimeter, Sides}) :-
	nondet_int_in_range(121, 1000, Perimeter),
	solutions(sides(Perimeter), Sides),
	length(Sides) > 2.

:- pred sides(int::in, {int, int, int}::out) is nondet.
sides(Perimeter, {A, B, C}) :-
	nondet_int_in_range(1, Perimeter, A),
	nondet_int_in_range(A + 1, Perimeter, B),
	nondet_int_in_range(B + 1, Perimeter, C),
	(A * A) + (B * B) = (C * C),
	A + B + C = Perimeter.

