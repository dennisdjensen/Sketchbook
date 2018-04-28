% Copyright 2018 Dennis Decker Jensen
% Date: 28 April 2018
% Purpose: Sub-string divisibility.
% Tectonics: mmc --make euler043
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler043.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module integer.
:- import_module string.
:- import_module list.
:- import_module char.
:- import_module solutions.

main(!IO) :-
	write_string(string.between("1406357289", 1, 4), !IO), nl(!IO),
	(if substring_divisible("1406357289") then
		print_line("1406357289 has it!", !IO)
	else
		print_line("1406357289 has been lost to us!", !IO)
	),
	nl(!IO),
	print_line("0 to 9 pandigital numbers with substr-div prop:", !IO),
	solutions(substring_divisible_pandigital, Numbers),
	foldl(print_line, Numbers, !IO),
	print_line("The sum of all 0 to 9 pandigital numbers", !IO),
	print("with the sub-string divisibility property: ", !IO),
	print_line(sum(map(det_to_int, Numbers)), !IO).

:- pred substring_divisible_pandigital(string::out) is nondet.
substring_divisible_pandigital(Pandigital) :-
	perm(to_char_list("0123456789"), Ds),
	string.from_char_list(Ds) = Pandigital,
	substring_divisible(Pandigital).

:- pred substring_divisible(string::in) is semidet.
substring_divisible(Pandigital) :-
	Pandigital = P,
	det_to_int(string.between(P, 1, 4)) rem 2 = 0,
	det_to_int(string.between(P, 2, 5)) rem 3 = 0,
	det_to_int(string.between(P, 3, 6)) rem 5 = 0,
	det_to_int(string.between(P, 4, 7)) rem 7 = 0,
	det_to_int(string.between(P, 5, 8)) rem 11 = 0,
	det_to_int(string.between(P, 6, 9)) rem 13 = 0,
	det_to_int(string.between(P, 7, 10)) rem 17 = 0,
	true.

:- func sum(list(int)) = integer.
sum(Numbers) = foldl(integer.(+), map(integer, Numbers), integer.zero).

