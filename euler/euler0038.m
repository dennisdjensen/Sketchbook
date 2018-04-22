% Copyright 2018 Dennis Decker Jensen
% Date: 22 April 2018
% Purpose: Largest pandigital multiple.
% Tectonics: mmc --make euler0038
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0038.

% Yes, I could have used
%   * integer bits as sets
%   * and shifting and multiplying instead of division
% See the book "Hacker's Delight".

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module list.
:- import_module solutions.

main(!IO) :-
	print("Pandigital multiples:", !IO),
	nl(!IO),
	solutions(multiples, Multiples0),
	Multiples = map(func({P, N, M}) = {det_to_int(P), N, M}, Multiples0),
	foldl(print_multiple, Multiples, !IO),
	nl(!IO),
	print("Largest pandigital multiple: ", !IO),
	print(foldl(func({P,_,_}, Acc) = max(P, Acc), Multiples, 0), !IO),
	nl(!IO).

:- pred print_multiple({int, int, int}::in, io::di, io::uo) is det.
print_multiple({P, N, M}, !IO) :-
	write_int(P, !IO),
	write_string(" = ", !IO),
	write_int(N, !IO),
	write_string(" * (1 .. ", !IO),
	write_int(M, !IO),
	write_char(')', !IO),
	nl(!IO).

:- pred multiples({string, int, int}::out) is nondet.
multiples({Pandigital, N, M}) :-
	% We want a nested loop rather in order to short cut.
	nondet_int_in_range(1, 987654321, N),
	nondet_int_in_range(2, 9, M),
	build_pandigital(N, 1 `..` M, "", Candidate),
	pandigital(Candidate),
	Candidate = Pandigital.

:- pred build_pandigital(int::in, list(int)::in, string::in, string::out) is semidet.
build_pandigital(_, [], Candidate, Candidate) :-
	length(Candidate) = 9.
build_pandigital(N, [M | Ms], S, Candidate) :-
	S1 = append(S, int_to_string(N * M)),
	length(S1) =< 9,
	build_pandigital(N, Ms, S1, Candidate).

:- pred pandigital(string::in) is semidet.
pandigital(S) :-
	det_to_int(S) rem 9 = 0, % eliminates around 89%
	Digits = map(det_decimal_digit_to_int, to_char_list(S)),
	(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) = sum(Digits),
	(1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9) = product(Digits). % 9! = 362880

:- func sum(list(int)) = int.
sum(Numbers) = foldl(plus, Numbers, 0).

:- func product(list(int)) = int.
product(Numbers) = foldl(times, Numbers, 1).

