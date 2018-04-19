% Copyright 2018 Dennis Decker Jensen
% Date: 19 April 2018
% Purpose: The sum of all curious numbers = sum of the factorial of their digits.
% Tectonics: mmc --make euler0034
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0034.

% A curious number is 145 = 1! + 4! + 5! = 1 + 24 + 120 = 25 + 120 = 145.
% Find the sum of all curious numbers, i.e. numbers,
% which are equal to the sum of of the factorial of their digits.
%
% An upper limit of curious numbers is on the largest factorial, 9!
%
% facdigitsum(99999) = fac(9) * 5 = 1'814'400
% facdigitsum(999999) = fac(9) * 6 = 2'177'280
% facdigitsum(9999999) = fac(9) * 7 = 2'540'160
% facdigitsum(99999999) = fac(9) * 8 = 2'903'040
%
% Of course, any smaller digit anywhere gives a lesser number
% that cannot possibly reach the same number of digits.
% On the other hand there is no reason
% to check numbers > 9999999 (7 digits).
% They cannot be curious numbers, because their factorial digit sum 
% cannot reach the same number of digits as the number itself.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.
:- import_module char.
:- import_module solutions.
:- import_module list.

:- pred fac(int::in, int::out) is semidet.
fac(0, 1).
fac(1, 1).
fac(2, 2).
fac(3, 6).
fac(4, 24).
fac(5, 120).
fac(6, 720).
fac(7, 5040).
fac(8, 40320).
fac(9, 362880).

main(!IO) :-
	solutions(curious, Curious_numbers),
	Sum = list.foldl(func(N, Acc) = N +Acc, Curious_numbers, 0),
	print("Sum of curious numbers: ", !IO),
	print(Sum, !IO),
	nl(!IO).

:- pred curious(int::out) is nondet.
curious(N) :-
	int.nondet_int_in_range(3, 9999999, N),
	string.foldl(factorial_digit, string.int_to_string(N), 0, Sum),
	Sum = N.

:- pred factorial_digit(char::in, int::in, int::out) is semidet.
factorial_digit(Chardigit, Sum, Fac + Sum) :-
	fac(char.to_int(Chardigit) - 48, Fac).

