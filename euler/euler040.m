% Copyright 2018 Dennis Decker Jensen
% Date: 22 April 2018
% Purpose: Champernowne's constant
% Tectonics: mmc --make euler040
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler040.

% Champernowne's constant =
% d_1 * d_10 * d_100 * d_1000 * d_1000 * d_10000 * d_100000 * d_1000000
% Trial and error with dc gives
% dc -e '4k 9 1 * 89 2 * + 899 3 * + 8999 4 * + 89999 5 * + 85188 6 * + p'
% 1000003 that we need to count up to in order to make the integer with
% that many digits. We don't need to keep the integer while finding
% the digits though, if we just keep count of the digits.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module char.
:- import_module string.

main(!IO) :-
	print("Champernowne's constant = ", !IO),
	print(champernowne_constant, !IO),
	nl(!IO).

:- func champernowne_constant = int.
champernowne_constant = Product :-
	Digit = (func(S, I) = det_decimal_digit_to_int(det_index(S, I))),
	fold_up3((pred(N::in,
				S0::in, S1::out,
				I0::in, I1::out,
				P0::in, P1::out) is det :-
			S1 = int_to_string(N),
			L = length(S0),
			I1 = I0 + L,
			(if I0 =< 1, I0 + L > 1 then
				P1 = P0 * Digit(S0, (1 - I0))
			else if I0 =< 10, I0 + L > 10 then
				P1 = P0 * Digit(S0, (10 - I0))
			else if I0 =< 100, I0 + L > 100 then
				P1 = P0 * Digit(S0, (100 - I0))
			else if I0 =< 1000, I0 + L > 1000 then
				P1 = P0 * Digit(S0, (1000 - I0))
			else if I0 =< 10000, I0 + L > 10000 then
				P1 = P0 * Digit(S0, (10000 - I0))
			else if I0 =< 100000, I0 + L > 100000 then
				P1 = P0 * Digit(S0, (100000 - I0))
			else if I0 =< 1000000, I0 + L > 1000000 then
				P1 = P0 * Digit(S0, (1000000 - I0))
			else
				P1 = P0)
		),
		1, 1000003,       % Low, High
		"", _,            % String of part of fraction
		1, _,             % One-based index of first digit in string
		1, Product).      % Product of digits giving the constant

