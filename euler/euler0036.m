% Copyright 2018 Dennis Decker Jensen
% Date: 20 April 2018
% Purpose: Palindromes in both base 10 and 2 less a million
% Tectonics: mmc --make euler0036
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0036.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.
:- import_module list.
:- import_module solutions.

main(!IO) :-
	print("Sum of numbers palindrome in base 10 and 2 < 1000000: ", !IO),
	solutions(palindrome_nondet, Palindromes),
	Sum = sum(Palindromes),
	print(Sum, !IO),
	nl(!IO),
	foldl(print_line, Palindromes, !IO),
	nl(!IO).

:- pred palindrome_nondet(int::out) is nondet.
palindrome_nondet(Palindrome) :-
	nondet_int_in_range(0, 999999, Base_10_number),
	int_to_string(Base_10_number, Candidate_base_10),
	palindrome(Candidate_base_10),
	int_to_base_string(Base_10_number, 2, Candidate_base_2),
	palindrome(Candidate_base_2),
	Palindrome = Base_10_number.

:- pred palindrome(string::in) is semidet.
palindrome(String) :-
	left(String, 1) \= "0",
	String = from_char_list(reverse(to_char_list(String))).

:- func sum(list(int)) = int.
sum(Numbers) = foldl(plus, Numbers, 0).

