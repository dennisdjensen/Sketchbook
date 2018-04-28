% Copyright 2018 Dennis Decker Jensen
% Date: 28 April 2018
% Purpose: Coded triangle numbers.
% Tectonics: mmc --make euler0042
% Mercury: mercurylang.org - a logic/functinal programming language.

:- module euler0042.

% The Nth term of the sequence of triangle numbers, t_n = n(n+1)/2.
% t_1 .. t_10 = 1, 3, 6, 10, 15, 21, 28, 36, 45, 55

% This is a procedural solution, which is not too ackward,
% but then again not exactly pretty in Mercurial.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module tree_bitset.
:- import_module string.
:- import_module list.
:- import_module char.

main(!IO) :-
	print_line("The first 10 triangle numbers:", !IO),
	fold_up((pred(N::in, In::di, Out::uo) is det :-
		print_line(t(N), In, Out)), 1, 10, !IO),
	Triangles = fold_up(func(N, T) = insert(T, t(N)),
		1, 26 * 15, tree_bitset.init),
	print("No of triangle numbers in bitset: ", !IO),
	write_int(count(Triangles), !IO), nl(!IO),
	print("Number of triangle words: ", !IO),
	open_input("p042_words.txt", Result, !IO),
	(if Result = ok(Stream) then
		read_file(Stream, Read_result, !IO),
		(if Read_result = ok(Cs) then
			Quoted_words = words_separator(unify(','),
				string.from_char_list(Cs)),
			Words = list.map(func(QW) =
				remove_prefix_if_present("\"",
					remove_suffix_if_present("\"", QW)),
				Quoted_words),
			%foldl(print_line, Words, !IO),
			foldl2((pred(W::in,
					In::in, Out::out,
					!.IO::di, !:IO::uo) is det :-
				(if member(word_t(W), Triangles) then
					Out = In + 1
					%write_string(W ++ ": ", !IO),
					%write_int(word_t(W), !IO),
					%nl(!IO)
				else
					Out = In)),
				Words, 0, No_triangle_words, !IO),
			write_int(No_triangle_words, !IO)
		else
			print_line("Read from file failed!", !IO)
		),
		close_input(Stream, !IO)
	else
		print_line("Could not open file!", !IO)
	),
	nl(!IO).

:- func t(int) = int.
t(N) = N*(N+1)/2.

:- func word_t(string) = int.
word_t(S) = foldl(func(C, Sum) =
	Sum + char.to_int(C) - char.to_int('A') + 1,
	S, 0).

