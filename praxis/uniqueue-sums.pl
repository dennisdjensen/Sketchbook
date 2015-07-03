% Sum problem:
% A + B = C, and they are all between 0 and 999,
% and each digit in A, B, and C are only used once.
% Corollary: A, B, and C are all >= 100.

% There is supposed to be 592 unique solutions,
% but this program only finds 544 solutions.
% Removing constraints such as B > A confirms this.
% It just doubles the number of solutions.

% gplc --no-top-level sum.pl
% gprolog --query-goal "consult('sum.pl')"

main :-
	write_summing_fd; halt.

:- initialization(main).

% between(+Low, +High, ?Value)
between(Low, High, Low) :- Low =< High.
between(Low, High, Value) :- var(Value), Low < High,
	Low1 is Low + 1,
	between(Low1, High, Value).
between(Low, High, Value) :- nonvar(Value), % optimization for checking C
	Low =< Value,
	Value =< High.

% How about between(Low, inf, Value) or between(Low, infinite, Value)?
upfrom(Min, Min).
upfrom(Min, Out) :- Min1 is Min + 1, upfrom(Min1, Out).

% summing_pl(?A, ?B, ?C)
summing_pl(A, B, C) :-
	between(100,499,A),
	between(A,999,B), % to avoid redundant solutions
	C is A + B,
	between(0,999,C),
	unique_digits(A,B,C).

% unique_digits(+A, +B, +C)
unique_digits(A, B, C) :-
	number_codes(A, As),
	number_codes(B, Bs),
	number_codes(C, Cs),
	append(Bs, Cs, N0),
	append(As, N0, N),
	fd_domain(N,48,57), % 0..9 => ASCII 48..57
	fd_all_different(N).

write_summing_pl :-
	summing_pl(A,B,C),
	format('%3d + %3d = %3d', [A,B,C]),
	nl,
	fail.

% Faster finite domain solution using constraint programming.
% Well, sort of, faster on a command prompt, but
% not when consulting this file. There is barely any
% difference, 230ms versus 270ms.
% For larger sets of data perhaps.
% summing_fd(?A, ?B, ?C)
summing_fd(A, B, C) :-
	fd_domain([A,B,C],100,999),
	A #=< 500, B #>= A, % to avoid redundant solutions
	C #= A + B,
	fd_labeling([A,B,C]), % instantiate numbers
	unique_digits(A,B,C).

write_summing_fd :-
	summing_fd(A,B,C),
	format('%3d + %3d = %3d', [A,B,C]),
	nl,
	fail.
