% Partition a telephone book, a classic problem
% Copyright 2015 Dennis Decker Jensen
% Tectonics: gprolog --consult-file partitioning-the-telephone-book.pl
% gprolog 1.4.4

% http://programmingpraxis.com/2015/07/10/partitioning-the-telephone-book/
%
% There are still cities that print telephone directories, and
% some of those cities are big enough that the directory must be
% partitioned into multiple volumes. Consider this distribution
% of the first letters of customer’s last names:

% A  B C  D  E  F G H I J  K L  M  N 0 P Q R  S  T U V W X Y Z
% 16 4 17 10 15 4 4 6 7 14 9 17 27 6 1 9 0 12 20 8 0 3 4 0 3 4
% 
% I’m not sure what the units are (probably tens of
% thousands of telephone customers), but the total is 220,
% and the telephone company has decided to print 4 volumes,
% so each should be about 55 units. One possible partitioning
% is A-D, E-J, K-O, P-Z, with counts of 47, 50, 60 and 63 units,
% differences of 8, 5, 5 and 8 from the ideal of 55, and a total
% difference of 26. Another partitioning is A-E, F-K, L-O, P-Z,
% with counts of 62, 44, 51, and 63 units, differences of 7, 11,
% 4 and 8, and a total difference of 30, which is worse. Before
% continuing, you might want to work out the optimal solution by
% hand, finding a minimum score of 18.
% 
% Your task is to write a program that determines the
% partitioning of the telephone book that minimizes the total
% difference. When you are finished, you are welcome to read
% or run a suggested solution, or to post your own solution or
% discuss the exercise in the comments below.

% A phone book with this many names and phones per letter
phone_weights([17, 5,18,11,16, 5, 5, 7, 8,15,10,18,28,
                7, 2,10, 1,13,21, 9, 1, 4, 5, 1, 4, 5]).

% Scan sum of number list (like +\ in J)
cumulative(Ns0,Ns1) :-
	cumulative(Ns0,0,Ns1).

cumulative([],_,[]).
cumulative([X|Xs],Acc,[Y|Ys]) :-
	Y is Acc + X,
	cumulative(Xs,Y,Ys).

% Find optimal partition by branch-and-bound algorithm with restart.
% This finds multiple solutions, if there are any.
partition(Phone,A,B,C,Score) :-
	cumulative(Phone,Phone_cum),
	nth(26,Phone_cum,Last),
	Target is Last div 4,
	fd_domain([A,B,C],1,26),
	B #> A, C #> B,
	fd_minimize(score(Phone_cum,Last,Target,A,B,C,Score), Score).

% Minimization goal (with different heuristics)
score(Phone_cum,Last,Target,A,B,C,Score) :-
	%fd_labelingff([A,B,C]),
	%fd_labeling([A,B,C],[value_method(middle)]),
	%fd_labeling([A,B,C],[value_method(max)]),
	%fd_labeling([A,B,C],[value_method(bisect)]),
	%fd_labeling([A,B,C],[value_method(min)]),
	%fd_labeling([A,B,C],[value_method(bounds)]),
	%fd_labeling([A,B,C],[value_method(random)]),
	%fd_labeling([A,B,C],[variable_method(most_constrained)]),
	%fd_labeling([A,B,C],[variable_method(smallest)]),
	%fd_labeling([A,B,C],[variable_method(largest)]),
	%fd_labeling([A,B,C],[variable_method(max_regret)]),
	%fd_labeling([A,B,C],[variable_method(random)]),
	%fd_labeling([A,B,C],[reorder(false)]),
	fd_labeling([A,B,C]),
	nth(A,Phone_cum,X),
	nth(B,Phone_cum,Y),
	nth(C,Phone_cum,Z),
	%format(`X=%d Y=%d Z=%d~n`,[X,Y,Z]),
	Score is abs(X-Target)
		+ abs(Y-X-Target)
		+ abs(Z-Y-Target)
		+ abs(Last-Z-Target).
	%format(`Score=%d~n`,[Score]).

show(A,B,C,Score) :-
	format(`Score = %d: A-%c, %c-%c, %c-%c, %c-Z~n`,
		[Score,64+A-1,64+A,64+B-1,64+B,64+C-1,64+C]).

test(A,B,C,Score) :-
	phone_weights(P),
	partition(P,A,B,C,Score).

go :-
	phone_weights(P),
	partition(P,A,B,C,Score),
	show(A,B,C,Score),
	% Continue until all minimization solutions are found:
	fail; true.

:- initialization(go).

% 8041 bytes written, 6 ms
% Score = 22: A-C, D-J, K-P, Q-Z
% Score = 22: A-C, D-J, K-O, P-Z
% | ?- 

% Branch-and-bound didn't find the minimal solution with score = 18
% That surprises me. I would have expected the value_method(middle)
% to find it, since it works from the middle towards the bounds.
% None of the heuristics differed in any way from the default labelling.

