% bigint-test.pl - test for big integers
% Copyright 2015 Dennis Decker Jensen <ddj_sf@mailc.net>

:- include(bigint).

:- public(test/1).

test(bn_zero) :-
	bn_zero(bigint(0,[])).

test(bn_unit) :-
	bn_unit(bigint(1,[1])).

test(bn_minus_one) :-
	bn_neg_unit(bigint(-1,[1])).

test(bn_positive1) :-
	bn_unit(One),
	bn_positive(One).

test(bn_positive2) :-
	bn_zero(Zero),
	\+ bn_positive(Zero).

test(bn_positive3) :-
	bn_neg_unit(Mone),
	\+ bn_positive(Mone).

test(bn_negative1) :-
	bn_neg_unit(Mone),
	bn_negative(Mone).

test(bn_negative2) :-
	bn_zero(Zero),
	\+ bn_negative(Zero).

test(bn_negative3) :-
	bn_unit(One),
	\+ bn_negative(One).

test(bn_negation1) :-
	bn_unit(One),
	bn_negation(One,Minus),
	bn_neg_unit(Minus).

test(bn_negation2) :-
	bn_neg_unit(Minus),
	bn_negation(One1,Minus),
	bn_unit(One1).

test(bn_abs1) :-
	bn_unit(One),
	bn_abs(One,One).

test(bn_abs2) :-
	bn_zero(Zero),
	bn_abs(Zero,Zero).

test(bn_abs3) :-
	bn_neg_unit(Mone),
	bn_abs(Mone,One),
	bn_unit(One).

test(bn_odd1) :-
	bn_unit(One),
	bn_odd(One).

test(bn_odd2) :-
	bn_odd(bigint(1,[3])).

test(bn_odd3) :-
	bn_odd(bigint(-1,[5])).

test(bn_odd4) :-
	\+ bn_odd(bigint(-1,[4])).

test(bn_even1) :-
	bn_zero(Zero),
	bn_even(Zero).

test(bn_even2) :-
	bn_even(bigint(1,[2])).

test(bn_even3) :-
	bn_even(bigint(-1,[4])).

test(bn_even4) :-
	\+ bn_even(bigint(-1,[7])).

test(bn_lt1) :-
	bn_zero(Zero), bn_unit(One),
	bn_lt(Zero,One).

test(bn_lt2) :-
	bn_zero(Zero), bn_neg_unit(Mone),
	bn_lt(Mone,Zero).

test(bn_lt3) :-
	bn_unit(One), bn_neg_unit(Mone),
	bn_lt(Mone,One).

test(bn_lt4) :-
	bn_unit(One), bn_neg_unit(Mone),
	\+ bn_lt(One,Mone).

test(bn_lt5) :-
	integer_bigint(123456789,Big),
	integer_bigint(-87654321,Small),
	bn_lt(Small,Big).

test(bn_lt6) :-
	integer_bigint(123456789,Longer),
	integer_bigint(456789,Shorter),
	bn_lt(Shorter,Longer).

test(bn_lt7) :-
	integer_bigint(123456789,Longer),
	integer_bigint(456789,Shorter),
	\+ bn_lt(Longer,Shorter).

test(bn_gt1) :-
	bn_zero(Zero), bn_unit(One),
	bn_gt(One,Zero).

test(bn_gt2) :-
	bn_zero(Zero), bn_neg_unit(Mone),
	bn_gt(Zero,Mone).

test(bn_gt3) :-
	bn_unit(One), bn_neg_unit(Mone),
	bn_gt(One,Mone).

test(bn_gt4) :-
	bn_unit(One), bn_neg_unit(Mone),
	\+ bn_gt(Mone,One).

test(bn_le1) :-
	bn_zero(Zero),
	bn_le(Zero,Zero).

test(bn_le2) :-
	bn_zero(Zero), bn_unit(One),
	bn_le(Zero,One).

test(bn_le3) :-
	bn_zero(Zero), bn_unit(One),
	\+ bn_le(One,Zero).

test(bn_ge1) :-
	bn_zero(Zero),
	bn_ge(Zero,Zero).

test(bn_ge2) :-
	bn_zero(Zero), bn_unit(One),
	bn_ge(One,Zero).

test(bn_ge3) :-
	bn_zero(Zero), bn_unit(One),
	\+ bn_ge(Zero,One).

test(bn_eq) :-
	bn_unit(One),
	bn_eq(One,One).

test(bn_ne) :-
	bn_zero(Zero), bn_unit(One),
	\+ bn_eq(One,Zero).

test(int1) :-
	integer_bigint(123456789,bigint(3,[789,456,123])).

test(int2) :-
	integer_bigint(-87654321,bigint(-3,[321,654,87])).

test(int3) :-
	integer_bigint(N,bigint(4,[789,456,123])),
	N = 123456789.

test(int4) :-
	integer_bigint(N,bigint(-3,[321,654,87])),
	N = -87654321.

test(int5) :-
	integer_bigint(3456789,bigint(3,[789,456,3])).

%test(int7) :- % Fail driven loop to catch semi- or non-determinism.
%    (   integer_bigint(123456789,B),
%	B = bigint(3,[789,456,123]),
%	fail
%    ;
%	true
%    ).

test(bigint1) :-
	atom_bigint('123456789123456789',bigint(6,[789,456,123,789,456,123])).

test(bigint2) :-
	atom_bigint('-123456789123456789',bigint(-6,[789,456,123,789,456,123])).

test(bigint3) :-
	atom_bigint(A,bigint(6,[789,456,123,789,456,123])),
	A = '123456789123456789'.

test(bigint4) :-
	atom_bigint(A,bigint(-6,[789,456,123,789,456,123])),
	A = '-123456789123456789'.

test(bigint5) :-
	atom_bigint('3456789',bigint(3,[789,456,3])).

test(bigint6) :-
	atom_bigint('0',bigint(0,[])).

test(bigint7) :-
	atom_bigint('0000',B),
	atom_bigint(A,B),
	A = '0'.

test(bigint8) :-
	atom_bigint('-0000',B),
	atom_bigint(A,B),
	A = '0'.

test(bigint9) :-
	atom_bigint('00001',B),
	atom_bigint(A,B),
	A = '1'.

test(bigint10) :-
	atom_bigint('-00001',B),
	atom_bigint(A,B),
	A = '-1'.

test(bigint11) :-
	atom_bigint(A,bigint(4,[789,456,123])),
	A = '123456789'.

test(bigint12) :-
	atom_bigint(A,bigint(-3,[321,654,87])),
	A = '-87654321'.

test(add1) :-
	atom_bigint('123456789999',X),
	bn_unit(Y),
	bn_add(X,Y,bigint(4,[0,790,456,123])).

test(add2) :-
	atom_bigint('3999999999999',X),
	bn_unit(Y),
	bn_add(X,Y,Z),
	Z = bigint(5,[0,0,0,0,4]).

test(add3) :-
	atom_bigint('-123456789999',X),
	bn_neg_unit(Y),
	bn_add(X,Y,bigint(-4,[0,790,456,123])).

test(add4) :-
	atom_bigint('-3999999999999',X),
	bn_neg_unit(Y),
	bn_add(X,Y,Z),
	Z = bigint(-5,[0,0,0,0,4]).

test(add5) :-
	atom_bigint('-999999999999999',X),
	atom_bigint('-1',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '-1000000000000000'.

test(add6) :-
	atom_bigint('123456789999',X),
	atom_bigint('-00000000999',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '123456789000'.

test(add7) :-
	atom_bigint('0000000000999',X),
	atom_bigint('-123456789999',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '-123456789000'.

test(add8) :-
	atom_bigint('-17',X),
	atom_bigint('17',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '0'.

test(add9) :-
	atom_bigint('-17',X),
	atom_bigint('18',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '1'.

test(add10) :-
	atom_bigint('999999999999',X),
	atom_bigint('000000000001',Y),
	bn_add(X,Y,Z),
	atom_bigint(A,Z),
	A = '1000000000000'.

test(sub1) :-
	atom_bigint('1000000000000',X),
	bn_unit(Y),
	bn_sub(X,Y,Z),
	Z = bigint(4,[999,999,999,999]).

test(sub2) :-
	atom_bigint('-999999999999999',X),
	atom_bigint('1',Y),
	bn_sub(X,Y,Z),
	atom_bigint(A,Z),
	A = '-1000000000000000'.

test(sub3) :-
	atom_bigint('17',X),
	atom_bigint('34',Y),
	bn_sub(X,Y,Z),
	atom_bigint(A,Z),
	A = '-17'.

test(sub4) :-
	atom_bigint('17',X),
	atom_bigint('17',Y),
	bn_sub(X,Y,Z),
	atom_bigint(A,Z),
	A = '0'.

test(sub5) :-
	atom_bigint('17',X),
	atom_bigint('18',Y),
	bn_sub(X,Y,Z),
	atom_bigint(A,Z),
	A = '-1'.

test(mul1) :-
	atom_bigint('123456789',X),
	atom_bigint('0',Y),
	bn_mul(X,Y,Z),
	bn_zero(Z).

test(mul2) :-
	atom_bigint('123456789',X),
	atom_bigint('0',Y),
	bn_mul(Y,X,Z),
	bn_zero(Z).

test(mul3) :-
	atom_bigint('123456789',X),
	atom_bigint('100',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '12345678900'.

test(mul4) :-
	atom_bigint('-123456789',X),
	atom_bigint('100',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '-12345678900'.

test(mul5) :-
	atom_bigint('123456789',X),
	atom_bigint('-100',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '-12345678900'.

test(mul6) :-
	atom_bigint('-1000000',X),
	atom_bigint('-1000000',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '1000000000000'.

test(mul7) :-
	atom_bigint('5000000000000',X),
	atom_bigint('7000000000000',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '35000000000000000000000000'.

test(mul8) :-
	atom_bigint('123456789',X),
	atom_bigint('123456789',Y),
	bn_mul(Y,X,Z),
	atom_bigint(B,Z),
	B = '15241578750190521'.

plan(Testnames) :-
	length(Testnames,Len),
	write(`1..`), write(Len), nl.

writeok(No,Name) :-
	write(`ok `), write(No), write(` - `), write(Name), nl.

runtests :-
	findall(Name,clause(test(Name),_Body),Testnames),
	plan(Testnames),
	runtests(Testnames, 1),
	halt.

runtests([Name|Rest],No) :-
	(test(Name); write(`not `)), % backtracking test is not caught
	writeok(No,Name),
	No1 is No + 1,
	runtests(Rest,No1).
runtests([],_).

setup_tests :-
	abolish(bn_radix/1),
	assertz(bn_radix(1000)).

test :-
	setup_tests,
	runtests.

main :- test.

%:- initialization(main).

% TODO. Context:
% Fix testing to handle nondeterminism. Right now only semidet. is caught.

