% bigint.pl - library for big integers
% Copyright 2015 Dennis Decker Jensen <ddj_sf@mailc.net>

% A arbitrary big integer is represented with the term
% bigint(sign,[digits]), where [digits] begins with the least significant
% and ends with most significant, which simplifies all the calculations.
% Only the conversion function to integer needs to see the in reverse.
%    The operations are prefixed with bn for big number, because they
% can be expanded with other types, such as bigfloat.
%    No attempt has been made yet to make operations partial,
% e.g. letting bn_add find all possible additions to a given result.
% However, it does use meta-predicates to handle parameter combinations.
%    Radix is hardwired in a parameter.

:- dynamic(bn_radix/1).

% Choose a much higher radix for 64 bit systems.
%bn_radix(1000000). % Too high for multiplication on 32 bit system.
bn_radix(10000). % Low enough for multiplication on 32 bit system.

bigint(bigint(SignLength,Ds)) :-
	integer(SignLength),
	length(Ds,Len),
	Len =:= abs(SignLength),
	integers(Ds).

integers([D|Ds]) :-
	integer(D),
	integers(Ds).
integers([]).

bn_zero(bigint(0,[])).
bn_not_zero(bigint(SignLength,_)) :- SignLength =\= 0.
bn_unit(bigint(1,[1])).
bn_neg_unit(bigint(-1,[1])).

bn_positive(bigint(S,_)) :- S > 0.
bn_negative(bigint(S,_)) :- S < 0.

bn_negation(B0,B1) :- var(B0), !, bn_negation(B1,B0).
bn_negation(bigint(S0,Ns0),bigint(S1,Ns0)) :- S1 is -S0.

bn_eq(X,X).
bn_lt(bigint(S0,_),bigint(S1,_)) :-
	S0 < S1.
bn_lt(bigint(S0,Ns0),bigint(S1,Ns1)) :-
	S0 =:= S1,
	bn_lt_list(Ns0,Ns1).
bn_lt_list([X|Xs],[Y|Ys]) :-
	X < Y,
	bn_lt_list(Xs,Ys).
bn_lt_list([],[]).
bn_gt(X,Y) :- bn_lt(Y,X).
bn_le(X,Y) :- bn_eq(X,Y); bn_lt(X,Y).
bn_ge(X,Y) :- bn_eq(X,Y); bn_lt(Y,X).

bn_even(X) :- bn_zero(X).
bn_even(bigint(_,[X|_])) :- X mod 2 =:= 0.
bn_odd(bigint(_,[X|_])) :- X mod 2 =:= 1. % zero has empty list

% bn_abs(+Big,?AbsBig)
bn_abs(bigint(S0,Ns),bigint(S1,Ns)) :- S1 is abs(S0).

integer_bigint(N,bigint(Signlength,Ns)) :- nonvar(N), !,
	bn_radix(Radix),
	Value is abs(N),
	encode_bigint(Radix,Value,Ns,0,Len),
	Signlength is sign(N) * Len.
integer_bigint(N,bigint(S,Ns)) :- var(N), !,
	bn_radix(Radix),
	reverse(Ns,Ns1),
	decode_bigint(Radix, Ns1, 0, N0),
	N is N0 * sign(S).

decode_bigint(Radix, [X|Xs], N0, N) :-
	N1 is N0 * Radix + X,
	decode_bigint(Radix, Xs, N1, N).
decode_bigint(_Radix, [], N, N).

encode_bigint(Radix, Value, [X|Xs],Len0,Len) :- Value =\= 0, !,
	X is Value mod Radix,
	Value1 is floor(Value / Radix),
	Len1 is Len0 + 1,
	encode_bigint(Radix,Value1,Xs,Len1,Len).
encode_bigint(_Radix,0,[],Len,Len).

digit(1). digit(2). digit(3). digit(4). digit(5).
digit(6). digit(7). digit(8). digit(9). digit(0). 

% Green cut, because code_digit(C,0) or similar
% is only semi deterministic!? Apparently indexing
% only happens on the first term. Yikes!
code_digit(48,0) :- !. code_digit(49,1) :- !.
code_digit(50,2) :- !. code_digit(51,3) :- !.
code_digit(52,4) :- !. code_digit(53,5) :- !.
code_digit(54,6) :- !. code_digit(55,7) :- !.
code_digit(56,8) :- !. code_digit(57,9) :- !.

atom_digits(A,Digits) :-
	atom_codes(A,Codes),
	codes_digits(Codes,Digits).

codes_digits([Code|Cs],[Digit|Ds]) :-
	code_digit(Code,Digit),
	codes_digits(Cs,Ds).
codes_digits([],[]).

atom_sign_atom(Atom0,S,Atom1) :-
	atom_chars(Atom0,[Sign|Rest]),
    (   Sign = (-), !, % Parenthesis due to syntax: -/+ is an Prolog operator.
	S = -1, atom_chars(Atom1,Rest)
    ;
	Sign = (+), !,
	S = +1, atom_chars(Atom1,Rest)
    ;
	S = +1, Atom0 = Atom1
    ).

trim(E,[X|Xs],[X|Xs]) :- X =\= E, !.
trim(E,[E|Xs],Ys) :- !, trim(E,Xs,Ys). % Cut added to make det??!
trim(_E,[],[]).

trim_back(E,Xs,Ys) :-
	reverse(Xs,Xs1), trim(E,Xs1,Ys1), reverse(Ys1,Ys).

atom_bigint(A,bigint(Signlength,Ns)) :- nonvar(A), !,
	bn_radix(Radix),
	atom_sign_atom(A,Sign,A1),
	atom_digits(A1,Digits0),
	trim(0,Digits0,Digits1), % More fast and convenient to trim beforehand.
	reverse(Digits1,Digits), % From least to most significant
	encode_atom_bigint(Radix,Sign,1,Digits,0,Ns,0,Signlength).
atom_bigint(A,bigint(S,Ns)) :- var(A),
	bn_radix(Radix),
	DigitLength is ceiling(log(Radix)/log(10)),
	Sign is sign(S),
	decode_atom_bigint(Sign,DigitLength,Ns,[],A).

encode_atom_bigint(_Radix,Sign,_Pos,[],Carry,[],Len0,Len) :-
	Carry =:= 0, !,
	Len is Len0 * Sign. % Handles zero case: Len0 = 0.
encode_atom_bigint(_Radix,Sign,_Pos,[],Carry,[Carry],Len0,Len) :-
	Carry =\= 0, !,
	Len is (Len0 + 1) * Sign.
encode_atom_bigint(Radix,Sign,Pos,[C|Cs],Carry,[X|Xs],Len0,Len) :-
	digit(C), Pos * 10 > Radix, !,                      % New digit.
	N is Carry + Pos * C,
	X is N mod Radix,
	Carry1 is floor(N / Radix),
	Len1 is Len0 + 1,                                   % No zero value.
	% Note that we start anew with Pos = 10,
	% because Carry1 already is collected as Pos = 1.
	encode_atom_bigint(Radix,Sign,10,Cs,Carry1,Xs,Len1,Len).
encode_atom_bigint(Radix,Sign,Pos,[C|Cs],Carry,Xs,Len0,Len) :-
	digit(C), Pos1 is Pos * 10, Pos1 =< Radix, !,       % Add to digit.
	Carry1 is Carry + Pos * C, % Building up both digit and carry.
	encode_atom_bigint(Radix,Sign,Pos1,Cs,Carry1,Xs,Len0,Len).

%decode_atom_bigint(0,_Len,_Xs,_Cs,'0'). % Optimization.
decode_atom_bigint(Sign,Len,[X|Xs],Codes,A) :- var(A), !,
	decode_digit_codes(Len,X,Codes,Codes1),
	decode_atom_bigint(Sign,Len,Xs,Codes1,A).
decode_atom_bigint(1,_Len,[],Codes,A) :- var(A), !, % Not +1 (unification)
	char_code('0',Zero),
	trim(Zero,Codes,Codes1),
	atom_codes(A,Codes1).
decode_atom_bigint(-1,_Len,[],Codes,A) :- var(A), !,
	char_code('0',Zero),
	trim(Zero,Codes,Codes1),
	char_code(-,MinusCode),
	atom_codes(A,[MinusCode|Codes1]).
decode_atom_bigint(0,_Len,[],_Codes,'0').

%decode_atom_bigint(Sign,Len,[],Codes,A) :-
%	char_code('0',Zero),
%	trim(Zero,Codes,Codes1),
%    (   Sign =:= +1, atom_codes(A,Codes1)
%    ;   Sign =:= -1,
%	char_code(-,MinusCode),
%	atom_codes(A,[MinusCode|Codes1])
%    ;   Sign =:= 0, A = '0'
%    ).

decode_digit_codes(Len,Digit,Codes0,Codes1) :- Len > 0, !,
	DecimalDigit is Digit mod 10,
	Digit1 is floor(Digit / 10),
	code_digit(DecimalCode,DecimalDigit),
	Len1 is Len - 1,
	decode_digit_codes(Len1,Digit1,[DecimalCode|Codes0],Codes1).
decode_digit_codes(0,0,Codes,Codes).
	
% Partial sums with two variables are not implemented,
% but possible should the need arise.

% bn_add(?X,+Y,+Z): (Z + -Y) = X
% bn_add(+X,?Y,+Z): (-X + Z) = Y
% bn_add(+X,+Y,?Z):
% Clauses depending on the signs (and zero).
% The second final clause depends on
% the magnitude (numerical valule) of X and Y.
%      (0,?,?): (0 + Y) = Y = Z, optimization
%      (?,0,?): (X + 0) = X = Z, optimization
%      (-,-,-): -(-X + -Y) = Z
%      (-,+,?): (-X + Y) = (Y + -X) = Z
%      (+,-,?): negative case
%      (+,+,+): positive case
bn_add(X,Y,Z) :- var(X), nonvar(Y), nonvar(Z), !,
	bn_negation(Y,Y1),
	bn_add(Z,Y1,X).
bn_add(X,Y,Z) :- nonvar(X), var(Y), nonvar(Z), !,
	bn_negation(X,X1),
	bn_add(X1,Z,Y).
bn_add(X,Y,Y) :- nonvar(X), nonvar(Y), bn_zero(X), !. % Optimization.
bn_add(X,Y,X) :- nonvar(X), nonvar(Y), bn_zero(Y), !. % Optimization.
bn_add(X,Y,Z) :- nonvar(X), nonvar(Y),% var(Z),
	bn_negative(X), bn_negative(Y), !,
	bn_negation(X,X1), bn_negation(Y,Y1),
	bn_add(X1,Y1,Z1),
	bn_negation(Z1,Z).
bn_add(X,Y,Z) :- nonvar(X), nonvar(Y),% var(Z),
	bn_negative(X), bn_positive(Y), !,
	bn_add(Y,X,Z).
bn_add(X,Y,Z) :- nonvar(X), nonvar(Y), % Negative case, |X| < |Y|
	bn_positive(X), bn_negative(Y),
	bn_negation(Y,YAbs), bn_lt(X,YAbs), !,
	bn_radix(Radix),
	X = bigint(_XSign,Xs), Y = bigint(_YSign,Ys),
	pairwise_sub(Radix,Ys,Xs,0,Zs),
	trim_back(0,Zs,Zs1),
	length(Zs1,ZSign1),
	ZSign is ZSign1 * -1,
	Z = bigint(ZSign,Zs1).
bn_add(X,Y,Z) :- nonvar(X), nonvar(Y), % Negative case, |X| >= |Y|
	bn_positive(X), bn_negative(Y),
	bn_negation(Y,YAbs), bn_ge(X,YAbs), !,
	bn_radix(Radix),
	X = bigint(_XSign,Xs), Y = bigint(_YSign,Ys),
	pairwise_sub(Radix,Xs,Ys,0,Zs),
	trim_back(0,Zs,Zs1),
	length(Zs1,ZSign),
	Z = bigint(ZSign,Zs1).
bn_add(X,Y,Z) :- nonvar(X), nonvar(Y),% var(Z), Positive case.
	bn_positive(X), bn_positive(Y),
	bn_radix(Radix),
	X = bigint(_XSign,Xs), Y = bigint(_YSign,Ys),
	pairwise_add(Radix,Xs,Ys,0,Zs),
	length(Zs,ZSign),
	Z = bigint(ZSign,Zs).

bn_sub(X,Y,Z) :-
	bn_negation(Y,YNegated),
	bn_add(X,YNegated,Z).

% add_carry(+Radix,+X,+Y,+Carry,?Z,?Carry1)
add_carry(Radix,X,Y,Carry,Z,Carry1) :-
	Z0 is X + Y + Carry,
    (   Z0 < Radix ->
	Z is Z0,         Carry1 is 0
    ;   Z is Z0 - Radix, Carry1 is 1).

% sub_borrow(+Radix,+X,+Y,?Z,?Borrow1)
sub_borrow(Radix,X,Y,Borrow,Z,Borrow1) :-
	Z0 is X - Y - Borrow,
    (   Z0 < 0 ->
	Z is Z0 + Radix, Borrow1 is 1
    ;   Z is Z0,         Borrow1 is 0).

% Alternative name: bn_abs_add(...)
% pairwise_add(+Radix,+Xs,+Ys,+Carry,?Zs)
pairwise_add(Radix,[X|Xs],[Y|Ys],Carry,[Z|Zs]) :- !,
	add_carry(Radix,X,Y,Carry,Z,Carry1),
	pairwise_add(Radix,Xs,Ys,Carry1,Zs).
pairwise_add(Radix,[X|Xs],[],Carry,[Z|Zs]) :- !,
	add_carry(Radix,X,0,Carry,Z,Carry1),
	pairwise_add(Radix,Xs,[],Carry1,Zs).
pairwise_add(Radix,[],[Y|Ys],Carry,[Z|Zs]) :- !,
	add_carry(Radix,0,Y,Carry,Z,Carry1),
	pairwise_add(Radix,[],Ys,Carry1,Zs).
pairwise_add(_Radix,[],[],Carry,[Carry]) :- Carry =\= 0, !.
pairwise_add(_Radix,[],[],0,[]).

% Alternative name: bn_abs_sub(...)
% Only works for number lists, where X > Y.
% pairwise_sub(+Radix,+Xs,+Ys,+Borrow,?Zs)
pairwise_sub(Radix,[X|Xs],[Y|Ys],Borrow,[Z|Zs]) :- !,
	sub_borrow(Radix,X,Y,Borrow,Z,Borrow1),
	pairwise_sub(Radix,Xs,Ys,Borrow1,Zs).
pairwise_sub(Radix,[X|Xs],[],Borrow,[Z|Zs]) :- !,
	sub_borrow(Radix,X,0,Borrow,Z,Borrow1),
	pairwise_sub(Radix,Xs,[],Borrow1,Zs).
pairwise_sub(Radix,[],[Y|Ys],Borrow,[Z|Zs]) :- !,
	sub_borrow(Radix,0,Y,Borrow,Z,Borrow1),
	pairwise_sub(Radix,[],Ys,Borrow1,Zs).
pairwise_sub(_Radix,[],[],Borrow,[Borrow]) :- Borrow =\= 0, !,
	throw(pairwise_sub(first_argument_too_small)).
pairwise_sub(_Radix,[],[],0,[]).

% No partial multiplication relation,
% because it would yield incorrect results.

% bn_mul(+X,+Y,?Z)
bn_mul(X,_Y,X) :- bn_zero(X). % Optimization.
bn_mul(_X,Y,Y) :- bn_zero(Y). % Optimization.
bn_mul(X,Y,Z) :- bn_not_zero(X), bn_not_zero(Y),
	bn_radix(Radix),
	X = bigint(XSign,Xs), Y = bigint(YSign,Ys),
	pairwise_mul_tableau(Radix,Ys,Xs,Ys,0,[],[],[],Zs),
	length(Zs,Len),
	ZSign is Len * sign(XSign) * sign(YSign),
	Z = bigint(ZSign,Zs).

% Ys0: The original Ys, which we repeatedly multiply each X over.
% Ps: prefixes of 0's for each row in the tableau
% Ts: Each row in the tableau in reverse order,
%     stacking up multiplications.
%     TODO: Use difference list instead (1 extra parameter = 10).
% Zs0,Zs1: Tableau, running sum, ending with the result Zs1.
pairwise_mul_tableau(Radix,Ys0,[X|Xs],[Y|Ys],Carry,Ps,Ts,Zs0,Zs1) :- !,
	Digit is X * Y + Carry,
	Carry1 is floor(Digit / Radix),
	T is Digit mod Radix,
	pairwise_mul_tableau(Radix,Ys0,[X|Xs],Ys,Carry1,Ps,[T|Ts],Zs0,Zs1).
pairwise_mul_tableau(Radix,Ys0,[_|Xs],[],Carry,Ps,Ts,Zs0,Zs2) :- !,
	(Carry =\= 0 -> reverse([Carry|Ts],Ts1) ; reverse(Ts,Ts1)),
	pairwise_add(Radix,Ts1,Zs0,0,Zs1),
	pairwise_mul_tableau(Radix,Ys0,Xs,Ys0,0,[0|Ps],[0|Ps],Zs1,Zs2).
pairwise_mul_tableau(_Radix,_Ys0,[],_Ys,_Carry,_Ps,_Ts,Zs,Zs).

% TODO, context:
% Just finished bn_mul, and made things deterministic.
% Next make bn_div(X,Y,Z,R), where R is remainder.
% Then make bn_mod(X,Y,M), which is different from remainder.
% Then make bn_gcd, bn_expt, bn_sqrt, and maybe bn_rand.
% Then make bn_factorial, bn_rabin_miller_prime, bn_rho_pollard, bn_factors.

