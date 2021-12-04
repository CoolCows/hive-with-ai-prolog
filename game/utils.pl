:-module(utils,[ push/3,
				 pop/2,
				 len/2,
				 any/2,
				 is_odd/1,
				 is_even/1
				 ]).
:- module_transparent(any/2).


push(X,Y,[X|Y]).

pop([_|T],T).

len([], LenResult):-
    LenResult is 0.

len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

any([X|Y], C) :- 
	append(C,[X],D),
	T=..D,
	T,
	!;
	any(Y,C).

is_odd(X):-
	1 is X mod 2.

is_even(X):-
	0 is X mod 2.

