:-module(utils,[ push/3,
				 pop/2,
				 len/2,
				 any/2
				 ]).
:- module_transparent(any/2).
% :- meta_predicate
% 			any(0,0).


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
