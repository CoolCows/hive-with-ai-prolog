:- use_module(misc, [
    max/3,
    min/3
]).

max(X, Y, R) :-
    (X > Y,!, R = X);
     R is Y.
min(X, Y, R) :-
    (X < Y,!, R = X);
     R is Y.

