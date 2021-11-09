:- module(geometry, [
    line/4,
    below_line/3,
    above_line/3
]).
:- use_module(library(pce)).

%Y -mX - n = 0
line(point(X1, Y1), point(X2, Y2), M, N):-
    slope(X2, Y2, X1, Y1, M),
    trace(X2, Y2, M, N).

slope(X2, Y2, X1, Y1, M) :-
    M is (Y2 - Y1)/(X2 - X1).

trace(X, Y, M, N) :-
    N is Y - M*X.

below_line(point(X, Y), M, N) :-
    write_ln(format('below_line M',M,'N',N,' point x',X,'y:',Y)),
    R is Y - M*X - N,!,
    write_ln(format('Result ', R)),
    R =< 0.

above_line(point(X, Y), M, N) :-
    write_ln(format('above_line point X',X,'y:',Y)),
    R is Y - M*X - N,!,
    write_ln(format('Result ', R)),
    R >= 0.
