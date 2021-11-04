:- module(tools, [max/3,
                  min/3,
                  slope/5,
                  trace/4,
                  below_line/4,
                  above_line/4]).

max(X, Y, R) :-
    (X > Y,!, R = X);
     R is Y.
min(X, Y, R) :-
    (X < Y,!, R = X);
     R is Y.

slope(X2, Y2, X1, Y1, M) :-
    M is (X2 - X1)/(Y2 - Y1).

%Y - mX - n = 0
trace(X, Y, M, N) :-
    N is Y - M*X.

line(Point2, Point1, M, N):-
    get(Point2, x, X2), get(Point2, y, Y2),
    get(Point1, x, X1), get(Point1, y, Y1),
    line(X2, Y2, X1, Y1, M, N).

line(X2, Y2, X1, Y1, M, N) :-
    slope(X2, Y2, X1, Y1, M),
    trace(X2, Y2, M, N).

below_line(Point, M, N) :-
    get(Point, x, X), get(Point, y, Y),
    below_line(X, Y, M, N).

below_line(X, Y, M, N) :-
    R is Y - M*X - N,!,
    R =< 0.

above_line(Point, M, N) :-
    get(Point, x, X), get(Point, y, Y),
    above_line(X, Y, M, N).

above_line(X, Y, M, N) :-
    R is Y - M*X - N,!,
    R >= 0.
