:- module(events_commons, [click_inside_hexagon/3]).

:- use_module("../tools/geometry", [
        below_line/3,
        above_line/3,
        line/4
    ]).


click_inside_hexagon(point(ClickX, ClickY), point(X, Y), Dist) :-
    % Check click position is between the hexagon up and bottom frontiers
    ClickY < Y + Dist,
    ClickY > Y - Dist,
    % Check cp is between the diags
    click_line(below, point(ClickX, ClickY), X + Dist/2,  Y + Dist,  X + Dist, Y),
    click_line(below, point(ClickX, ClickY), X - Dist/2, Y + Dist, X - Dist, Y),
    click_line(above, point(ClickX, ClickY), X + Dist, Y, X + Dist/2, Y - Dist),
    click_line(above, point(ClickX, ClickY), X - Dist, Y, X - Dist/2, Y - Dist).

click_line(T, point(ClickX, ClickY), X1val, Y1val, X2val, Y2val) :-
    X1init is X1val, Y1init is Y1val,
    X2init is X2val, Y2init is Y2val,
    line(point(X1init, Y1init), point(X2init, Y2init), M, N),
    ((T=below, below_line(point(ClickX, ClickY), M, N));
    (T=above, above_line(point(ClickX, ClickY), M, N))).
