:- module(commons, [get_hexagon/3, get_hexagon/4]).

get_hexagon(X, Y, H) :- draw_hexagon(X,Y,1,H).
get_hexagon(X, Y, S, H) :-
    new(H, path),
    send(H, append, point(X + -25*S, Y + -50*S)),
    send(H, append, point(X + -50*S, Y)),
    send(H, append, point(X + -25*S, Y + 50*S)),
    send(H, append, point(X + 25*S, Y + 50*S)),
    send(H, append, point(X + 50*S, Y)),
    send(H, append, point(X + 25*S, Y + -50*S)),
    send(H, append, point(X + -25*S, Y + -50*S)).


