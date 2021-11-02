:- module(draw_hex, [ draw_hexagon/1,
                      draw_hexagon/2,
                      draw_hexagon/3,
                      draw_hexagon/4]).
:- use_module(library(pce)).

draw_hexagon(H) :- draw_hexagon(0,0,1,H).
draw_hexagon(S, H) :- draw_hexagon(0,0,S,H).
draw_hexagon(X, Y, H) :- draw_hexagon(X,Y,1,H).
draw_hexagon(X, Y, S, H) :-
    new(H, path),
    send(H, append, point(X + -25*S, Y + -50*S)),
    send(H, append, point(X + -50*S, Y)),
    send(H, append, point(X + -25*S, Y + 50*S)),
    send(H, append, point(X + 25*S, Y + 50*S)),
    send(H, append, point(X + 50*S, Y)),
    send(H, append, point(X + 25*S, Y + -50*S)),
    send(H, append, point(X + -25*S, Y + -50*S)).
