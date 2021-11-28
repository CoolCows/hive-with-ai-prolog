:- module(commons, [
    get_hexagon/4,
    draw_bug/4
]).

get_hexagon(X, Y, S, H) :-
    new(H, path),
    send(H, append, point(X + -25*S, Y + -50*S)),
    send(H, append, point(X + -50*S, Y)),
    send(H, append, point(X + -25*S, Y + 50*S)),
    send(H, append, point(X + 25*S, Y + 50*S)),
    send(H, append, point(X + 50*S, Y)),
    send(H, append, point(X + 25*S, Y + -50*S)),
    send(H, append, point(X + -25*S, Y + -50*S)).

draw_bug(Bug, X, Y, Canvas) :-
    Bug = none;
    (
        (
            (Bug = queen, new(BM, bitmap( './graphics/xpm/queen.xpm')));
            (Bug = ant, new(BM, bitmap('./graphics/xpm/ant.xpm')));
            (Bug = beetle, new(BM, bitmap('./graphics/xpm/beetle.xpm')));
            (Bug = grasshopper, new(BM, bitmap('./graphics/xpm/grasshopper.xpm')));
            (Bug = spyder, new(BM, bitmap('./graphics/xpm/spyder.xpm')));
            (Bug = pillbug, new(BM, bitmap('./graphics/xpm/pillbug.xpm')));
            (Bug = mosquito, new(BM, bitmap('./graphics/xpm/mosquito.xpm')));
            (Bug = ladybug, new(BM, bitmap('./graphics/xpm/ladybug.xpm')))
        ),
        send(Canvas, display, BM, point(X - 35, Y - 35))
    ).

