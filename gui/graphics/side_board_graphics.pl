:- module(side_board_graphics, [draw_side_board/3]).

:- use_module(library(pce)).
:- use_module(commons, [get_hexagon/4]).
:- use_module("../../game/player", [
    get_queens/2,
    get_ants/2,
    get_beetles/2,
    get_grasshoppers/2,
    get_ladybugs/2,
    get_mosquitos/2,
    get_pillbugs/2,
    get_spiders/2
]).

draw_side_board(_, Colour, Canvas) :-
    write_ln('Drawing side board'),
    send(Canvas, clear),
    color_side_board(Colour, Canvas),
    
    % Erase after testing
    % Player should be recieved as an arg
    % from _
    init_player(Player),

    % Saving the player, this logic is incorrectly put here
    (
        (Colour = white, nb_setval(white_player, player(0, 0, 0, 0, 1, 1, 1, 0)));
        (Colour = black, nb_setval(black_player, player(0, 0, 0, 0, 1, 1, 1, 0)))
    ),
    
    draw_remaining_cells(
        Canvas,
        player(0, 0, 0, 0, 1, 1, 1, 0),
        %Player,
        Colour,
        0,
        [
            get_queens, get_ants, get_beetles,
            get_grasshoppers, get_ladybugs,
            get_mosquitos, get_pillbugs, get_spiders
        ],
        [
           queen, ant, beetle, grasshopper, ladybug,
           mosquito, pillbug, spyder
        ]
    ).

color_side_board(Colour, Canvas) :-
    (Colour = white, send(Canvas, background, colour(lightblue)));
    (Colour = black, send(Canvas, background, colour(darkblue))).

draw_remaining_cells( _, _, _, _, [], []).
draw_remaining_cells(Canvas, Player, Colour, Index, [Getter|FuncRest], [Type|TypeRest]) :-
    apply(Getter, [Player, Count]),
    (
        (
            Count =:= 0, 
            draw_remaining_cells(Canvas, Player, Colour, Index, FuncRest, TypeRest),!
        );
        (
            S is 0.75, % Current hexagon Scale
            ModIndex is Index mod 4,
            Row is Index // 4,
            X is ModIndex*60*S + ModIndex*65*S + 50,
            Y is 60*S + 110*S*Row,
            color_cell(Canvas, Colour, Count, Type, X, Y),
            draw_remaining_cells(Canvas, Player, Colour, Index + 1, FuncRest, TypeRest),!
        )
    ).
    
color_cell(Canvas, Colour, Count, Type, X, Y) :-
    S is 0.75,
    get_hexagon(X, Y, S, H),
    new(Text, text(new(_, string('x%s', Count)))),
    send(Text, font, font(helvetica, arial, 20)),
    (
        (Colour = white, send(H, fill_pattern, colour(white)), send(H, colour, colour(lightgray)));
        (Colour = black, send(H, fill_pattern, colour(black)))
    ),
    send(Text, colour, colour(orange)),
    % Display in Canvas
    send(Canvas, display, H),
    send(Canvas, display, Text, point(X + 30, Y + 10)),
    draw_bug(Type, X, Y, Canvas).

draw_bug(Bug, X, Y, Canvas) :-
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
    send(Canvas, display, BM, point(X - 35, Y - 35)).

