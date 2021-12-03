:- module(side_board_graphics, [draw_side_board/3, color_selected_cell/4]).

:- use_module(library(pce)).
:- use_module(commons).
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

draw_side_board(Player, Colour, Canvas) :-
    write_ln('Drawing side board'),
    send(Canvas, clear),
    color_side_board(Colour, Canvas),
    
    draw_remaining_cells(
        Canvas,
        Player,
        Colour,
        0,
        [
            get_queens, get_ants, get_beetles,
            get_grasshoppers, get_ladybugs,
            get_mosquitos, get_pillbugs, get_spiders
        ],
        [
           queen, ant, beetle, grasshopper, ladybug,
           mosquito, pillbug, spider
        ]
    ).

color_side_board(Colour, Canvas) :-
    (Colour = white, send(Canvas, background, colour(lightblue)));
    (Colour = black, send(Canvas, background, colour(darkblue))).

color_selected_cell(Index, Player, Colour, Canvas) :-
    draw_side_board(Player, Colour, Canvas),
    color_cell(Canvas, show, " ", none, Index).

draw_remaining_cells( _, _, _, _, [], []).
draw_remaining_cells(Canvas, Player, Colour, Index, [Getter|FuncRest], [Type|TypeRest]) :-
    apply(Getter, [Player, Count]),
    (
        (
            Count =:= 0, 
            draw_remaining_cells(Canvas, Player, Colour, Index, FuncRest, TypeRest),!
        );
        (
            color_cell(Canvas, Colour, Count, Type, Index),
            draw_remaining_cells(Canvas, Player, Colour, Index + 1, FuncRest, TypeRest),!
        )
    ).
    
color_cell(Canvas, Colour, Count, Type, Index) :-
    S is 0.75,
    ModIndex is Index mod 4,
    Row is Index // 4,
    X is ModIndex*60*S + ModIndex*65*S + 50,
    Y is 60*S + 110*S*Row,

    get_hexagon(X, Y, S, H),
    new(Text, text(new(_, string('x%s', Count)))),
    send(Text, font, font(helvetica, arial, 20)),
    (
        (Colour = white, send(H, fill_pattern, colour(white)), send(H, colour, colour(lightgray)));
        (Colour = black, send(H, fill_pattern, colour(black)));
        (Colour = show, send(H, colour, colour(green)), send(H, pen, 5))
    ),
    send(Text, colour, colour(orange)),
    % Display in Canvas
    send(Canvas, display, H),
    send(Canvas, display, Text, point(X + 30, Y + 10)),
    draw_bug(Type, X, Y, Canvas).


