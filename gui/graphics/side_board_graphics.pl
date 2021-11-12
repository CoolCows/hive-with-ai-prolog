:- module(side_board_graphics, [draw_side_board/2]).

:- use_module(commons, [get_hexagon/4]).
:- use_module("../../game/player", [
    get_queens/2,
    get_ants/2,
    get_beetles/2,
    get_grasshoppers/2,
    get_ladybugs/2,
    get_mosquitos/2,
    get_pillbugs/2,
    get_spiders/2,
    % Erase after functioning logic
    init_player/1
]).

draw_side_board(_, Canvas) :-
    write_ln('Drawing side board'),
    send(Canvas, clear),
    % Erase after testing
    init_player(Player),
    draw_remaining_cells(
        Canvas,
        Player, 
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
    ),
    true.

draw_remaining_cells(_, _, _, [], []).
draw_remaining_cells(Canvas, Player, Index, [Getter|FuncRest], [Type|TypeRest]) :-
    write_ln('Drawing Remaining Cells'),
    S is 0.75, % Current hexagon Scale
    ModIndex is Index mod 4,
    Row is Index // 4,
    get_hexagon(ModIndex*60*S + ModIndex*50*S + 100, 60*S + 110*S*Row, S, H),
    apply(Getter, [Player, Count]),
    send(Canvas, display, H),
    draw_remaining_cells(Canvas, Player, Index + 1, FuncRest, TypeRest).
    

