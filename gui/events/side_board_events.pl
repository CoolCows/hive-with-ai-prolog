:- module(side_board_events, [select_side_board/4]).

:- use_module("events_commons", [
    click_inside_hexagon/3,
    position_cell/1
]).

:- use_module("../graphics/board_graphics", [
    draw_board/2
]).

:- use_module("../graphics/side_board_graphics",[
    color_selected_cell/4
]).

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

:- use_module("../../game/gui_api", [
    gui_get_possible_positions/2
]).

select_side_board(MainCanvas, SideCanvas, ClickPosition, Colour) :-
    (
        (Colour = white, nb_getval(white_player, Player));
        (Colour = black, nb_getval(black_player, Player))
    ),
    nb_getval(player_turn, Colour),
    select_remaining_cell(
        SideCanvas,
        ClickPosition,
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
        ],
        position(BugType, Index)
    ),
    position_cell(BugType),
    gui_get_possible_positions(+Colour, -NewBoard),
    color_selected_cell(Index, Player, Colour, SideCanvas),
    draw_board(NewBoard, MainCanvas),
    nb_setval(board, NewBoard).

select_remaining_cell(
    Canvas,
    ClickPosition, 
    Player, 
    Index, 
    [Getter|FuncRest], 
    [Type|TypeRest], 
    PositionCell
):-
    apply(Getter, [Player, Count]),
    (
        (
            Count =:= 0,!,
            select_remaining_cell(
                Canvas,
                ClickPosition,
                Player,
                Index,
                FuncRest,
                TypeRest,
                PositionCell
            )
        );
        (
            (
                click_inside(ClickPosition, Index),
                PositionCell = position(Type, Index)
            );
            select_remaining_cell(
                Canvas,
                ClickPosition,
                Player,
                Index + 1,
                FuncRest,
                TypeRest,
                PositionCell
            )
        )
    ).
                

click_inside(ClickPosition, Index) :-
    S is 0.75,
    get(ClickPosition, x, ClickX),
    get(ClickPosition, y, ClickY),
    ModIndex is Index mod 4,
    Row is Index // 4,
    X is ModIndex*60*S + ModIndex*75*S + 50,
    Y is 60*S + 110*S*Row,
    Dist is 50*S,
    click_inside_hexagon(point(ClickX, ClickY), point(X, Y), Dist).
