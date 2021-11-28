 :- module(side_board_events, [select_side_board/3]).

:- use_module("events_commons", [
    click_inside_hexagon/3,
    position_cell/1
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

select_side_board(Canvas, ClickPosition, Colour) :-
    (
        (Colour = white, nb_getval(white_player, Player));
        (Colour = black, nb_getval(black_player, Player))
    ),
    nb_getval(player_turn, Colour),
    select_remaining_cell(
        Canvas,
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
        SelectedCell
    ),
    position_cell(SelectedCell),
    % Ask logic for positioning locations
    % Recieve something of possible locations
    % Paint those possible locations
    write_ln('Can now position'),
    write_ln(SelectedCell).

select_remaining_cell(
    Canvas,
    ClickPosition, 
    Player, 
    Index, 
    [Getter|FuncRest], 
    [Type|TypeRest], 
    SelectedCell
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
                SelectedCell
            )
        );
        (
            (
                click_inside(ClickPosition, Index),
                SelectedCell = Type
            );
            select_remaining_cell(
                Canvas,
                ClickPosition,
                Player,
                Index + 1,
                FuncRest,
                TypeRest,
                SelectedCell
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
