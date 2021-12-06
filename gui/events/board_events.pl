:- module(board_events,[
        select_event/4
    ]).

:- use_module(library(pce)).

:- use_module("events_commons", [
    click_inside_hexagon/3,
    move_cell/1,
    position_cell/1
    ]).

:- use_module("../graphics/board_graphics", [
    draw_board/2,
    draw_selected_cell/2
]).

:- use_module("../graphics/side_board_graphics", [
    draw_side_board/3
]).

:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).

:- use_module("../../game/gui_api", [
    gui_put_cell/3,
    gui_move_cell/3,
    gui_get_possible_moves/2,
    gui_get_board/1,
    gui_mosquito_adyacent_pillbug/1
]).

select_event(Canvas, ClickPosition, WhiteCanvas, BlackCanvas) :-
    nb_getval(board, Board),
    get_correct_cells(Board, ClickPosition, CorrectCell),
    (
        (
            (
                not(nb_getval(pillbug_effect, undefined)),
                nb_getval(pillbug_effect, [MovableBugs, PosPositions]),
                member(CorrectCell, MovableBugs),
                select_movable_bug(Canvas, CorrectCell, [MovableBugs, PosPositions])
            );
            (
                not(get_bug_type(CorrectCell, none)),
                nb_getval(player_turn, Colour),
                get_color(CorrectCell, Colour), 
                write_ln('selecting_cell'),
                select_cell(Canvas, CorrectCell)
            )
        );
        (
            not(nb_getval(position_cell, undefined)), 
            write_ln('position_event'), 
            position_cell(Canvas, CorrectCell, WhiteCanvas, BlackCanvas)
        );
        (
            not(nb_getval(move_cell, undefined)), 
            write_ln('moving_cell'), 
            move_cell(Canvas, CorrectCell)
        )
    ).

position_cell(
    Canvas,
    cell(none, Row, Col, none, Stack),
    WhiteCanvas,
    BlackCanvas
) :-
    nb_getval(player_turn, Colour),
    nb_getval(position_cell, BugType),
    gui_put_cell(
        +cell(BugType, Row, Col, Colour, Stack),
        -NewBoard,
        -NewPlayer
    ),
    (
        (Colour = white, SideCanvas = WhiteCanvas);
        (Colour = black, SideCanvas = BlackCanvas)
    ),
    change_turn(NewBoard, NewPlayer, Canvas, SideCanvas),
    write_ln('Correctly postioned').
    
move_cell(Canvas, cell(none, Row, Col, none, Stack)) :-
    nb_getval(move_cell, SourceCell),
    SourceCell = cell(BugType, _, _, Colour, _),
    gui_move_cell(
        +SourceCell,
        +cell(BugType, Row, Col, Colour, Stack),
        -NewBoard
    ),
    change_turn(NewBoard, Canvas).
    

select_cell(Canvas, CorrectCell) :-
    gui_get_possible_moves(+CorrectCell, -NewBoard),
    nb_setval(board, NewBoard),
    move_cell(CorrectCell),
    draw_board(NewBoard, Canvas),
    CorrectCell = cell(BugType, Row, Col, _, StackPos),
    (
        (
            BugType = pillbug,
            handle_pillbug_effect(Canvas, CorrectCell)
        );
        (
            BugType = mosquito,
            handle_mosquito_effect(Canvas, CorrectCell)
        );
        nb_setval(pillbug_effect, undefined)
    ),
    draw_selected_cell(cell(none, Row, Col, show, StackPos), Canvas).

select_movable_bug(Canvas, CorrectCell, [MovBugs, MovPositions]) :-
    move_cell(CorrectCell),
    gui_get_board(-Board),
    append(MovPositions, Board, NewBoard),
    nb_setval(board, NewBoard),
    write_ln(NewBoard),
    draw_board(NewBoard, Canvas),
    findall(cell(none, Row, Col, pillbug, 0), member(cell(_, Row, Col, _, _), MovBugs), ShowMovBugs),
    CorrectCell = cell(_, Row, Col, _, 0),
    draw_all(Canvas, ShowMovBugs),
    draw_selected_cell(cell(none, Row, Col, show, 1), Canvas),
    true.

handle_pillbug_effect(Canvas, CorrectCell) :-
    write_ln('Handling pillbug effect'),
    gui_get_pillbug_effect(+CorrectCell, -[MovBugs, PosBugs]),
    nb_setval(pillbug_effect, [MovBugs, PosBugs]),
    findall(cell(none, Row, Col, pillbug, 5), member(cell(_, Row, Col, _, _), MovBugs), ShowMovBugs),
    write_ln(ShowMovBugs),
    draw_all(Canvas, ShowMovBugs).

handle_mosquito_effect(Canvas, CorrectCell) :- 
    write_ln('Handling mosquito effect'),
    gui_mosquito_adyacent_pillbug(+CorrectCell),
    handle_pillbug_effect(Canvas, CorrectCell).

draw_all(_, []).
draw_all(Canvas, [X|Rest]) :-
    draw_selected_cell(X, Canvas),
    draw_all(Canvas, Rest).

get_correct_cells(CellList, ClickPosition, CorrectCell):-
    bagof(Cell, scan_board(CellList, ClickPosition, Cell), CorrectCells),
    get_top_cell(CorrectCells, CorrectCell).
    
get_top_cell([X], X).
get_top_cell([X, Y|Rest], TopCell) :-
    get_stack_pos(X, StackPosX),
    get_stack_pos(Y, StackPosY),
    (
        StackPosX > StackPosY, !,
        get_top_cell([X|Rest], TopCell)
    );
    get_top_cell([Y|Rest], TopCell).
    

change_turn(Board, Canvas) :- change_turn(Board, _, Canvas, _).
change_turn(Board, Player, Canvas, SideCanvas) :-
    nb_setval(board, Board),
    nb_setval(pillbug_effect, undefined),
    nb_setval(move_cell, undefined),
    nb_setval(position_cell, undefined),

    nb_getval(player_turn, Colour),
    (
        (   
            Colour = white,
            nb_setval(player_turn, black),
            not(var(Player)),
            nb_setval(white_player, Player),
            draw_side_board(Player, Colour, SideCanvas)
        );
        (
            Colour = black, 
            nb_setval(player_turn, white),
            not(var(Player)),
            nb_setval(black_player, Player),
            draw_side_board(Player, Colour, SideCanvas)
        );
        true
    ),
    draw_board(Board, Canvas).

scan_board([Cell|Rest], ClickPosition, CorrectCell) :-
    click_inside(Cell, ClickPosition, CorrectCell);
    scan_board(Rest, ClickPosition, CorrectCell).

click_inside(Cell, ClickPosition, Cell) :- 
    get_col(Cell, Col),
    get_row(Cell, Row),
    get(ClickPosition, y, ClickY),
    get(ClickPosition, x, ClickX),
    nb_getval(scale, Scale),
    nb_getval(center, point(CW, CH)),
    X is CW + Scale*75*Col,
    Y is CH + Scale*100*Row + Scale*50*(Col mod 2),
    Dist is 50*Scale,
    click_inside_hexagon(point(ClickX, ClickY), point(X, Y), Dist).
