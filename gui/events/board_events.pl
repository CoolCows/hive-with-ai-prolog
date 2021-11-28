:- module(board_events,[
        select_event/2
    ]).

:- use_module(library(pce)).

:- use_module("events_commons", [
    click_inside_hexagon/3,
    move_cell/1,
    position_cell/1
    ]).

:- use_module("../graphics/board_graphics", [
    draw_board/2
]).

:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).

:- use_module("../../game/gui_api", [
    gui_put_cell/2,
    gui_move_cell/2,
    gui_get_possible_moves/2
]).

select_event(Canvas, ClickPosition) :-
    nb_getval(board, Board),
    get_correct_cells(Board, ClickPosition, CorrectCell),
    nb_getval(player_turn, Colour),
    get_color(CorrectCell, Colour), 
    (
        not(get_bug_type(CorrectCell, none)),
        write_ln('selecting_cell'),
        select_cell(Canvas, CorrectCell)
    );
    (
        not(nb_getval(position_cell, undefined)), 
        write_ln('position_event'), 
        position_cell(Canvas, CorrectCell)
    );
    (
        not(nb_getval(move_cell, undefined)), 
        write_ln('moving_cell'), 
        move_cell(Canvas, CorrectCell)
    ).

position_cell(Canvas, CorrectCell) :-
    get_color(CorrectCell, Colour),
    gui_put_cell(+Colour, -NewBoard),
    draw_board(NewBoard, Canvas),
    nb_setval(board, NewBoard),
    position_cell(undefined),
    write_ln('Correctly postioned').
    
move_cell(Canvas, CorrectCell) :-
    gui_move_cell(+CorrectCell, -NewBoard),
    draw_board(NewBoard, Canvas),
    nb_setval(board, NewBoard),
    move_cell(undefined),
    write_ln('Correctly moved').
    

select_cell(Canvas, CorrectCell) :-
    gui_get_possible_moves(+CorrectCell, -NewBoard),
    draw_board(NewBoard, Canvas),
    nb_setval(board, NewBoard),
    write_ln('Can now move'),
    move_cell(CorrectCell).

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
