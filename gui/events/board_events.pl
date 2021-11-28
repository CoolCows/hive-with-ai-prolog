:- module(board_events,[
        select_event/2
    ]).

:- use_module(library(pce)).
:- use_module("events_commons", [
    click_inside_hexagon/3,
    move_cell/1,
    position_cell/1
    ]).
:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).

select_event(Canvas, ClickPosition) :-
    write_ln('Selecting event'),
    (
        (not(nb_getval(position_cell, undefined)), write_ln('position_event'), position_cell(Canvas, ClickPosition));
        (not(nb_getval(move_cell, undefined)), write_ln('moving_cell'), move_cell(Canvas, ClickPosition));
        write_ln('selectting_position'),
        select_cell(Canvas, ClickPosition)
    ).

position_cell(Canvas, ClickPosition) :-
    nb_getval(board, Board),
    scan_board(Board, ClickPosition, CorrectCell),
    % if cell is not empty call select_cell instead
    % Send coordinates to logic
    % Logic returns a new board to draw
    % After, board is drawn
    write_ln('Correctly postioned'),
    position_cell(undefined).
    
move_cell(Canvas, ClickPosition) :-
    nb_getval(board, Board),
    scan_board(Board, ClickPosition, CorrectCell),
    % Anologous Papolodopus to the prevous func,
    % ... board is Drawn
    write_ln('Correctly moved'),
    move_cell(undefined).
    

select_cell(Canvas, ClickPosition) :-
   nb_getval(board, C),
   scan_board(C, ClickPosition, CorrectCell),
   % Check cell is not empty
   nb_getval(player_turn, Colour),
   get_color(CorrectCell,  Colour),
   write_ln('Can now move'),
   move_cell(CorrectCell).
   % Draw possible moves of cell

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
