:- module(board_events,[
        select_event/2
    ]).

:- use_module(library(pce)).
:- use_module("events_commons", [click_inside_hexagon/3]).
:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).

select_event(Canvas, ClickPosition) :-
    nb_getval(position_cell, Position),
    nb_getval(move_cell, Move),
    (
        (Position, position_cell(Canvas, ClickPosition));
        (Move, move_cell(Canvas, ClickPosition));
        select_cell(Canvas, ClickPosition)
    ).

position_cell(Canvas, ClickPosition) :-
    true.

move_cell(Canvas, ClickPosition) :-
    true.

select_cell(Canvas, ClickPosition) :-
   nb_getval(board, C),
   scan_board(C, ClickPosition, CorrectCell),
   %T0D0: Check Player Turn
   %T0D0: Send logic selected cell%
   %T0D0: Draw something pretty%
   write_ln(CorrectCell).

scan_board([Cell|Rest], ClickPosition, CorrectCell) :-
    click_inside(Cell, ClickPosition, CorrectCell);
    scan_board(Rest, ClickPosition, CorrectCell).

click_inside(Cell, ClickPosition, point(Col, Row)) :- 
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
