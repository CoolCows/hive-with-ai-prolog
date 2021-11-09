:- module(board_evens,[
        select_cell/2
    ]).

:- use_module(library(pce)).
:- use_module("../tools/geometry", [
        below_line/3,
        above_line/3,
        line/4
    ]).
:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).

select_cell(Canvas, ClickPosition) :-
   nb_getval(board, C),
   scan_board(C, ClickPosition, CorrectCell),
   %T0D0: Send logic selected cell%
   %T0D0: Draw something pretty%
   write_ln(CorrectCell).

scan_board([Cell|Rest], ClickPosition, CorrectCell) :-
    click_inside(Cell, ClickPosition, CorrectCell);
    scan_board(Rest, ClickPosition, CorrectCell).

log_click_simple(Val1, Val2, ClickY) :-
    write_ln('Click log simple:'),
    Init1 is Val1,
    Init2 is Val2,
    write_ln('Val 1 is'),
    write_ln(Init1),
    write_ln('Val 2 is'),
    write_ln(Init2),
    write_ln('Click Y is'),
    write_ln(ClickY).

click_inside(Cell, ClickPosition, point(Col, Row)) :- 
    get_col(Cell, Col),
    get_row(Cell, Row),
    get(ClickPosition, y, ClickY),
    get(ClickPosition, x, ClickX),
    nb_getval(scale, Scale),
    nb_getval(center, point(CW, CH)),
    X is CW + Scale*75*Col,
    Y is CH + Scale*100*Row + Scale*50*(Col mod 2),
    % Check click position is between the hexagon up and bottom frontiers
    write_ln('Easy Checking'),
    %write_ln(format('CW is', CW, 'CH is', CH)),
    %log_click_simple(Y + 50*Scale, Y - 50*Scale, ClickY),
    Dist is 50*Scale,
    ClickY < Y + Dist,
    ClickY > Y - Dist,
    % Check cp is between the diags
    write_ln('Hard Checking'),
    click_line(below, point(ClickX, ClickY), X + Dist/2,  Y + Dist,  X + Dist, Y),write_ln('Hard Checking1'),
    click_line(below, point(ClickX, ClickY), X - Dist/2, Y + Dist, X - Dist, Y),write_ln('Hard Checking2'),
    click_line(above, point(ClickX, ClickY), X + Dist, Y, X + Dist/2, Y - Dist),write_ln('Hard Checking3'),
    click_line(above, point(ClickX, ClickY), X - Dist, Y, X - Dist/2, Y - Dist),write_ln('Hard Checking4').


type(above).
type(below).

click_line(T, point(ClickX, ClickY), X1val, Y1val, X2val, Y2val) :-
    X1init is X1val, Y1init is Y1val,
    X2init is X2val, Y2init is Y2val,
    write_ln(format('x1',X1init, 'y1',Y1init)),
    write_ln(format('x2',X2init, 'y2',Y2init)),
    line(point(X1init, Y1init), point(X2init, Y2init), M, N),
    ((T=below, below_line(point(ClickX, ClickY), M, N));
    (T=above, above_line(point(ClickX, ClickY), M, N))).
