:- module(board_graphics, [draw_board/2]).

:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2,
        %Delete init cell after testing
        init_cell/6
    ]).

draw_board(_, Canvas) :-
    send(Canvas, clear),
    get(Canvas, size, size(W, H)),
    CH is H/2, CW is W/2,
    nb_getval(scale, Scale),
    %Erase this funcall when logic is functional

    %_ is going to be the Board
    get_test_cells(Board),
    
    nb_setval(board, Board),
    nb_setval(center, point(CW, CH)),

    %Function that gets the screen painting
    draw_cells(Board, Canvas, point(CW, CH), Scale).

draw_cells([], _, _).
draw_cells([Cell|Rest], Canvas, point(CW, CH), Scale) :-
    get_col(Cell, Col), 
    get_row(Cell, Row),
    X is CW + Scale*75*Col,
    Y is CH + Scale*100*Row + Scale*50*(Col mod 2),
    get_hexagon(X, Y, Scale, Hexagon),
    %T0d0: Modify the hexagon so it represents correctly the bug
    send(Canvas, display, Hexagon),
    draw_cells(Rest, Canvas, point(CW, CH), Scale). 
    
get_hexagon(X, Y, H) :- draw_hexagon(X,Y,1,H).
get_hexagon(X, Y, S, H) :-
    new(H, path),
    send(H, append, point(X + -25*S, Y + -50*S)),
    send(H, append, point(X + -50*S, Y)),
    send(H, append, point(X + -25*S, Y + 50*S)),
    send(H, append, point(X + 25*S, Y + 50*S)),
    send(H, append, point(X + 50*S, Y)),
    send(H, append, point(X + 25*S, Y + -50*S)),
    send(H, append, point(X + -25*S, Y + -50*S)).

% Erase method when complete
get_test_cells(C) :-
    init_cell(bug, 0, 0, black, 0, C1),
    init_cell(bug, 0, 1, black, 0, C2),
    init_cell(bug, 0, 2, white, 0, C3),
    init_cell(bug, 0, 3, white, 0, C4),
    init_cell(bug, 1, 0, white, 0, C5),
    init_cell(bug, 1, 1, white, 0, C6),
    init_cell(bug, 1, 2, white, 0, C7),
    init_cell(bug, 1, 3, white, 0, C8),
    init_cell(bug, 0, -1, white, 0, C9),
    init_cell(bug, 0, -2, white, 0, C10),
    init_cell(bug, 0, -3, white, 0, C11),
    init_cell(bug, -1, 0, white, 0, C12),
    init_cell(bug, 1, -1, white, 0, C13),
    init_cell(bug, 1, -2, white, 0, C14),
    init_cell(bug, 1, -3, white, 0, C15),
    append(C, [], [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]).


