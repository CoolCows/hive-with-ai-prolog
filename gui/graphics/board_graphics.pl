:- module(board_graphics, [
    draw_board/2,
    draw_selected_cell/2,
    refresh/1
]).

:- use_module(commons).
:- use_module("../../game/cell", [
        get_bug_type/2,
        get_row/2,
        get_col/2,
        get_color/2,
        get_stack_pos/2
    ]).
:- use_module("../../game/gui_api", [gui_test_board/1]).

draw_board(Board, Canvas) :-
    write_ln('draw_board'),   
    send(Canvas, clear),
    send(Canvas, background, colour(brown)),
    
    nb_getval(scale, Scale),
    nb_getval(center, point(CW, CH)),

    quick_sort(Board, SortedBoard),
    draw_cells(SortedBoard, Canvas, point(CW, CH), Scale).

draw_selected_cell(Cell, Canvas) :-
    nb_getval(scale, Scale),
    nb_getval(center, Center),
    draw_cells([Cell], Canvas, Center, Scale).

% Sort according to stack position
draw_cells([], _, _, _).
draw_cells([Cell|Rest], Canvas, point(CW, CH), Scale) :-
    get_col(Cell, Col), 
    get_row(Cell, Row),
    X is CW + Scale*75*Col,
    Y is CH + Scale*100*Row + Scale*50*(Col mod 2),
    get_hexagon(X, Y, Scale, Hexagon),
    set_hexagon_color(Cell, Hexagon),
    send(Canvas, display, Hexagon),
    get_bug_type(Cell, Bug),
    draw_bug(Bug, X, Y, Canvas),
    draw_cells(Rest, Canvas, point(CW, CH), Scale).

set_hexagon_color(Cell, Hex) :-
    get_color(Cell, Color),
    (
        (Color = white, send(Hex, colour, colour(lightgray)), send(Hex, fill_pattern, colour(white)));
        (Color = black, send(Hex, colour, colour(darkgray)), send(Hex, fill_pattern, colour(black)));
        (Color = none, send(Hex, colour, colour(yellow)), send(Hex, pen, 3));
        (Color = show, send(Hex, colour, colour(green)), send(Hex, pen, 5));
        (Color = pillbug, send(Hex, colour, colour(magenta)), send(Hex, pen, 3))
    ).
    
refresh(Canvas) :- 
    get(Canvas, size, size(W, H)),
    CH is H/2, CW is W/2,
    nb_setval(center, point(CW, CH)),
    gui_test_board(-NewBoard),
    nb_setval(board, NewBoard),
    draw_board(NewBoard, Canvas).

quick_sort([], []).
quick_sort([Cell|Rest], Sorted) :-
    pivot(Cell, Rest, Lesser, Greater),
    quick_sort(Lesser, LesserSorted),
    quick_sort(Greater, GreaterSorted),
    append(LesserSorted, [Cell|GreaterSorted], Sorted).

pivot(_, [], [], []).
pivot(CellX, [CellY|T], [CellY|Lesser], Greater) :-
    get_stack_pos(CellX, X),
    get_stack_pos(CellY, Y),
    Y =< X,
    pivot(CellX, T, Lesser, Greater).
pivot(CellX, [CellY|T], Lesser, [CellY|Greater]) :-
    get_stack_pos(CellX, X),
    get_stack_pos(CellY, Y),
    Y > X,
    pivot(CellX, T, Lesser, Greater).


