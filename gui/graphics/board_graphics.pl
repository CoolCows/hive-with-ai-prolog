:- module(board_graphics, [draw_board/2]).
:- use_module(commons, [get_hexagon/4]).
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
    send(Canvas, background, colour(brown)),
    get(Canvas, size, size(W, H)),
    CH is H/2, CW is W/2,
    nb_getval(scale, Scale),
    %Erase this funcall when logic is functional

    %Var '_' is going to be the Board
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
    set_hexagon_color(Cell, Hexagon),
    %T0d0: Modify the hexagon so it represents correctly the player and bug
    send(Canvas, display, Hexagon),
    draw_bug(Cell, X, Y, Canvas),
    draw_cells(Rest, Canvas, point(CW, CH), Scale),!.

set_hexagon_color(Cell, Hex) :-
    get_color(Cell, Color),
    (
        (Color = black, send(Hex, colour, colour(lightgray)), send(Hex, fill_pattern, colour(white)));
        (Color = white, send(Hex, colour, colour(darkgray)), send(Hex, fill_pattern, colour(black)))
    ).


draw_bug(Cell, X, Y, Canvas) :-
    get_bug_type(Cell, Bug),
    (
        (Bug = queen, new(BM, bitmap( './graphics/xpm/queen.xpm')));
        (Bug = ant, new(BM, bitmap('./graphics/xpm/ant.xpm')));
        (Bug = beetle, new(BM, bitmap('./graphics/xpm/beetle.xpm')));
        (Bug = grasshopper, new(BM, bitmap('./graphics/xpm/grasshopper.xpm')));
        (Bug = spyder, new(BM, bitmap('./graphics/xpm/spyder.xpm')));
        (Bug = pillbug, new(BM, bitmap('./graphics/xpm/pillbug.xpm')));
        (Bug = mosquito, new(BM, bitmap('./graphics/xpm/mosquito.xpm')));
        (Bug = ladybug, new(BM, bitmap('./graphics/xpm/ladybug.xpm')))
    ),
    send(Canvas, display, BM, point(X - 35, Y - 35)).
    
% Erase method when complete
get_test_cells(C) :-
    init_cell(ant, 0, 0, black, 0, C1),
    init_cell(spyder, 0, 1, black, 0, C2),
    init_cell(queen, 0, 2, white, 0, C3),
    init_cell(grasshopper, 0, 3, white, 0, C4),
    init_cell(beetle, 1, 0, white, 0, C5),
    init_cell(pillbug, 1, 1, white, 0, C6),
    init_cell(spyder, 1, 2, white, 0, C7),
    init_cell(spyder, 1, 3, white, 0, C8),
    init_cell(mosquito, 0, -1, white, 0, C9),
    init_cell(ladybug, 0, -2, white, 0, C10),
    init_cell(beetle, 0, -3, white, 0, C11),
    init_cell(beetle, -1, 0, white, 0, C12),
    init_cell(ant, 1, -1, white, 0, C13),
    init_cell(ant, 1, -2, white, 0, C14),
    init_cell(ant, 1, -3, white, 0, C15),
    append(C, [], [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]).


