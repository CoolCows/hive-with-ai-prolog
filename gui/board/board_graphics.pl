:- module(board_graphics, [draw_board/2,
                       draw_board/3]).

:- use_module("../primitives/draw_hex", [draw_hexagon/4]).
:- use_module("../../game/cell", [init_cell/6,
                                  get_bug_type/2,
                                  get_row/2,
                                  get_col/2,
                                  get_stack_pos/2]).

draw_board(Board, Picture) :-
    draw_board(Board, Picture, 1).

draw_board(_, Picture, S) :-
    send(Picture, clear),
    get(Picture, height, H),
    get(Picture, width, W),
    MX is W/2, MY is H/2,
    get_test_cells(C),
    %draw_tiles(C, Picture, S, MX, MY). 
    draw_pos_moves(C, Picture, S, MX, MY).
    
draw_tiles([],_,_,_,_).
draw_tiles([Cell|Rest], Picture, S, MX, MY) :-
    get_coordinates(Cell, MX, MY, S, X, Y), 
    draw_hexagon(X, Y, S, Hex),
    send(Picture, display, Hex),
    draw_tiles(Rest, Picture, S, MX, MY).

draw_pos_moves([],_,_,_,_).
draw_pos_moves([Mov|Rest], Picture, S, MX, MY) :-
    get_coordinates(Mov,MX, MY, S, X, Y),
    draw_hexagon(X, Y, S, Hex),
    send(Hex, fill_pattern, colour(green)),
    send(Picture, display, Hex),
    draw_pos_moves(Rest, Picture, S, MX, MY).

get_coordinates(Cell, MX, MY, S, X, Y) :-
    get_col(Cell, Xindex),
    get_row(Cell, Yindex),
    X is MX + S*75*Xindex,
    Y is MY + S*100*Yindex + S*50*(Xindex mod 2).

get_test_cells(C) :-
    init_cell(0, 0, 0, 0, 0, C1),
    init_cell(0, 0, 1, 0, 0, C2),
    init_cell(0, 0, 2, 0, 0, C3),
    init_cell(0, 0, 3, 0, 0, C4),
    init_cell(0, 1, 0, 0, 0, C5),
    init_cell(0, 1, 1, 0, 0, C6),
    init_cell(0, 1, 2, 0, 0, C7),
    init_cell(0, 1, 3, 0, 0, C8),
    init_cell(0, 0, -1, 0, 0, C9),
    init_cell(0, 0, -2, 0, 0, C10),
    init_cell(0, 0, -3, 0, 0, C11),
    init_cell(0, -1, 0, 0, 0, C12),
    init_cell(0, 1, -1, 0, 0, C13),
    init_cell(0, 1, -2, 0, 0, C14),
    init_cell(0, 1, -3, 0, 0, C15),
    append(C, [], [C1, C2, C3, C4, C5]).%, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]).


