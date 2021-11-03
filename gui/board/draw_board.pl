:- module(draw_board, [draw_board/2,
                       draw_board/3]).

:- use_module("../primitives/draw_hex", [draw_hexagon/4]).
:- use_module("../../game/cell", [init_cell/6,
                                  get_bug_type/2,
                                  get_row/2,
                                  get_col/2]).

draw_board(Board, Picture) :-
    draw_board(Board, Picture, 1).

draw_board(_, Picture, S) :-
    send(Picture, clear),
    get(Picture, height, H),
    get(Picture, width, W),
    MX is W/2, MY is H/2,
    init_cell(0, 0, 0, 0, 0, C1),
    init_cell(0, 0, 1, 0, 0, C2),
    init_cell(0, 0, 2, 0, 0, C3),
    init_cell(0, 0, 3, 0, 0, C4),
    init_cell(0, 1, 0, 0, 0, C5),
    init_cell(0, 1, 1, 0, 0, C6),
    init_cell(0, 1, 2, 0, 0, C7),
    init_cell(0, 1, 3, 0, 0, C8),
    draw_tiles([C1, C2, C3, C4, C5, C6, C7, C8], Picture, S, MX, MY). 
    
draw_tiles([Cell|Rest], Picture, S, MX, MY) :-
    get_col(Cell, Xindex),
    get_row(Cell, Yindex),
    X is MX + S*75*Xindex,
    Y is MY + S*100*Yindex + S*50*(Xindex mod 2),
    %((Xindex mod 2 =:= 0, Y is MY + S*100*Yindex, print("Mod1"));
    %(Xindex mod 2 =:= 1, Y is MY + S*75*Yindex, print("Mod2"))),
    draw_hexagon(X, Y, S, Hex),
    send(Picture, display, Hex),
    draw_tiles(Rest, Picture, S, MX, MY).


