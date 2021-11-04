:- module(board_graphics, [draw_board/2,
                       draw_board/3]).

:- use_module(library(pce)).
:- use_module("../primitives/draw_hex", [draw_hexagon/4]).
:- use_module("../../game/cell", [init_cell/6,
                                  get_bug_type/2,
                                  get_row/2,
                                  get_col/2,
                                  get_stack_pos/2]).
:- use_module(tools, [slope/5,
                      trace/4,
                      below_line/4,
                      above_line/4]).

draw_board(Board, Picture) :-
    draw_board(Board, Picture, 1).

draw_board(_, Picture, S) :-
    print('Drawing Board'),
    send(Picture, clear),
    get(Picture, height, H),
    get(Picture, width, W),
    MX is W/2, MY is H/2,
    get_test_cells(C),
    print('Obtained Test Cells'),
    draw_tiles(C, Picture, S, MX, MY). 
    %draw_pos_moves(C, Picture, S, MX, MY).
    
draw_tiles([],_,_,_,_).
draw_tiles([Cell|Rest], Picture, S, MX, MY) :-
    print('Drawing Tile'),
    get_coordinates(Cell, X, Y),
    print('Coordinates gotten'),
    %get_coordinates(Cell, MX, MY, S, X, Y), 
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

get_coordinates(Cell, X, Y) :-
    print('Analyzing new'),
    get(Cell, position, Pos),
    get(Pos, x, X),
    get(Pos, y, Y).

get_coordinates(Cell, MX, MY, S, X, Y) :-
    print('Analyzing old'),
    get_col(Cell, Xindex),
    get_row(Cell, Yindex),
    X is MX + S*75*Xindex,
    Y is MY + S*100*Yindex + S*50*(Xindex mod 2).

get_test_cells([X]) :-
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
    append(CF, [], [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15]),
    new(X, graphic_cell(bug, point(0,0), point(0,0))).


:- pce_begin_class(graphic_cell, object).

variable(type, name, both, 'Specify cell bug or if it si postion is empty').
variable(position, point, both, 'Indicates the center location of the hexagon').
variable(center, point, both, 'Indicates the center of the board').
variable(selected, bool, both, 'True if the user select this cell').
variable(color, name, both, 'Which player owns this cell').

initialise(C, Type:name, Position:point, Center:point) :->
    send(C, type, Type),
    send(C, position, Position),
    send(C, center, Center),
    send(C, selected, false).

click_inside(C, ClickPosition:point, Result:bool) :->
    get(ClickPosition, x, CX), get(ClickPosition, y, CY),
    get(C, center, Center),get(Center, x, X),get(Center, y, Y),
    Dist is 50,  
    CY < Y + Dist,
    CY > Y - Dist,
    line(point(X + Dist/2, Y + Dist), point(X + Dist, Y), M1, N1),
    below_line(ClickPosition, M1, N1).



select(C) :->
    send(C, selected, true). 

deselect(C) :->
    send(C, selected, false).

:- pce_end_class.
