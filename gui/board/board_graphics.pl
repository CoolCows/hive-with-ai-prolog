:- module(board_graphics, [draw_board/2]).

:- use_module(library(pce)).
:- use_module("../primitives/draw_hex", [draw_hexagon/4]).
:- use_module("../../game/cell", [init_cell/6,
                                  get_bug_type/2,
                                  get_row/2,
                                  get_col/2,
                                  get_color/2,
                                  get_stack_pos/2]).
:- use_module(tools, [slope/5,
                      trace/4,
                      below_line/4,
                      above_line/4]).


draw_board(Board, Picture) :-
    send(Picture, clear),
    get(Picture, height, H),
    get(Picture, width, W),
    MX is W/2, MY is H/2, 
 
    %Erase this funcall when logic is functional
    get_test_cells(C),
    board_to_graphic_board(C, GCells),
    
    nb_setval(board_center, point(MX, MY)),
    nb_setval(graphic_cells, GCells),
    nb_getval(graphic_cells, GCells2),
    
    draw_tiles(GCells, Picture). 

board_to_graphic_board([], []). 
board_to_graphic_board([Cell|Rest], GraphicBoard) :-
    get_col(Cell, Col),
    get_row(Cell, Row),
    get_bug_type(Cell, Bug),
    get_color(Cell, Color),
    get_stack_pos(Cell, StackPos),
    new(GraphicCell, graphic_cell(Bug, point(Col, Row), Color, StackPos)),
    board_to_graphic_board(Rest, RestGraphicBoard),
    append([GraphicCell], RestGraphicBoard, GraphicBoard).

draw_tiles([],_).
draw_tiles([Cell|Rest], Picture) :-
    nb_getval(scale, S),
    get_coordinates(Cell,S, X, Y),
    draw_hexagon(X, Y, S, Hex),
    send(Picture, display, Hex),
    draw_tiles(Rest, Picture).

draw_pos_moves([],_,_,_,_).
draw_pos_moves([Mov|Rest], Picture, S, MX, MY) :-
    get_coordinates(Mov,MX, MY, S, X, Y),
    draw_hexagon(X, Y, S, Hex),
    send(Hex, fill_pattern, colour(green)),
    send(Picture, display, Hex),
    draw_pos_moves(Rest, Picture, S, MX, MY).

get_coordinates(GrCell,S, X, Y) :-
    nb_getval(board_center, O),
    get(O, x, OX),
    get(O, y, OY),
    get(GrCell, position, Pos),
    get(Pos, x, CX),
    get(Pos, y, CY),
    X is OX + S*75*CX,
    Y is OY + S*100*CY + S*50*(CX mod 2).

get_coordinates(Cell, MX, MY, S, X, Y) :-
    get_col(Cell, Xindex),
    get_row(Cell, Yindex),
    X is MX + S*75*Xindex,
    Y is MY + S*100*Yindex + S*50*(Xindex mod 2).

get_test_cells(C) :-
    init_cell(bug, 0, 0, white, 0, C1),
    init_cell(bug, 0, 1, white, 0, C2),
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


:- pce_begin_class(graphic_cell, object).

variable(type, name, both, 'Specify cell bug or if it si postion is empty').
variable(position, point, both, 'Indicates the center location of the hexagon').
variable(color, name, both, 'Which player owns this cell').
variable(stack_pos, int, both, 'If bug is above any other bug').

initialise(C, Type:name, Position:point, Color:name, StackPos:int) :->
    send(C, type, Type),
    send(C, position, Position),
    send(C, color, Color),
    send(C, stack_pos, StackPos).

click_inside(C, ClickPosition:point, Result:bool) :->
    get(ClickPosition, y, CY),
    get(C, center, Center),get(Center, x, X),get(Center, y, Y),
    Dist is 50,  
    CY < Y + Dist,
    CY > Y - Dist,
    line(point(X + Dist/2, Y + Dist), point(X + Dist, Y), M1, N1),
    below_line(ClickPosition, M1, N1),
    Result is true.

:- pce_end_class.
