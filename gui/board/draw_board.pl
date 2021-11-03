:- module(draw_board, [draw_board/2,
                       draw_board/3]).

:- use_module("../primitives/draw_hex", [draw_hexagon/4]).

draw_board(Board, Picture) :-
    draw_board(Board, Picture, 1).

draw_board(Board, Picture, S) :-
    get(Picture, height, H),
    get(Picture, width, W),
    MX is W/2, MY is H/2,
    draw_hexagon(MX, MY, S, Hex),
    send(Picture, display, Hex).


