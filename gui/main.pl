% Load libraries and tools
:- use_module(library(pce)).
:- use_module("./graphics/board_graphics", [draw_board/2]).
:- use_module("./graphics/side_board_graphics", [draw_side_board/3]).
:- use_module("./events/board_events", [select_event/2]).
:- use_module("./events/side_board_events", [select_side_board/3]).
:- use_module("../game/gui_api").


menu_bar_setup(MainWin, Board, BlackCells, WhiteCells) :-
    send(new(D, dialog), above, Board),
    send(D, append, new(MB, menu_bar)),
    send(D, background, colour(orange)),

    send(MB, append, new(File, popup(file))),
    send_list(File, append,
                [new(Opt, popup(new_Game)),
                 menu_item(exit, message(MainWin, destroy))]),
    send_list(Opt, append,
                [menu_item(local_game, message(@prolog, start_game, Board, BlackCells, WhiteCells)),
                 menu_item(against_AI),
                 menu_item(online)]).

file_dialog_setup(Board, BlackCells, WhiteCells) :-
    send(new(T, dialog), right, Board),
    send(T, ver_shrink, 100),
    send(T, ver_stretch, 100),
    send(T, background, colour(orange)),

    send(T, append, new(BlackCells, window)),
    send(BlackCells, width, 400),
    send(BlackCells, height, 200),
    send(BlackCells, recogniser, 
            click_gesture(left, '', single,
                            message(@prolog, select_side_board, BlackCells, @event?position, black))),

    send(T, append, new(WhiteCells, window)),
    send(WhiteCells, width, 400),
    send(WhiteCells, height, 200),
    send(WhiteCells, recogniser,
            click_gesture(left, '', single,
                            message(@prolog, select_side_board, WhiteCells, @event?position, white))),

    send(button(refresh, message(@prolog, draw_board,[], Board)), below, WhiteCells).

start_game(Canvas, BlackCells, WhiteCells) :-
    % Init Global Vars
    nb_setval(scale, 0.75),
    nb_setval(move_cell, undefined),
    nb_setval(position_cell, undefined),
    nb_setval(player_turn, white),

    gui_init_players([Player1, Player2]),
    nb_setval(white_player, Player1),
    nb_setval(black_player, Player2),

    gui_init_board(Board),
    nb_setval(board, Board),

    draw_side_board(Player1, white, WhiteCells),
    draw_side_board(Player2, black, BlackCells),
    draw_board([], Canvas).

gui_init :-
    %Setting up Game Panel
    new(MainWin, frame("CoolCows Hive Game")),
    send(MainWin, append, new(Board, picture("Board"))),
    send(Board, width, 1280),
    send(Board, height, 720),
    send(Board, recogniser,
            click_gesture(left, '', single,
                          message(@prolog, select_event, Board, @event?position))),
    file_dialog_setup(Board, BlackPlayer, WhitePlayer),
    menu_bar_setup(MainWin, Board, BlackPlayer, WhitePlayer),
    send(MainWin, open).
    %draw_side_board([], BlackPlayer),
    %draw_side_board([], WhitePlayer).

?- gui_init.
