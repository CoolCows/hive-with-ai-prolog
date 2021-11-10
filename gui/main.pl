% Load libraries and tools
:- use_module(library(pce)).
:- use_module("./graphics/board_graphics", [draw_board/2]).
:- use_module("./events/board_events", [select_event/2]).


menu_bar_setup(MainWin, Board) :-
    send(new(D, dialog), above, Board),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file))),
    send_list(File, append,
                [new(Opt, popup(new_Game)),
                 menu_item(exit, message(MainWin, destroy))]),
    send_list(Opt, append,
                [menu_item(local_game, message(@prolog, start_game, Board)),
                 menu_item(against_AI),
                 menu_item(online)]).

file_dialog_setup(Board) :-
    send(new(T, dialog), right, Board),
    send(T, ver_shrink, 100),
    send(T, ver_stretch, 100),
    send(T, width, 512),

    send(T, append, new(P1, picture)),
    send(P1, width, 500),
    send(P1, height, 250),

    send(T, append, new(P2, picture)),
    send(P2, width, 500),
    send(P2, height, 250),

    send(button(refresh, message(@prolog, draw_board,[], Board)), below, P2).

start_game(Board) :-
    % Init Global Vars
    nb_setval(scale, 0.75),
    nb_setval(move_cell, false),
    nb_setval(position_cell, false),
    nb_setval(player_turn, white),
    draw_board([], Board).

gui_init :-
    %Setting up Game Panel
    new(MainWin, frame("CoolCows Hive Game")),
    send(MainWin, append, new(Board, picture("Board"))),
    send(Board, width, 1280),
    send(Board, height, 720),
    send(Board, recogniser,
            click_gesture(left, '', single,
                          message(@prolog, select_event, Board, @event?position))),
    file_dialog_setup(Board),
    menu_bar_setup(MainWin, Board),
    send(MainWin, open).

?- gui_init.
