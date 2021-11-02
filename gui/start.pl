% Load libraries and tools
:- use_module(library(pce)).
:- use_module("./primitives/draw_hex", [draw_hexagon/3]).

% Start Game Window
gui_init() :-
    new(MainWin, frame("CoolCows' Hive Game")),
    send(MainWin, append, new(Board, picture("Board"))),
    file_dialog_setup(Board),
    menu_bar_setup(MainWin, Board),

    send(MainWin, open).

menu_bar_setup(MainWin, Board) :-
    send(new(D, dialog), above, Board),
    send(D, append, new(MB, menu_bar)),
    send(MB, append, new(File, popup(file))),
    send_list(File, append,
                [menu_item(change_game,
                            and(message(@prolog, game_type_selection()),
                                message(MainWin, destroy))),
                 menu_item(exit, message(MainWin, destroy))]).

file_dialog_setup(Board) :-
    send(new(M, dialog),right, Board),
    send(M, gap, size(0, 0)),
    send(M, ver_shrink, 100),
    send(M, ver_stretch, 100),
    send(M, display, new(MM, menu(mode, choice))),
    send(MM, layout, vertical),
    send(MM, show_label, @off),
    send_list(MM, append,
                [menu_item(item1),
                 menu_item(item2)]),
    send_list(MM, new,
                [menu_item(item3)]).

% Open Select Game Window
game_type_selection() :-
    new(S, frame("Choose Playstyle")),
    send(S, append, new(D, dialog)),
    send(D, append, new(BTS, dialog_group(buttons, group))),
    send(BTS, gap, size(20,0)),
    send(BTS, append, button(local_game,
                        and(message(@prolog,gui_init()),
                            message(S, destroy)))),
    send(BTS, append, button(vs_ai)),
    send(BTS, append, button(online_game)),
    send(BTS, append, button(exit, message(S, destroy))),
    send(BTS, layout_dialog),
    send(BTS, open).

% ?- game_type_selection().
?- gui_init().
