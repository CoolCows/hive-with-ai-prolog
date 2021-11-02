% Load libraries and tools
:- use_module(library(pce)).
:- use_module("./primitives/draw_hex", [draw_hexagon/3]).

% Start Main Window
gui_init() :-
    new(MainWin, frame("CoolCows' Hive Game")),
    send(MainWin, append, new(Board, picture("Board"))),
    send(new(D, dialog),above(Board)),
    send(D, append, button(change_game,
                                and(message(@prolog, game_type_selection()),
                                    message(MainWin, destroy)))),
    send(D, append, button(exit, message(MainWin, destroy))),
    send(MainWin, open).

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
