:- use_module(player, [
    init_player/1,
    players/1
]).

gui_init_board(-Board) :-
    % Call method that return the board
    true.

gui_init_players(Players) :-
    init_player(white),
    init_player(black),
    players(Players).

gui_put_cell(+Cell, -Board) :-
    % Tries to put cell in a certain location
    true.

gui_move_cell(+Cell, -Board) :-
    % Tries to move cell to a certain location
    true.

