:- module(gui_api,[
    gui_init_board/1,
    gui_init_players/1,
    gui_put_cell/2,
    gui_move_cell/2
]).

:- use_module(player, [
    init_player/1,
    players/1
]).
:- use_module(cell).
:- use_module("movements/movements").

dummy_init:-
    init_cell(queen, 0, 0, white, 0),
    init_cell(ant, 0, 1, black, 0),
    init_cell(beetle, 0, 2, white, 0).

gui_init_board(-Board) :-
    % Call method that return the board
    dummy_init,
    cells(Board).

gui_init_players(-Players) :-
    init_player(white),
    init_player(black),
    players(Players).

gui_put_cell(+Cell, -Board) :-
    % Tries to put the cell in certain location
    % Returns the new board if succesful
    Board = [].

gui_move_cell(+Cell, -Board) :-
    % Tries to move cell to a certain location
    % Returns the new board if succesful
    Board = [].

gui_get_possible_moves(+Cell, -Board) :-
    % Get all Cells where a bug can be moved
    % return the board with possible positions.
    % Possible position cells has color = bug = none
    valid_movements(Cell, PosMoves),
    write_ln(PosMoves),
    cells(Cells),
    append(PosMoves, Cells, Board).

gui_get_possible_positions(+Color, -Board) :-
    % Get all Cells where a bug by certain player can be put
    % return the board with possible positions.
    % Possible position cells has color = bug = none
    Board= [].

% ALL RETURN TYPES MUST BE LISTS OF TYPE CELL
