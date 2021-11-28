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

gui_init_board(-Board) :-
    % Call method that return the board
    Board = [
        cell(spyder, 0, 0, white, 0),
        cell(ant, 0, 1, white, 0),
        cell(queen, 1, 0, black, 0),
        cell(none, 1, 1, none, 0),
        cell(none, 2, 1, none, 0),
        cell(none, 3, 1, show, 0)
    ].

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

gui_get_possible_moves(+Cell, -PosMoves) :-
    % Given a Cell, it's positon and type
    % return possible moves, which is a 
    % list of Cells with bug=None and color=show
    PosMoves = [].

gui_get_possible_positions(+Color, -PosPositions) :-
    % Get all Cells where a bug by certain player can be put
    % return possible positions, which is a 
    % list of Cells with bug=None and color=show
    PosPositions= [].

% ALL RETURN TYPES MUST BE LISTS OF TYPE CELL
