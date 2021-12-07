:- module(gui_api,[
    gui_init_board/1,
    gui_init_players/1,
    gui_put_cell/3,
    gui_move_cell/3,
    gui_get_possible_moves/2,
    gui_get_possible_positions/2,
	gui_get_pillbug_effect/2,
    gui_get_board/1
]).

:- use_module(player).
:- use_module(cell).
:- use_module("movements/movements").
:- use_module(board).
:- use_module(turns).
:- use_module(hive_api).

dummy_init:-
	write_ln("GAME STARTED"),
    % init_cell(cell(queen, 0, 0, white, 0)),
    % init_cell(cell(pillbug, 1, 0, white, 0)),
    % init_cell(cell(mosquito, 2, 0, white, 0)),
    % init_cell(cell(ant, 1, 1, white, 0)),
    % init_cell(cell(ant, 1, -1, white, 0)),
    % init_cell(cell(queen, -1, 0, black, 0)),
    % init_cell(cell(pillbug, -2, 0, black, 0)),
    % init_cell(cell(mosquito, -3, 0, black, 0)),
    % init_cell(cell(ant, -3, -1, black, 0)),
    % init_cell(cell(ant, -3, 1, black, 0)),
    true.

gui_init_board(-Board) :-
    % Call method that return the board
    dummy_init,
	init_turns,
    cells(Board).

gui_get_board(-Board) :-
    cells(Board).

gui_init_players(-Players) :-
	hive_init_players(),
    players(Players).

gui_put_cell(+Cell, -Board, -NewPlayer) :-
	hive_put_cell(Cell),
    get_color(Cell, Color),
    get_player(player(Color,_,_,_,_,_,_,_,_), NewPlayer),
    cells(Board).

gui_move_cell(+SourceCell, +DestCell, -Board) :-
    % Tries to move cell to a certain location
    % Returns the new board if succesful
	hive_move_cell(SourceCell,DestCell),
    cells(Board).

gui_get_possible_moves(+Cell, -Board) :-
    % Get all Cells where a bug can be moved
    % return the board with possible positions.
    % Possible position cells has color = bug = none
	hive_get_possible_moves(Cell,PosMoves),
    cells(Cells),
    append(PosMoves, Cells, Board).

gui_get_possible_positions(+Color, -Board) :-
    % Get all Cells where a bug by certain player can be put
    % return the board with possible positions.
    % Possible position cells has color = bug = none
	hive_get_possible_positions(Color,PosPositions),
	cells(Cells),
    append(PosPositions, Cells, Board).

gui_get_pillbug_effect(+PillbugCell, -[MovableBugs, MovablePositions]) :-
    % call to method that with pillbug cells return 
    % a list of MovableBugs
 	hive_get_pillbug_effect(PillbugCell,[MovableBugs,MovablePositions]).

gui_mosquito_adyacent_pillbug(+MosquitoCell) :-
	hive_mosquito_adyacent_pillbug(MosquitoCell).

gui_skip_turn():-
	hive_skip_turn().

