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

dummy_init:-
    init_cell(queen, 0, 0, white, 0),
    init_cell(spider, 1, 0, black, 0),
    init_cell(spider, -1, -1, white, 0),
    init_cell(beetle, -1, 0, black, 0),
    init_cell(ant, 0, 2, white, 0),
    init_cell(spider, -1, 1, black, 0),
    init_cell(pillbug, -2, 1, white, 0).

gui_init_board(-Board) :-
    % Call method that return the board
    dummy_init,
    cells(Board).

gui_get_board(-Board) :-
    cells(Board).

gui_init_players(-Players) :-
    init_player(white),
    init_player(black),
    players(Players).

gui_put_cell(+Cell, -Board, -NewPlayer) :-
    % This method is yet to be implemented
    add_new_cell(Cell),
    get_color(Cell, Color),
    get_player(player(Color,_,_,_,_,_,_,_,_), NewPlayer),
    cells(Board).

gui_move_cell(+SourceCell, +DestCell, -Board) :-
    % Tries to move cell to a certain location
    % Returns the new board if succesful
    delete_cell(SourceCell),
    init_cell(DestCell),
    cells(Board).

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

    % This method is yet to be implemented
    bagof(ValidCell, valid_new_cell(Color, ValidCell), PosPositions),
    cells(Cells),
    append(PosPositions, Cells, Board).

gui_get_pillbug_effect(+PillbugCell, -[MovableBugs, MovablePositions]) :-
    % call to method that with pillbug cells return 
    % a list of MovableBugs
	movable_cells_by_pillbug(PillbugCell,MovableBugs),
    movable_positions_by_pillbug(PillbugCell, MovablePositions),
	write_ln('Adding movable bugs'),
	write_ln(MovableBugs),
    write_ln(MovablePositions).

% ALL RETURN TYPES MUST BE LISTS OF TYPE CELL
