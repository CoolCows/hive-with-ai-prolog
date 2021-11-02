:- module( board, [ init_board/2,
					get_cells/2,
					add_cell/3] ).

:- use_module( utils, [push/3] ).

% Board structure ->  board(listOfCells,turns)
init_board(board([],0)).

get_cells(board(Cells,_), Cells).

get_current_turn(board(_, Turns),Turns).

add_cell(Cell,Board, NewBoard) :-
	get_cells(Board,BoardCells),
	push(Cell, BoardCells, NewBoard).

