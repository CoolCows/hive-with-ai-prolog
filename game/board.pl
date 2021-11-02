:- module( board, [ init_board/2,
					get_cells/2,
					add_cell/3] ).

:- use_module( utils, [push/3] ).

% Board structure ->  board(listOfCells)
init_board(_,board([])).

get_cells(board(X), X).

add_cell(Cell,Board, NewBoard) :-
	get_cells(Board,BoardCells),
	push(Cell, BoardCells, NewBoard).

