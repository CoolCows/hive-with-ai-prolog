:- module( queen, [ movements_queen/3 ] ).

:- use_module("../board", [ get_cells/2 ])
:- use_module("../cell.", [ get_color/2,
							get_row/2,
							get_col/2
							])

movements_queen(Cell, Board, PossibleMovements):-
	get_cells(Board,Cells),
	get_color(Cell,CellColor).


non_occuppied_adjacent(Cell, Cells, Result):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjRow is Row 	

