:- use_module( cell, [ get_row/2,
					   set_row/3,
					   get_bug_type/2]).

:- use_module( board, [ add_new_cell/3,
						get_cell/4,
						init_board/1,
						get_cells/2,
						adjacent_cells/3,
					    adjacent_cell_1/3,
					    adjacent_cell_2/3,
					    adjacent_cell_3/3,
					    adjacent_cell_4/3,
					    adjacent_cell_5/3,
					    adjacent_cell_6/3,
					    get_current_turn/2,
						reachable/4,
					    move_cell/4,
						insect_above/2] ).

:- use_module( player, [ init_player/1 ] ).
:- use_module( "./movements/queen", [ movements_queen/3 ] ).
% this is just for testing
%
testing(P) :- 
	init_board(B),
	add_new_cell(cell(queen,1,2,white,0),B,NewB0),
	add_new_cell(cell(ant,1,1,black,0),NewB0, NewB),
	insect_above(cell(ant,1,2,white,0),NewB).

	% reachable(cell(ant,1,0,none,0),NewB,[cell(ant,1,0,none,0)],P).
	% one_hive(NewB,cell(ant,1,0,white,0),P).
	% get_cell(1,1,NewB,SourceCell),
	% get_cell(1,2,NewB, DestCell),
	% get_cell(0,1, NewB, DestCell),
	% move_cell(SourceCell,DestCell, NewB, P).
	% adjacent_cells(Cell, NewB, P),
	% movements_queen( Cell, NewB, P ).
	

