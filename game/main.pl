:- use_module( cell).
:- use_module( board).
:- use_module( player).
:- use_module( "movements/movements" ).
% this is just for testing
%
testing(P) :- 
	% add_new_cell(cell(queen,1,2,white,1),B,NewB0),
	% add_new_cell(cell(ant,1,2,black,0),NewB0, NewB),
	% insect_above(cell(ant,1,2,white,0),NewB).
	% init_cell(ant, 1,2,black,0),	
	% init_cell(ant, 2,2,black,0),	
	init_player(black),

	add_new_cell(cell(ant,1,2,black,0)),
	add_new_cell(cell(grasshopper,2,2,black,0)),
	add_new_cell(cell(ant,0,2,black,0)),
	% one_hive(cell(spider,2,2,black,0)).
	% move_cell(cell(ant,1,2,black,0),cell(none,1,1,none,0),R),
	valid_movements(cell(grasshopper,2,2,black,0),P).
	% adjacent_cells(cell(spider,2,2,black,0),P).
	% delete_cell(cell(ant,1,2,black,0)),
	% init_cell(R),
	% get_player(player(black,_,_,_,_,_,_,_,_),X),
	% write(X),
	% add_new_cell(cell(ant,1,2,black,0)),
	% get_player(player(black,_,_,_,_,_,_,_,_),P).
	% cells(P).

	% init_cell(ant, 1,4,white,0),	
	% valid_new_cell(black, P).
	% adjacent_cells(cell(ant,1,2,white,0),P).

	

	% reachable(cell(ant,1,0,none,0),NewB,[cell(ant,1,0,none,0)],P).
	% one_hive(NewB,cell(ant,1,0,white,0),P).
	% get_cell(1,1,NewB,SourceCell),
	% get_cell(1,2,NewB, DestCell),
	% get_cell(0,1, NewB, DestCell),
	% move_cell(SourceCell,DestCell, NewB, P).
	% adjacent_cells(Cell, NewB, P),
	% movements_queen( Cell, NewB, P ).
	

