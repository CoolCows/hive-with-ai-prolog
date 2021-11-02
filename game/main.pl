:- use_module( cell, [ get_row/2,
					   set_row/3]).
:- use_module( board, [ add_cell/3 ] ).


% this is just for testing
move_cell(Cell,NewCell) :-
	get_row(Cell,Row), 
	NewRow is Row + 1, 
	set_row(NewRow,Cell, NewCell).
	
%move_cell(cell(ant,1,2),Y).	
%add_cell(cell(ant,1,2),board([]), R ).

