:- module(beetle, [ valid_beetle_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").

valid_beetle_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	adjacent_cell(SourceCell,DestCell),
	get_bug_type(DestCell,none),
	cells(Cells),
	delete(Cells,SourceCell,CellsWithoutSourceCell),
	adjacent_to_hive(DestCell,CellsWithoutSourceCell),
	adjacent_to_hive(DestCell).

valid_beetle_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	adjacent_cell(SourceCell,DestCell),
	not(get_bug_type(DestCell,none)).


	

