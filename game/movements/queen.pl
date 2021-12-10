:- module( queen, [valid_queen_movement/2 ] ).

:- use_module("../board").
:- use_module("../cell").
:- use_module("../utils").

valid_queen_movement(SourceCell, DestCell) :-
	one_hive(SourceCell),
    adjacent_cell(SourceCell, DestCell),
	accesible_cell(SourceCell,DestCell),
	get_bug_type(DestCell,none),
	adjacent_hive_cell(SourceCell,AdjHiveCell),
	adjacent_cell(AdjHiveCell,DestCell).
