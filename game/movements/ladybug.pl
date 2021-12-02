:- module(ladybug, [ valid_ladybug_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").


valid_ladybug_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	adjacent_cell(SourceCell,AdjCell1),
	not(get_bug_type(AdjCell1,none)),
	AdjCell1 \== SourceCell,
	adjacent_cell(AdjCell1,AdjCell2),
	not(get_bug_type(AdjCell2,none)),
	AdjCell2 \== SourceCell,
	AdjCell2 \== AdjCell1,
	adjacent_cell(AdjCell2,DestCell),
	get_bug_type(DestCell,none),
	DestCell \== SourceCell,
	DestCell \== AdjCell1,
	DestCell \== AdjCell2.

	
