:- module(beetle, [ valid_beetle_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").

valid_beetle_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	adjacent_cell(SourceCell,DestCell),
	accesible_cell_top_level(SourceCell,DestCell),
	get_bug_type(DestCell,none),
	adjacent_hive_cell(SourceCell,AdjHiveCell),
	adjacent_cell(DestCell,AdjHiveCell).

valid_beetle_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	adjacent_cell(SourceCell, AdjCell),
	accesible_cell_top_level(SourceCell,AdjCell),
	not(get_bug_type(AdjCell,none)),
    AdjCell = cell(_, Row, Col, _, Stack),
    NewStack is Stack + 1,
    DestCell = cell(none, Row, Col, none, NewStack).


	

