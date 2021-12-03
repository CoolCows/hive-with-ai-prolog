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
	adjacent_cell(SourceCell, AdyCell),
	not(get_bug_type(AdyCell,none)),
    AdyCell = cell(_, Row, Col, _, Stack),
    NewStack is Stack + 1,
    DestCell = cell(none, Row, Col, none, NewStack).


	

