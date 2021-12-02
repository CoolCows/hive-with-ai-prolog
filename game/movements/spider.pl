:- module(spider, [ valid_spider_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").


valid_spider_movement(SourceCell,DestCell):-
	% TODO: checks if the cell is accesible
	one_hive(SourceCell),
	cells(HiveCells),
	delete(HiveCells,SourceCell,HiveCellsWithoutSourceCell),
	adjacent_cell(SourceCell,AdjCell1),
	adjacent_to_hive(AdjCell1,HiveCellsWithoutSourceCell),
	AdjCell1 \== SourceCell,
	adjacent_cell(AdjCell1,AdjCell2),
	adjacent_to_hive(AdjCell2,HiveCellsWithoutSourceCell),
	AdjCell2 \== SourceCell,
	AdjCell2 \== AdjCell1,
	adjacent_cell(AdjCell2,DestCell),
	adjacent_to_hive(DestCell,HiveCellsWithoutSourceCell),
	DestCell \== SourceCell,
	DestCell \== AdjCell1,
	DestCell \== AdjCell2.
