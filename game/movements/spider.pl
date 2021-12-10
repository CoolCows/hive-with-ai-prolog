:- module(spider, [ valid_spider_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").


valid_spider_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	cells(Cells),
	delete(Cells,SourceCell,NewCells),

	adjacent_cell(SourceCell,AdjCell1),
	get_bug_type(AdjCell1,none),
	accesible_cell(SourceCell,NewCells,AdjCell1),
	adjacent_hive_cell(SourceCell,NewCells,AdjHiveCell1),
	adjacent_cell(AdjCell1,AdjHiveCell1),

	adjacent_cell(AdjCell1,AdjCell2),
	get_bug_type(AdjCell2,none),
	AdjCell2 \= SourceCell,
	accesible_cell(AdjCell1,NewCells,AdjCell2),
	adjacent_hive_cell(AdjCell1,NewCells,AdjHiveCell2),
	adjacent_cell(AdjCell2,AdjHiveCell2),
	AdjHiveCell2 \= SourceCell,

	adjacent_cell(AdjCell2,DestCell),
	get_bug_type(DestCell,none),
	DestCell \= SourceCell,
	DestCell \= AdjCell1,
	accesible_cell(AdjCell2,NewCells,DestCell),
	adjacent_hive_cell(AdjCell2,NewCells,AdjHiveCell3),
	adjacent_cell(DestCell,AdjHiveCell3),
	AdjHiveCell2 \= SourceCell.



