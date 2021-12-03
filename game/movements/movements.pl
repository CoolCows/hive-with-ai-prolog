:- module(movements, [ valid_movement/2,valid_movements/2,movable_cells_by_pillbug/2, movable_positions_by_pillbug/2 ]).

:- use_module("../cell").
:- use_module(ant).
:- use_module(queen).
:- use_module(spider).
:- use_module(grasshopper).
:- use_module(ladybug).
:- use_module(beetle).
:- use_module(pillbug).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,beetle),!,
	valid_beetle_movement(SourceCell,DestCell).

valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,ant),!,
	valid_ant_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,queen),!,
	valid_queen_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,spider),!,
	valid_spider_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,grasshopper),!,
	valid_grasshopper_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,ladybug),!,
	valid_ladybug_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,pillbug),!,
	valid_pillbug_movement(SourceCell,DestCell).

% all possible movements of a given cell 
valid_movements(SourceCell,DestCells):-
	findall(DestCell, valid_movement(SourceCell,DestCell),A),
	list_to_set(A,DestCells).

