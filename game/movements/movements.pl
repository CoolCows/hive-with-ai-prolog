:- module(movements, [ 
    valid_movement/2,
    valid_movements/2,
    movable_cells_by_pillbug/2,
    movable_positions_by_pillbug/2,
    mosquito_adjacent_to_pillbug/1
]).

:- use_module("../cell").
:- use_module("../board").
:- use_module(ant).
:- use_module(queen).
:- use_module(spider).
:- use_module(grasshopper).
:- use_module(ladybug).
:- use_module(beetle).
:- use_module(pillbug).
:- use_module(mosquito).


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


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,mosquito),!,
	valid_mosquito_movement(SourceCell,DestCell).

% all possible movements of a given cell 
valid_movements(SourceCell,DestCells):-
	not(get_fixed_cell(fixed_cell(SourceCell,_),_)),
	not(insect_above(SourceCell,_)),
	findall(DestCell, valid_movement(SourceCell,DestCell),A),
	flatten(A,B),
	list_to_set(B,DestCells).

