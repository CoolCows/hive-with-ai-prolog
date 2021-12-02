:- module(grasshopper, [ valid_grasshopper_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").

:- discontiguous valid_grasshopper_movement/2.

valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_1(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_1(AdjCell,DestCell).

jump_adjacent_1(SourceCell,DestCell):-
	adjacent_cell_1(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_1(SourceCell,DestCell):-
	adjacent_cell_1(SourceCell,AdjCell),
	jump_adjacent_1(AdjCell,DestCell).


valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_2(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_2(AdjCell,DestCell).

jump_adjacent_2(SourceCell,DestCell):-
	adjacent_cell_2(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_2(SourceCell,DestCell):-
	adjacent_cell_2(SourceCell,AdjCell),
	jump_adjacent_2(AdjCell,DestCell).


valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_3(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_3(AdjCell,DestCell).

jump_adjacent_3(SourceCell,DestCell):-
	adjacent_cell_3(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_3(SourceCell,DestCell):-
	adjacent_cell_3(SourceCell,AdjCell),
	jump_adjacent_3(AdjCell,DestCell).


valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_4(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_4(AdjCell,DestCell).

jump_adjacent_4(SourceCell,DestCell):-
	adjacent_cell_4(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_4(SourceCell,DestCell):-
	adjacent_cell_4(SourceCell,AdjCell),
	jump_adjacent_4(AdjCell,DestCell).


valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_5(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_5(AdjCell,DestCell).

jump_adjacent_5(SourceCell,DestCell):-
	adjacent_cell_5(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_5(SourceCell,DestCell):-
	adjacent_cell_5(SourceCell,AdjCell),
	jump_adjacent_5(AdjCell,DestCell).


valid_grasshopper_movement(SourceCell,DestCell):-
	adjacent_cell_6(SourceCell,AdjCell),
	not(get_bug_type(AdjCell, none)),
	jump_adjacent_6(AdjCell,DestCell).

jump_adjacent_6(SourceCell,DestCell):-
	adjacent_cell_6(SourceCell,AdjCell),
	get_bug_type(AdjCell,none),
	DestCell = AdjCell,!.

jump_adjacent_6(SourceCell,DestCell):-
	adjacent_cell_6(SourceCell,AdjCell),
	jump_adjacent_6(AdjCell,DestCell).
