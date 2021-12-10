:- module(ant, [ valid_ant_movement/2 ] ).

:- use_module("../board").
:- use_module("../cell").
:- use_module("../utils").

valid_ant_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
	delete_cell(SourceCell),
	ant_path(SourceCell,[],DestCells),
	init_cell(SourceCell),
	member(DestCell,DestCells),
<<<<<<< HEAD
	not(same_position(SourceCell,DestCell)).
=======
    not(same_position(SourceCell, DestCell)).
>>>>>>> 296f92396431c3fc9ea464c0a47bc6ce59e9e351

non_visited_ant(Cell, Visited, AdjCell):-
	adjacent_cell(Cell,AdjCell),
	accesible_cell(Cell,AdjCell),
	get_bug_type(AdjCell,none),
	adjacent_to_hive(AdjCell),
	not(member(AdjCell,Visited)).

ant_path([],X,X):-!.
ant_path([Cell|RestOfCells],Visited, ReachableCells):-
	!,
	ant_path(Cell,Visited,A),
	% NOTE: visit only the rest of non-visited cells instead of RestOfCells
	ant_path(RestOfCells,A,ReachableCells).

ant_path(Cell,Visited,ReachableCells):-
	findall(AdjCell, non_visited_ant(Cell, Visited,AdjCell),AdjCells),
	append(AdjCells,Visited,A),
	ant_path(AdjCells,A,ReachableCells).


