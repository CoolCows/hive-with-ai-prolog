:- module( heuristics, [ 
    apply_heuristics/2
] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").
:- use_module("../game/cell").

apply_heuristics(Move, Value) :-
	write_ln("MOVE"),
	write_ln(Move),
	write_ln("HEURISTICS METRICS"),
	surround_enemy_queen(Move,A),
	write_ln(A),
	Value is A.

surround_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2)),Value):-
	DestCell = cell(B1,R2,C2,D1,S2),
	init_cell(DestCell),
	surround_enemy_queen_aux(DestCell,Value),
	delete_cell(DestCell).

surround_enemy_queen(place(Cell),Value):-
	init_cell(Cell),
	surround_enemy_queen_aux(Cell,Value),
	delete_cell(Cell).

surround_enemy_queen_aux(DestCell,Value):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	adjacent_cell(QueenCell,DestCell),
	Value = 1,!.
surround_enemy_queen_aux(DestCell,0).

free_current_player_queen(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2)):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	hive_current_player_color(Color),
	hive_get_cell(cell(queen,_,_,Color,_),QueenCell),
	adjacent_cell(QueenCell,SourceCell),
	not(adjacent_cell(QueenCell,DestCell)).
	
free_current_player_queen(place(Cell)):-
	hive_current_player_color(Color),
	hive_get_cell(cell(queen,_,_,Color,_),QueenCell),
	not(adjacent_cell(QueenCell,Cell)).

block_current_player_bug(move(SourceCell,DestCell)):-
	true.

block_current_player_bug(place(Cell)):-
	true.

block_enemy_bug(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2))):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	delete_cell(SourceCell),
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	init_cell(DestCell),
	adjacent_cell(DestCell,AdjCell),
	get_color(AdjCell,OponentColor),
	hive_get_possible_moves(AdjCell, Cond),
	delete_cell(DestCell),
	init_cell(SourceCell),
    Cond = [].

block_enemy_bug(place(Cell)):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	init_cell(Cell),
	adjacent_cell(Cell,AdjCell),
	get_color(AdjCell,OponentColor),
	hive_get_possible_moves(AdjCell, Cond),
	delete_cell(Cell),
    Cond = [].

