:- module( heuristics, [ 
    apply_heuristics/2
] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").
:- use_module("../game/cell").

apply_heuristics(Move, Value) :-
	surround_enemy_queen(Move,A),
	closer_to_enemy_queen(Move,B),
	Value is A +  B.

surround_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2)),Value):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	init_cell(DestCell),
	surround_enemy_queen_aux(SourceCell,DestCell,Value),
	delete_cell(DestCell).

surround_enemy_queen(place(Cell),Value):-
	init_cell(Cell),
	surround_enemy_queen_aux(Cell,Cell,Value),
	delete_cell(Cell).

surround_enemy_queen_aux(SourceCell,DestCell,Value):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	not(adjacent_cell(SourceCell,QueenCell)),
	adjacent_cell(QueenCell,DestCell),
	Value = 1,!.
surround_enemy_queen_aux(SourceCell,DestCell,0).


closer_to_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2)),Value):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	init_cell(DestCell),
	closer_to_enemy_queen_aux(DestCell,SourceCell,Value),
	delete_cell(DestCell).

closer_to_enemy_queen(place(Cell),Value):-
	Value = 0.

closer_to_enemy_queen_aux(DestCell,SourceCell,Value):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	hive_distance(DestCell,QueenCell,RD),
	hive_distance(SourceCell,QueenCell,RS),
	RD < RS,!,
	Value = 1.
closer_to_enemy_queen_aux(DestCell,SourceCell,0).
	

