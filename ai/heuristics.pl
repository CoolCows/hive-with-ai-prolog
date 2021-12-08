:- module( heuristic, [ surround_queen/1 ] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").


surround_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2))):-
	DestCell = cell(B1,R2,C2,D1,S2),
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	adjacent_cell(QueenCell,DestCell).

surround_enemy_queen(place(Cell)):-
	% hive_current_player_color(Color),
	% oponent_color(Color,OponentColor),
	% hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	% adjacent_cell(QueenCell,Cell).
	false.

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
	gui_get_possible_moves(AdjCell,[]),
	delete_cell(DestCell),
	init_cell(SourceCell).

block_enemy_bug(place(Cell)):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	init_cell(Cell),
	adjacent_cell(Cell,AdjCell),
	get_color(AdjCell,OponentColor),
	gui_get_possible_moves(AdjCell,[]),
	delete_cell(Cell).

