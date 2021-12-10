:- module( heuristics, [ 
    apply_heuristics/2
] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").
:- use_module("../game/cell").

apply_heuristics(Move, Value) :-
    write_ln("HEURISTICS METRICS"),
	surround_enemy_queen(Move, H1P),
    free_enemy_queen(Move, H1N),
    free_ally_queen(Move, H2P),
    surround_ally_queen(Move, H2N),
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    block_bug(Move, OpponentColor, H3P),
    block_bug(Move, Color, H3N),
    PreValue is H1P + H1N + H2P + H2N + H3P - H3N,
    (
        (PreValue > 0, Value = PreValue);
        Value = 0.2
    ),
	write_ln(Value).

surround_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(B2,R2,C2,D2,S2)),Value):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	init_cell(DestCell),
	surround_enemy_queen_aux(SourceCell,DestCell,Value),
	delete_cell(DestCell).
surround_enemy_queen(_, 0).

surround_enemy_queen_aux(SourceCell,DestCell,Value):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	not(adjacent_cell(SourceCell,QueenCell)),
	adjacent_cell(QueenCell,DestCell),
	Value = 1,!.
surround_enemy_queen_aux(_, 0).

free_enemy_queen(move(SourceCell, _), -0.7):-
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    adjacent_cell(SourceCell, cell(queen, _, _, OpponentColor, _)).
free_enemy_queen(_, 0).

surround_ally_queen(place(Cell), -0.7) :-
    hive_current_player_color(Color),
    adjacent_cell(Cell, cell(queen, _, _, Color, _)).
surround_ally_queen(move(_, DestCell), -0.7) :-
    hive_current_player_color(Color),
    adjacent_cell(DestCell, cell(queen, _, _, Color, _)).
surround_ally_queen(_, 0).

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
	
free_ally_queen(move(SourceCell, DestCell), 0.5):-
	hive_current_player_color(Color),
    adjacent_cell(SourceCell, cell(queen, _, _, Color, _)),
    not(adjacent_cell(DestCell, cell(queen, _, _, Color, _))).
free_ally_queen(_, 0).

block_bug(move(SourceCell, DestCell), Color, Value):-
	delete_cell(SourceCell),
	init_cell(DestCell),
	block_bug_aux(DestCell, Color, Value),
	delete_cell(DestCell),
	init_cell(SourceCell),
    Cond = [].
block_bug(_, _, 0).
block_bug_aux(DestCell, Color, 0.5) :-
	adjacent_cell(DestCell,AdjCell),
	get_color(AdjCell, Color),
	hive_get_possible_moves(AdjCell, []).
block_bug_aux(_, _, 0).

