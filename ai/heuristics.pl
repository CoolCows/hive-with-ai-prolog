:- module( heuristics, [ 
    apply_heuristics/2
] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").
:- use_module("../game/cell").

apply_heuristics(Move, Value) :-
    %write_ln("HEURISTICS METRICS"),
    %write_ln(Move),
    surround_enemy_queen(Move, H1P),%write_ln('h2'),
    free_enemy_queen(Move, H1N),%write_ln('h3'),
    free_ally_queen(Move, H2P),%write_ln('h4'),
    surround_ally_queen(Move, H2N),%write_ln('h5'),
    closer_to_enemy_queen(Move,H4),%write_ln('h6'),
    place_ants(Move, H5),
    place_bug(Move, H6),
    climb_queen(Move, H7),
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    block_enemy_bug(Move, OpponentColor, H3P),
    block_ally_bug(Move, Color, H3N),
    PreValue is H1P + H1N + H2P + H2N + H4 + H5 + H6 + H7 + H3P + H3N,
    (
        (PreValue > 0.1, Value = PreValue);
        Value = 0.25
    ).
    %write_ln(Value).

surround_enemy_queen(move(QueenCell, SurroundedCell), Value) :-
    current_player_color(Color),
    oponent_color(Color, OpponentColor),
    QueenCell = cell(queen, _, _, OpponentColor, _),
    findall(X, empty_cell(QueenCell, X), SourceEmptyCells),
    findall(Y, empty_cell(SurroundedCell, Y), DestEmptyCells),
    length(SourceEmptyCells, A),
    length(DestEmptyCells, B),
    B < A,
    Value is (6 - B)*0.2.
surround_enemy_queen(move(_,DestCell), 1):-
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    adjacent_cell(DestCell, cell(queen, _, _, OpponentColor, _)).
surround_enemy_queen(place(Cell), 1) :-
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    adjacent_cell(Cell, cell(queen, _, _, OpponentColor, _)).
surround_enemy_queen(_, 0).

free_enemy_queen(move(SourceCell, _), -0.6):-
    get_stack_pos(SourceCell, 0),
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    adjacent_cell(SourceCell, cell(queen, _, _, OpponentColor, _)).
free_enemy_queen(_, 0).

surround_ally_queen(place(Cell), -0.6) :-
    hive_current_player_color(Color),
    adjacent_cell(Cell, cell(queen, _, _, Color, _)).
surround_ally_queen(move(_, DestCell), -0.5) :-
    hive_current_player_color(Color),
    adjacent_cell(DestCell, cell(queen, _, _, Color, _)).
surround_ally_queen(_, 0).

closer_to_enemy_queen(move(cell(B1,R1,C1,D1,S1),cell(_,R2,C2,_,S2)),Value):-
	DestCell = cell(B1,R2,C2,D1,S2),
	SourceCell = cell(B1,R1,C1,D1,S1),
	init_cell(DestCell),
	closer_to_enemy_queen_aux(DestCell,SourceCell,Value),
	delete_cell(DestCell).
closer_to_enemy_queen(_, 0).

closer_to_enemy_queen_aux(DestCell,SourceCell, 0.5):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
    not(adjacent_cell(SourceCell, cell(queen, _, _, OponentColor, _))),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	hive_distance(DestCell,QueenCell,RD),
	hive_distance(SourceCell,QueenCell,RS),
	RD < RS.
closer_to_enemy_queen_aux(_,_,0).
	
free_ally_queen(move(QueenCell, SurroundedCell), Value) :-
    current_player_color(Color),
    QueenCell = cell(queen, _, _, Color, _),
    findall(X, empty_cell(QueenCell, X), SourceEmptyCells),
    findall(Y, empty_cell(SurroundedCell, Y), DestEmptyCells),
    length(SourceEmptyCells, A),
    length(DestEmptyCells, B),
    A < B,
    Value is B*0.2.
free_ally_queen(move(SourceCell, DestCell), 0.5):-
	hive_current_player_color(Color),
    adjacent_cell(SourceCell, cell(queen, _, _, Color, _)),
    not(adjacent_cell(DestCell, cell(queen, _, _, Color, _))).
free_ally_queen(_, 0).

place_ants(place(cell(ant,_,_,_,_)), 0.05) :-
    hive_current_player_turns(T),
    T > 8.
place_ants(_, 0).

place_bug(place(_), Value) :-
    hive_current_player_turns(T),
    Value is 0.25 + T/30.
place_bug(_, 0).

climb_queen(move(cell(beetle,_,_,_,_), cell(none,Row,Col,none,Stack), 0.45)) :-
    Stack > 0,
    hive_current_player_color(Color),
    oponent_color(Color, OpponentColor),
    get_cell(cell(queen, Row, Col, OpponentColor, 0)).
climb_queen(_, 0).

block_enemy_bug(move(_, DestCell), Color, 0.5) :-
   findall(AdyCell, not_empty_cell(DestCell, AdyCell), [Cell]),
   Cell = cell(_, _, _, Color, 0),
   write_ln('bem').
block_enemy_bug(place(cell(_, Row, Col, _, _)), Color, 0.5) :-
   findall(AdyCell, not_empty_cell(cell(none, Row, Col, none, 0), AdyCell), [Cell]),
   Cell = cell(_, _, _, Color, 0),
   write_ln('bep').
block_enemy_bug(X,_, 0) :-
    write_ln(X).

block_ally_bug(move(_, DestCell), Color, -0.3) :-
   findall(AdyCell, not_empty_cell(DestCell, AdyCell), [Cell]),
   Cell = cell(_, _, _, Color, 0),
   write_ln('bam').
block_ally_bug(place(cell(_, Row, Col, _, _)), Color, -0.3) :-
   findall(AdyCell, not_empty_cell(cell(none, Row, Col, none, 0), AdyCell), [Cell]),
   Cell = cell(_, _, _, Color, 0),
   write_ln('bap').
block_ally_bug(_, _, 0).

empty_cell(Cell, AdyCell):-
    adjacent_cell(Cell, AdyCell),
    AdyCell = cell(none, _, _, none, _).
not_empty_cell(Cell, AdyCell) :-
    adjacent_cell(Cell, AdyCell),
    not(AdyCell = cell(none, _, _, none, _)).
