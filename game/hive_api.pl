:- module(hive_api,[
    hive_init_board/0,
    hive_init_players/0,
    hive_put_cell/1,
    hive_move_cell/2,
    hive_get_possible_moves/2,
    hive_get_possible_positions/2,
	hive_get_pillbug_effect/2,
	hive_skip_turn/0,
	hive_force_skip_turn/0,
	hive_mosquito_adjacent_pillbug/1,
	hive_game_status/1,
	hive_current_player_color/1,
	hive_get_cell/2,
	hive_get_game_state/1,
	hive_set_game_state/1,
    hive_change_game_state/1,
	hive_get_player/2,
	hive_current_player_turns/1,
	hive_possible_plays/2,
	hive_distance/3
]).
:- use_module(player).
:- use_module(cell).
:- use_module("movements/movements").
:- use_module(board).
:- use_module(turns).

hive_get_game_state(GameState):-
	get_game_state(GameState).

hive_set_game_state(GameState):-
	set_game_state(GameState).

hive_init_board() :-
	init_turns.

hive_init_players() :-
    init_player(white),
    init_player(black).

hive_change_game_state(MoveType) :-
    (MoveType = place(Cell), add_new_cell(Cell));
    (MoveType = move(SourceCell, DestCell), move_cell(SourceCell, DestCell));
    (MoveType = pillbug(SourceCell, DestCell), pillbug_move(SourceCell, DestCell));
    (MoveType = skip_move, hive_force_skip_turn()).

hive_put_cell(Cell):-
    add_new_cell(Cell).

hive_move_cell(SourceCell, DestCell) :-
	move_cell(SourceCell,DestCell).

hive_get_possible_moves(Cell, PosMoves) :-
	get_color(Cell,Color),
	get_cell(cell(queen,_,_,Color,_),_),
    valid_movements(Cell, PosMoves).

hive_get_possible_positions(Color,PosPositions) :-
    bagof(ValidCell, valid_new_cell(Color, ValidCell), PosPositions).

hive_get_pillbug_effect(PillbugCell, [MovableBugs, MovablePositions]) :-
	get_color(PillbugCell,Color),
	get_cell(cell(queen,_,_,Color,_),_),
	movable_cells_by_pillbug(PillbugCell,MovableBugs),
    movable_positions_by_pillbug(PillbugCell, MovablePositions).

hive_mosquito_adjacent_pillbug(MosquitoCell) :-
    mosquito_adjacent_to_pillbug(MosquitoCell).

hive_skip_turn():-
	current_player_color(Color),
	hive_possible_plays(Color,[]),
	increase_turns(),
	remove_expired_fixed_cells().

hive_possible_plays(Color,PossiblePlays):-
	findall(Move,possible_moves(Color,Move), Moves),
	findall(Place, possible_place(Color,Place), Places),
	findall(PillbugEffectMove, pillbug_effect_move(Color,PillbugEffectMove), PillbugEffectMoves),
	append(Moves,Places, A),
	append(A,PillbugEffectMoves,PossiblePlays).

possible_moves(Color,move(SourceCell,DestCell)):-
	hive_get_cell(cell(_,_,_,Color,_),SourceCell),
	hive_get_possible_moves(SourceCell,PosMoves),
	member(DestCell,PosMoves).

possible_place(Color,place(Cell)):-
	hive_current_player_turns(3),
	hive_get_player(player(Color,_, _, _, _, _, _, _, _),
					player(Color,Queen, _, _, _, _, _, _, _)),
	Queen = 1,	
	hive_get_possible_positions(Color,PosPositions),!,
	member(cell(_,Row,Col,_,_),PosPositions),
	Cell = cell(queen,Row,Col,Color,0).

possible_place(Color,place(Cell)):-
	hive_get_player(player(Color,_, _, _, _, _, _, _, _),
					player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),
	hive_get_possible_positions(Color,PosPositions),
	member(cell(_,Row,Col,_,_),PosPositions),
	(
		(
			Queen > 0,
			Cell = cell(queen,Row,Col,Color,0) 
		);
		(
			Ants > 0,
			Cell = cell(ant,Row,Col,Color,0) 
		);
		(
			Beetle > 0,
			Cell = cell(beetle,Row,Col,Color,0) 
		);
		(
			Grasshopper > 0,
			Cell = cell(grasshopper,Row,Col,Color,0) 
		);
		(
			Ladybug > 0,
			Cell = cell(ladybug,Row,Col,Color,0) 
		);
		(
			Mosquito > 0,
			Cell = cell(mosquito,Row,Col,Color,0) 
		);
		(
			Pillbug > 0,
			Cell = cell(pillbug,Row,Col,Color,0) 
		);
		(
			Spider > 0,
			Cell = cell(spider,Row,Col,Color,0) 
		)
	).

pillbug_effect_move(Color, PillbugEffectMove) :-
	get_cell(cell(pillbug,_,_,Color,_), PillbugCell),
    hive_get_pillbug_effect(PillbugCell, [MovableBugs, MovablePositions]),
	member(SourceCell,MovableBugs),
	member(DestCell,MovablePositions),
	PillbugEffectMove = move(SourceCell,DestCell).

pillbug_effect_move(Color, PillbugEffectMove) :-
	get_cell(cell(mosquito,_,_,Color,_), MosquitoCell),
	hive_mosquito_adjacent_pillbug(MosquitoCell),
	get_color(MosquitoCell, Color),
    hive_get_pillbug_effect(MosquitoCell, [MovableBugs, MovablePositions]),
	member(SourceCell,MovableBugs),
	member(DestCell,MovablePositions),
	PillbugEffectMove = move(SourceCell,DestCell).

hive_force_skip_turn() :-
    increase_turns(),
    remove_expired_fixed_cells().


hive_game_status(Status):-
	game_status(Status).

hive_current_player_color(Color):-
	current_player_color(Color).

hive_get_cell(cell(Bug, Row, Col, Color, StackPos), cell(Bug, Row, Col, Color, StackPos)):-
	get_cell(cell(Bug, Row, Col, Color, StackPos), cell(Bug, Row, Col, Color, StackPos)).

hive_get_player(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		   player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)):-
	get_player(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		   player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)
	).

hive_current_player_turns(P):-
	current_player_turns(P).
	
hive_distance(cell(_,_,C1,_,_),cell(_,_,C2,_,_),R):-
	R is  abs(C1 - C2).
