:- module(hive_api,[
    hive_init_board/0,
    hive_init_players/0,
    hive_put_cell/1,
    hive_move_cell/2,
    hive_get_possible_moves/2,
    hive_get_possible_positions/2,
	hive_get_pillbug_effect/2,
	hive_skip_turn/0,
	hive_mosquito_adyacent_pillbug/1,
	hive_game_status/1,
	hive_current_player_color/1,
	hive_get_cell/2,
	hive_get_game_state/1,
	hive_set_game_state/1,
    hive_change_game_state/1,
	hive_get_player/2
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
    (MoveType = pillbug(SourceCell, DestCell), move_cell(SourceCell, DestCell)).


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

hive_mosquito_adyacent_pillbug(MosquitoCell) :-
    mosquito_adyacent_to_pillbug(MosquitoCell).

hive_skip_turn():-
	current_player_color(Color),
	cells(Cells),
	forall(member(cell(Bug,Row,Col,Color,StackPos),Cells),
		  not(hive_get_possible_moves(cell(Bug,Row,Col,Color,StackPos),_))),
	not(hive_get_possible_positions(Color,_)),
	increase_turns().

hive_current_player_color(Color):-
    current_player_color(Color).

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

	
