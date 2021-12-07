:- module(hive_api,[
	virtual_hive_put_cell/3,
	virtual_hive_move_cell/4,
	virtual_hive_get_possible_moves/3,
	virtual_hive_get_possible_positions/3,
	virtual_hive_get_pillbug_effect/3,
	virtual_hive_mosquito_adyacent_pillbug/2
]).

:- use_module(board).
:- use_module(hive_api).


virtual_hive_put_cell(VirtualGameState,Cell,NewVirtualGameState):-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_put_cell(Cell),
	get_game_state(NewVirtualGameState),
	set_game_state(RealGameState).

virtual_hive_move_cell(VirtualGameState,SourceCell, DestCell,NewVirtualGameState) :-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_move_cell(SourceCell,DestCell),
	get_game_state(NewVirtualGameState),
	set_game_state(RealGameState).

virtual_hive_get_possible_moves(VirtualGameState,Cell, PosMoves) :-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_get_possible_moves(Cell,PosMoves),
	set_game_state(RealGameState).

virtual_hive_get_possible_positions(VirtualGameState,Color,PosPositions) :-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_get_possible_positions(Color,PosPositions),
	set_game_state(RealGameState).

virtual_hive_get_pillbug_effect(VirtualGameState,PillbugCell, [MovableBugs, MovablePositions]) :-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_get_pillbug_effect(PillbugCell,[MovableBugs, MovablePositions]),
	set_game_state(RealGameState).

virtual_hive_mosquito_adyacent_pillbug(VirtualGameState,MosquitoCell) :-
	get_game_state(RealGameState),
	set_game_state(VirtualGameState),
	hive_mosquito_adyacent_pillbug(MosquitoCell),
	set_game_state(RealGameState).

