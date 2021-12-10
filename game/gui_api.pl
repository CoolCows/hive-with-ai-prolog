:- module(gui_api,[
    gui_start_game/3,
    gui_put_cell/3,
    gui_move_cell/3,
    gui_all_possible_moves/2,
    gui_get_possible_moves/2,
    gui_get_possible_positions/2,
	gui_get_pillbug_effect/2,
    gui_get_board/1,
    gui_test_board/1,
	gui_mosquito_adyacent_pillbug/1,
    gui_change_game_state/1,
    gui_get_visual_game_state/2,
    gui_ai_turn/0,
    gui_ai_turn/1,
    gui_ai_vs_ai_visual/2,
    gui_game_status/1
]).

:- use_module(player).
:- use_module(cell).
:- use_module("movements/movements").
:- use_module(board).
:- use_module(turns).
:- use_module(hive_api).
:- use_module("../ai/ai_api", [
    ai_vs_human_init/0,
    ai_vs_ai_init/1,
    ai_vs_human/0,
    ai_vs_human/1,
    ai_vs_ai_visual/2
]).

gui_start_game(+Opponent, -Board, -Players) :-
    (
        (Opponent = aiw, ai_vs_human_init);
        (Opponent = aib, ai_vs_human_init);
        (Opponent = ai_vs_ai_visual, ai_vs_ai_init(Node), nb_setval(last_node, Node));
        true
    ),
	init_turns,
	hive_init_players(),
    players(Players),
    cells(Board).

gui_get_board(-Board) :-
    cells(Board).

gui_test_board(-Board) :-
    cells(Board).

gui_put_cell(+Cell, -Board, -NewPlayer) :-
	hive_put_cell(Cell),
    get_color(Cell, Color),
    get_player(player(Color,_,_,_,_,_,_,_,_), NewPlayer),
    cells(Board).

gui_move_cell(+SourceCell, +DestCell, -Board) :-
    % Tries to move cell to a certain location
    % Returns the new board if succesful
	hive_move_cell(SourceCell,DestCell),
    cells(Board).

gui_all_possible_moves(Color, Moves) :-
    hive_possible_plays(Color, Moves).

gui_get_possible_moves(+Cell, -Board) :-
    % Get all Cells where a bug can be moved
    % return the board with possible positions.
    % Possible position cells has color = bug = none
	hive_get_possible_moves(Cell,PosMoves),
    cells(Cells),
    append(PosMoves, Cells, Board).

gui_get_possible_positions(+Color, -Board) :-
    % Get all Cells where a bug by certain player can be put
    % return the board with possible positions.
    % Possible position cells has color = bug = none
	hive_get_possible_positions(Color,PosPositions),
	cells(Cells),
    append(PosPositions, Cells, Board).

gui_get_pillbug_effect(+PillbugCell, -[MovableBugs, MovablePositions]) :-
    % call to method that with pillbug cells return 
    % a list of MovableBugs
 	hive_get_pillbug_effect(PillbugCell,[MovableBugs,MovablePositions]).

gui_mosquito_adyacent_pillbug(+MosquitoCell) :-
	hive_mosquito_adjacent_pillbug(MosquitoCell).

gui_change_game_state(MoveType) :-
    hive_change_game_state(MoveType).

gui_get_visual_game_state(Board, [BlackPlayer, WhitePlayer]) :-
    get_game_state(game_state(Board, _, _, _, [Player1, Player2])),
    (
        (
            Player1 = player(black, _, _, _, _, _, _, _, _), 
            BlackPlayer = Player1, 
            WhitePlayer = Player2
        );
        (
            BlackPlayer = Player2, 
            WhitePlayer = Player1
        )
    ).

gui_ai_turn() :-
    ai_vs_human().
gui_ai_turn(MoveType) :-
    ai_vs_human(MoveType).

gui_ai_vs_ai_visual(Node, NextNode) :-
    ai_vs_ai_visual(Node, NextNode).

gui_skip_turn():-
	hive_skip_turn().

gui_game_status(Status):-
	hive_game_status(Status).
