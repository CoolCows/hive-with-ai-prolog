:- module(ai, [
    ai_vs_human_init/0,
    ai_vs_human/3,
    ai_vs_ai_init/1,
    ai_get_game_state/1,
    ai_set_game_state/1,
    ai_update_state/2
]).

:- use_module("../game/hive_api").
:- use_module(node). 

ai_vs_human_init :-
    add_initial_node,
    nb_setval(parent_address, 0).

ai_vs_human(GameState, EdgeMove, NewGameState) :-
    nb_getval(parent_address, ParentAddress),
    hive_game_status(non_terminal),
    force_find_node(ParentAddress, GameState, EdgeMove, GameStatus, Node),
    ai_play(Node, node(Address, _, _, NewGameState, _, _, _, _, _)),
    nb_setval(parent_address, Address).

ai_vs_ai_init(EndNode) :-
    % Init Game State
    add_initial_node,
    find_node_by_game_state(1, Node),
    ai_vs_ai(Node, EndNode).

ai_vs_ai(Node, EndNode) :-
    get_type(Node, non_terminal),
    ai_play(Node, NewNode),
    ai_vs_ai(NewNode, EndNode).

ai_play(Node, NewNode) :-
    true.

ai_get_game_state(State) :-
    hive_get_game_state(State).

ai_set_game_state(State) :-
    hive_set_game_state(State).

ai_update_state(NewState, OldState) :-
    hive_get_game_state(OldState),
    hive_set_game_state(NewState).

ai_current_player_color(Color) :-
    hive_current_player_color(Color).
