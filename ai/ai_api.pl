:- module(ai_api, [
    ai_vs_human_init/0,
    ai_vs_human/1,
    ai_vs_ai_init/1,
    ai_get_game_state/1,
    ai_set_game_state/1,
    ai_game_status/1,
    ai_current_player_color/1,
    ai_change_game_state/1
]).

:- use_module("../game/hive_api").
:- use_module(node). 
:- use_module(ai).

ai_vs_human_init :-
    start_up_ia,
    add_initial_node,
    nb_setval(parent_address, '1').


ai_vs_human(_) :-
    not(hive_game_status(non_terminal)).
ai_vs_human(EdgeMove) :-
    write_ln('Playing Ai vs human'),
    nb_getval(parent_address, ParentAddress),
    ai_change_game_state(EdgeMove),
    ai_get_game_state(GameState),
    ai_game_status(NodeType),
    force_find_node(ParentAddress, GameState, EdgeMove, NodeType, Node),
    ai_play(Node, node(Address, _, _, _, _, _, _, _)),
    nb_setval(parent_address, Address),
    write_ln('Ai ended its turn').

ai_vs_ai_init(EndNode) :-
    % Init Game State
    add_initial_node,
    find_node_by_game_state('1', Node),
    ai_vs_ai(Node, EndNode).

ai_vs_ai(Node, Node) :-
    not(get_type(Node, non_terminal)).
ai_vs_ai(Node, EndNode) :-
    ai_play(Node, NewNode),
    ai_vs_ai(NewNode, EndNode).

ai_play(Node, NewNode) :-
    run_simulation(Node, NewNode).

ai_put_cell(Cell) :-
    hive_put_cell(Cell).

ai_move_cell(SourceCell, DestCell) :-
    hive_move_cell(SourceCell, DestCell).

ai_get_game_state(State) :-
    hive_get_game_state(State).

ai_set_game_state(State) :-
    hive_set_game_state(State).

ai_change_game_state(MoveType) :-
    hive_change_game_state(MoveType).

ai_game_status(Status) :-
    hive_game_status(Status).

ai_current_player_color(Color) :-
    hive_current_player_color(Color).
