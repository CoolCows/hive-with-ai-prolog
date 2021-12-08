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
