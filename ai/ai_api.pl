:- module(ai_api, [
    ai_vs_human_init/0,
    ai_vs_ai_init/1,
    ai_vs_human/1,
    ai_vs_ai_visual/2,
    ai_vs_ai/2,
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
    write_ln('ai_vs_human init'),
    start_up_ia,
    add_initial_node,
    nb_setval(parent_address, '1').

ai_vs_ai_init(RootNode):-
    write_ln('ai_vs_ai init'),
    start_up_ia,
    add_initial_node, 
    find_node_by_game_state('1', RootNode).

ai_vs_human(_) :-
    not(hive_game_status(non_terminal)).
ai_vs_human(EdgeMove) :-
    write_ln('Playing Ai vs human'),
    nb_getval(parent_address, ParentAddress),
    explore_node(ParentAddress, EdgeMove, true, Node),
    ai_play(Node, ResultNode, 2, play),
    get_address(ResultNode, Address),
    nb_setval(parent_address, Address),
    write_ln('Ai ended its turn').

ai_vs_ai_visual(Node, NextNode) :-
    ai_play(Node, NextNode, 2, train).

ai_vs_ai(Node, Node, _, _) :-
    not(get_type(Node, non_terminal)).
ai_vs_ai(Node, EndNode) :-
    ai_play(Node, NewNode, SearchTimes, PlayOrTrain),
    ai_vs_ai(NewNode, EndNode, SearchTimes, PlayOrTrain).

ai_play(Node, Node, _, _) :-
    not(get_type(Node, non_terminal)),
    ai_game_status(Status),
    backpropagate(Node, Status).
ai_play(Node, NewNode, SearchTimes, PlayOrTrain) :-
    run_simulation(Node, NewNode, SearchTimes, PlayOrTrain).

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
