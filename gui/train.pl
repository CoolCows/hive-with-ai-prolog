:- use_module("../ai/ai_api").
:- use_module("../ai/node").
:- use_module("../game/hive_api").


train :-
    %Intial Setup
    start_up_ia,
    add_initial_node,
    hive_init_players(),
    hive_init_board(),

    %Run simulation
    find_node_by_game_state('1', RootNode),
    ai_vs_ai(RootNode, _),
    hive_game_status(Status),
    write_to_file(Status),
    halt.

write_to_file(Status):-
    open('./log.txt', append, File),
    write(File, Status),
    close(File).

?- train.
