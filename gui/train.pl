:- use_module("../ai/ai.pl").
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
    ai_vs_ai(RootNode, _, 4, train),
    hive_game_status(Status),
    write_to_file(Status),
    halt.

stamina :-
    start_up_ia,
    add_initial_node,
    hive_init_players(),
    hive_init_board(),

    find_node_by_game_state('1',RootNode),
    run_simulation(RootNode, N1, 8, train),
    run_simulation(N1, N2, 8, train),
    run_simulation(N2, N3, 8, train),
    run_simulation(N3, N4, 8, train),
    halt.

write_to_file(Status):-
    open('./log.txt', append, File),
    string_concat("\n", Status, Result),
    write(File, Result),
    close(File).

?- stamina.
