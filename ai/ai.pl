% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 
:- module(ai, [
    run_simulation/2,
    explore_node/4,
    backpropagate/2
]).

:- use_module(ai_api, [
    ai_current_player_color/1,
    ai_game_status/1,
    ai_get_game_state/1,
    ai_set_game_state/1,
    ai_change_game_state/1
]).
:- use_module("../game/hive_api").
:- use_module(total_visits, [increase_total_visits/0, get_total_visits/1]).
:- use_module(heuristics).
:- use_module(node).

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    write_ln('End Case Simulation').
run_simulation(
    Node,
    NextNode
) :-
    write_ln('Running simulation'),
    get_address(Node, Address),
    ai_get_game_state(RealGameState),
    get_next_moves(AllPosMoves),
    analyze_moves(Address, AllPosMoves, 0, [], BestMoves),
    do_searches(Address, RealGameState, BestMoves, 2),
    write_ln('Do searches completed'),
  
    % ===== Multi-Threading (for later) =====
    % Make the dynamic predicates thread independent
    % Use mutexes to when writing to database
    % create some threads to analyse several path down 
    % =====          End                =====
    
    select_next_move(Address, AllPosMoves, FinalNextMove),
    write_ln('Final Next Move'),
    write_ln(FinalNextMove),
    explore_node(Address, FinalNextMove, true, NextNode),
    ai_game_status(Status),
    (
        (not(Status = non_terminal), backpropagate(NextNode, Status));
        true
    ),
    write_ln('Simulation Ended').

do_searches(_, _, _, 0):-
    write_ln('do searches done').
do_searches(_, _, [], _).
do_searches(Address, RealGameState, [BestMove|OtherMoves], Amount) :-
    explore_node(Address, BestMove, Node),
    write_ln('\n--------------------Doing Search--------------------'),
    search(Node, EndNode),
    increase_total_visits,
    ai_game_status(Status),
    write_ln('Back Propagating starting from:'),
    write_ln(EndNode),
    backpropagate(EndNode, Status),
    write_ln('EndNOde updated is:'),
    get_address(EndNode, NA),
    find_node_by_game_state(NA, NEN),
    write_ln(NEN),
    write_ln('Back Propagation Complete'),
    sync_tree_db,
    
    write_ln('--------------------Ending Search--------------------\n'),
    DecAmount is Amount - 1,
    ai_set_game_state(RealGameState),
    do_searches(Address, RealGameState, OtherMoves, DecAmount).
  

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    write_ln('Search Completed').
search(
    Node,
    EndNode
) :-
    write_ln('===========Search=========='),
    get_address(Node, Address),
    select_next_move(Address, NextMove),
    explore_node(Address, NextMove, false, NewNode),
    search(NewNode, EndNode).

explore_node(Address, NextMove, Node) :-
    explore_node(Address, NextMove, false, Node).
explore_node(Address, NextMove, Visited, Node) :-
    ai_change_game_state(NextMove),
    ai_get_game_state(GameState),
    ai_game_status(NodeType),
    force_find_node(Address, GameState, NextMove, NodeType, Visited, Node).

backpropagate(_, non_terminal):-
    write_ln('backpropagation ended because of non terminal status').
backpropagate(Node, EndNodeType) :-
    get_parent_address(Node, '0'),!,
    write_ln('backpropagation final step'),
    update_node(Node, EndNodeType).
backpropagate(Node, EndNodeType) :-
    update_node(Node, EndNodeType),
    get_parent_address(Node, ParentGameStateAddress),
    find_node_by_game_state(ParentGameStateAddress, ParentNode),
    backpropagate(ParentNode, EndNodeType).

select_next_move(Address, NextMove) :-
    %write_ln('Selecting Next Move'),
    get_next_moves(NextMoves), 
    select_next_move(Address, NextMoves, NextMove).
select_next_move(Address, NextMoves, NextMove) :-
    %write_ln('Analyzing Next Moves'),
    %write_ln(Address),
    analyze_moves(Address, NextMoves, 0, [], [NextMove|_]),
    true.

analyze_moves(_, [], MaxValue, BestMoves, BestMoves).
    %write_ln('Analyzed all moves. Max Value'),
    %write_ln(MaxValue),
    %write_ln(BestMoves).
analyze_moves(Address, [Move|NextMoves], MaxValue, TopMoves, BestMoves) :-
    keccak256(Address, Move, AuxAddress),
    (
        (
            %write_ln('AM0'),
            find_node_by_edge_move(AuxAddress, Node),
            %write_ln('AM1'),
            %write_ln('Analyzing Next Moves'),
            uct(Node, Move, NewValue)%,
            %write_ln('AM2'),
            %write_ln('New Value of Explored Node'),
            %write_ln(NewValue)
        );
        (
            % Call to Heuristics and Multiply for constant Value
            %write_ln('AM3'),
            apply_heuristics(Move, C),
            get_total_visits(TotalVisits),
            %write_ln(TotalVisits),
            %write_ln('AM4'),
            NewValue is C*sqrt(TotalVisits)%,
            %write_ln('New Value of Unexplored Node'),
            %write_ln(NewValue)
        )
    ),
    % write_ln(NewValue),
    % write_ln(MaxValue),
    (
        (
            NewValue > MaxValue,!, 
            write_ln('Found Best Move'),
            write_ln(Move),
            analyze_moves(Address, NextMoves, NewValue, [Move], BestMoves)
        );
        (
            NewValue =:= MaxValue,!,
            analyze_moves(Address, NextMoves, MaxValue, [Move|TopMoves], BestMoves)
        );
        analyze_moves(Address, NextMoves, MaxValue, TopMoves, BestMoves)
    ).

% Get all possible moves
get_next_moves(NextMoves) :-
	hive_current_player_color(Color),
 	hive_possible_plays(Color, Moves),
	(
		(Moves = [],NextMoves = [skip_move]);
		NextMoves = Moves
	).


% Upper Confidence Bound
uct(Node, Move, Result) :-
    %write_ln('UCT0'),
    ai_current_player_color(Color),
    (
        (Color = white, get_stats(Node, Explored, _, TimesWon));
        get_stats(Node, Explored, TimesWon, _)
    ),
    %write_ln('UCT1'),
	apply_heuristics(Move, C),
    %write_ln('UCT2'),
    get_total_visits(TotalVisits),
    %write_ln('UCT3'),
    %write_ln(Explored),
    Result is TimesWon/(Explored + 1) + C*sqrt(TotalVisits)/(Explored + 1).
    %write_ln(Result).
