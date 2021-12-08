% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 
:- module(ai, [run_simulation/2]).

:- use_module(ai_api, [
    ai_current_player_color/1,
    ai_update_state/2
]).
:- use_module(total_visits, [increase_total_visits/0]).
:- use_module(heuristics).

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)).
run_simulation(
    Node,
    NextNode
) :-
    get_address(Node, Address),
    ai_get_game_state(RealGameState),
    select_next_move(Address, NextMove),  
    % ===== Multi-Threading (for later) =====
    % Make the dynamic predicates thread independent
    % Use mutexes to when writing to database
    % create some threads to analyse several path down 
    % =====          End                =====

    % After simulation select cells to move
    ai_set_game_state(RealGameState),
    select_next_move(Address, NextMove),
    update_game_state(NextMove),
    get_game_state(NewGameState),
    ai_game_status(NodeType),
    force_find_node(Address, NewGameState, NextMove, NodeType, true, NextNode).

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    ai_current_player_color(Color),
    increase_total_visits,
    backpropagate(Node, Color).
search(
    node(Address, _, _, _, _, _, Explored, WhiteWon, BlackWon),
    EndNode
) :-
    select_next_move(Address, NextMove),
    update_game_state(NextMove),
    get_game_state(NewGameState),
    ai_game_status(NodeType),
    force_find_node(Address, NewGameState, NextMove, NodeType, Node),
    search(Node, EndNode).

backpropagate(Node, Color) :-
    get_parent_address(Node, 0),!,
    update_node(Node, Color).
backpropagate(Node, Color) :-
    update_node(Node, Color),
    get_parent_address(Node, ParentGameStateAddress),
    find_node_by_game_state(ParentGameStateAddress, ParentNode),
    backpropagate(ParentNode, Color).

select_next_move(Address, NextMove) :-
    get_next_moves(NextMoves), 
    analyze_moves(Address, NextMoves, -2^64, [], [NextMove|_]),
    true.

analyze_moves(_, [], _, BestMoves, BestMoves).
analyze_moves(Address, [Move|NextMoves], MaxValue, TopMoves, BestMoves) :-
    keccak256(Address, Move, AuxAddress),
    (
        (
            find_node_by_edge_move(AuxAddress, Node),
            uct(Node, Move, NewValue)
        );
        (
            % Call to Heuristics and Multiply for constant Value
            apply_heuristics(Move, C),
            total_visits(TotalVisits),
            NewValue is C*sqrt(TotalVisits)
        )
    ),
    (
        (
            NewValue > MaxValue,!, 
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
    ai_current_player_color(Color),
    true.
    % Get all bug in board of certain color
    % Get all bug of player of certain color
    % make a super list of all possible positionings 

update_game_state(NextMove) :-
    (
        NextMove = place(Cell),
        ai_put_cell(Cell)
    );
    (
        NextMove = move(SourceCell, DestCell),
        ai_move_cell(SourceCell, DestCell)
    ).

% Upper Confidence Bound
uct(Node, Move, Result) :-
    ai_current_player_color(Color),
    (
        (Color = white, get_stats(Node, TimesWon, _, Explored));
        get_stats(Node, _, TimesWon, Explored)
    ),
    apply_heuristics(Move, C),
    total_visits(TotalVisits),
    Result is TimesWon/Explored + C*sqrt(TotalVisits)/Explored.
