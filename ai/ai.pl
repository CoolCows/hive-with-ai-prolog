% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)).
run_simulation(Node, NextNode) :-
    % For multi-threading (for later)
    % findall(NextMove, get_posible_next_nodes(GameState, Policy, NextMove), NextMoves),
    % create some threads to analyse several path down 
    true.

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!.
search(
    node(Address, _, GameState, Type, Visited, Explored, WhiteWon, BlackWon),
    EndNode
) :-
    select_next_move(Address, GameState, NextMoves, NextMove),
    % Create new node with new Game State
    true.

% Get all possible moves
get_next_moves(GameState, NextMoves) :-
    true.

select_next_move(Address, GameState, NextMove) :-
    get_next_moves(GameState, NextMoves),
    analyze_moves(Address, GameState, NextMoves, -2^64, [], [NextMove|_]),
    true.

analyze_moves(_, _, [], _, BestMoves, BestMoves).
analyze_moves(Address, GameState, [Move|NextMoves], MaxValue, TopMoves, BestMoves) :-
    keccak256(Address, Move, AuxAddress),
    (
        (
            find_node_by_edge_move(AuxAddress, Node),
            uct(Node, NewValue)
        );
        (
            NewValue is sqrt(TotalVisits)
            % Call to Heuristics and Multiply for constant Value
        )
    ),
    (
        (
            NewValue > MaxValue,!, 
            analyze_move(Address, GameState, NextMoves, NewValue, [Move], BestMoves)
        );
        (
            NewValue =:= MaxValue,!,
            analyze_move(Address, GameState, NextMoves, MaxValue, [Move|TopMoves], BestMoves)
        );
        analyze_move(Address, GameState, NextMoves, MaxValue, TopMoves, BestMoves)
    ).

% Upper Confidence Bound
uct(Node, Result) :-
    true.
