% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 

:- use_module(ai_api, [ai_current_player_color/1]).

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)).
run_simulation(Node, NextNode) :-
    % For multi-threading (for later)
    % findall(NextMove, get_posible_next_nodes(GameState, Policy, NextMove), NextMoves),
    % create some threads to analyse several path down 
    true.

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    ai_current_player_color(Color),
    backpropagate(Node, Color).
search(
    node(Address, _, GameState, Type, Visited, Explored, WhiteWon, BlackWon),
    EndNode
) :-
    select_next_move(Address, GameState, NextMoves, NextMove),
    % Create or find the node with new Game State representing the new move done
    % Repeat the search with the new Node
    true.

backpropagate(Node, Color) :-
    get_parent_address(Node, 0),!,
    update_node(Node, Color).
backpropagate(Node, Color) :-
    update_node(Node, Color),
    get_parent_address(Node, ParentGameStateAddress),
    find_node_by_game_state(ParentGameStateAddress, ParentNode),
    backpropagate(ParentNode, Color).

select_next_move(Address, GameState, NextMove) :-
    get_next_moves(GameState, NextMoves),
    % Set Game State
    analyze_moves(Address, GameState, NextMoves, -2^64, [], [NextMove|_]),
    true.

% Get all possible moves
get_next_moves(GameState, NextMoves) :-
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
            % Call to Heuristics and Multiply for constant Value
            NewValue is sqrt(TotalVisits)
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
uct(node(_, _, _, _, Explored, WhiteWon, BlackWon), Result) :-
    ai_current_player_color(Color),
    (
        (Color = white, TimesWon = WhiteWon);
        TimesWon = BlackWon 
    ),
    % Apply Heuristics
    Result is TimesWon/Explored + sqrt(TotalVisits)/Explored.
