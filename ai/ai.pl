% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
%
% Use reinforeced learning 


% Given a GameState and Color to play
% choose a next Move
run_simulation(Node, Color, Move) :-
    findall(NextMove, get_posible_next_nodes(GameState, Policy, NextMove), NextMoves),
    % create some threads to analyse several path down 
    true.

% Consumes a GameState and produce a next move
get_posible_next_nodes(GameState, Policy, NextMove) :-
    generate_next_game_states(GameState, NextMoves),
    (
        (Policy = uniform, uniform_policy(NextMoves, NextMove));
        (Policy = not_visited, not_visited_policy(NextMoves, NextMove));
        (Policy = uct, uct_policy(NextMoves, NextMove))
    ).

% Unifom policy
uniform_policy(NextMoves, NextMove) :-
    true.

% Uniform policy with heuristics prioritizing unvisited nodes
not_visited_policy(NextMoves, NextMove) :-
    true.

% Using exploitation vs exploration using uct
uct_policy(NextMoves, NextMove) :-
    true.

% Consumes a GameState produces all possible next moves
generate_next_game_states(GameState, NextMoves) :-
    true.

% Upper Confidence Bound
uct(Node, Result) :-
    true.
