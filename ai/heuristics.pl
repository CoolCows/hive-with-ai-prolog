% get_game_state(
%   game_state(
%       Board:list,
%       Turns,
%       LastMovedCells,
%       FixedCells,
%       Players
%)).

:- use_module("../game/virtual_hive_api").

% Import adyacent cells
% Import cells movement

surround_queen(
    game_state(Board, Turns, LastMovedCells, FixedCells, Players),
    NextGameState) 
:-
    % Get Color
    member(
        cell(queen, Row, Col, _, _), 
        Board
    ),
    AdyCell = cell(none, AdyRow, AdyCol, none, 0),
    adjacent_cell(Board, cell(queen, Row, Col, _, _), AdyCell),
    % Get Opposite Color
    % Get bugs with opposite color
    % Select which of those bugs can move to an empty position
    % if none return false
    % else true
    true.

free_queen() :-
    % Get Color
    % Get Color Queen
    % Get adyacent to Color Queen with the same color
    % Move it to surround the queen
    % Move it to block
    true.
    
block_bug() :-
    % Get Enemy Bug that can surround Ally Queen
    % Put Ally Bug in a position were Enemy Bug cause
    % Hive disruption
    true.
    
place_bug() :-
    % Shuld this method go here?
    true.

