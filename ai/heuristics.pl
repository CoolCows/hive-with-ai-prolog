:- module( heuristic, [ surround_queen/1 ] ).

:- use_module("../game/hive_api").
:- use_module("../game/board").

% Import adyacent cells
% Import cells movement

% heuristic(
%     game_state(Board, Turns, LastMovedCells, FixedCells, Players),
%     NextGameState) 
% :-
%     true.

surround_queen( NextGameState):-
	hive_current_player_color(Color),
	oponent_color(Color,OponentColor),
	hive_get_cell(cell(_,_,_,Color,_),Cell),
	hive_get_possible_moves(Cell, PosMoves),
	hive_get_cell(cell(queen,_,_,OponentColor,_),QueenCell),
	adjacent_cell(QueenCell,QueenAdj),
	member(QueenAdj,PosMoves),
	hive_move_cell(Cell,QueenAdj),
	hive_get_game_state(NextGameState).

free_queen(NextGameState):-
	hive_current_player_color(Color),
	hive_get_cell(cell(queen,_,_,Color,_),QueenCell),
	adjacent_cell(QueenCell,AdjCell),
	get_color(AdjCell,Color),
	hive_get_possible_moves(AdjCell,PosMoves),
	member(PosMove,PosMoves),
	not(adjacent_cell(QueenCell,PosMove)),
	move_cell(AdjCell,PosMove),
	hive_get_game_state(NextGameState).


    
block_bug() :-
    % Get Enemy Bug that can surround Ally Queen
    % Put Ally Bug in a position were Enemy Bug cause
    % Hive disruption
    true.
    
place_bug() :-
    % Shuld this method go here?
    true.

% current_player_color(T,Color):-
% 	is_even(T),
% 	Color = white.

% current_player_color(T,Color):-
% 	is_odd(T),
% 	Color = black.
