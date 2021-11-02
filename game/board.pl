:- module( board, [ init_board/2,
					get_cells/2,
					add_cell/3] ).

:- use_module( utils, [push/3] ).

% ---------------------------------------------------------------------------------
% Board structure ->  board(ListOfCells,Turns)
%
% ListOfCells: list of cells structure that represent each piece of the game's board
% Turns: total turns played so far
% WhitePlayer: info related to the white player.It's a player structure from player.pl 
% BlackPlayer: info related to the black player.It's a player structure from player.pl 
% ---------------------------------------------------------------------------------
init_board(board([],0,WhitePlayer, BlackPlayer)):-
	init_player(WhitePlayer),
	init_player(BlackPlayer).

get_cells(board(Cells,_), Cells).

get_current_turn(board(_, Turns),Turns).

add_cell(Cell,Board, NewBoard) :-
	get_cells(Board,BoardCells),
	push(Cell, BoardCells, NewBoard).

% Give all the possible movements of the given cell 
% move_cell(Cell,Board, PossibleMovements):-
		
