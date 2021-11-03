:- module( board, [ init_board/4,
					get_cells/2,
					add_cell/3,
					movements/3]
					).

:- use_module( cell, [ get_bug_type/3] ).
:- use_module( utils, [push/3] ).
:- use_module( player, [init_player/1] ).
:- use_module( "./movements/ant.", [movements_ant/3] ).
:- use_module( "./movements/queen", [movements_queen/3] ).

% ---------------------------------------------------------------------------------
% Board structure ->  board(ListOfCells,Turns,WhitePlayer,BlackPlayer)
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

get_white_player(board(_,_,WhitePlayer,_),WhitePlayer).

get_black_player(board(_,_,_,BlackPlayer),BlackPlayer).

set_cells(Cells, board(_, Turns, WhitePlayer, BlackPlayer),
		  board(Cells, Turns, WhitePlayer, BlackPlayer)).

set_turns(Turns, board(Cells,_, WhitePlayer, BlackPlayer),
		  board(Cells, Turns, WhitePlayer, BlackPlayer)).

set_white_player(WhitePlayer, board(Cells, Turns,_, BlackPlayer),
				 board(Cells, Turns, WhitePlayer, BlackPlayer)).

set_black_player(BlackPlayer, board(Cells, Turns, WhitePlayer,_),
				 board(Cells, Turns, WhitePlayer,BlackPlayer)).

increase_turn(Board, NewBoard ):-
	get_current_turn(Board, Turns),
	NewTurns is Turns + 1,
	set_turns(NewTurns, Board, NewBoard).


add_cell(Cell,Board, NewBoard) :-
	%TODO: validate position of new cell
	get_cells(Board,BoardCells),
	push(Cell, BoardCells, NewBoard).

% Give all the possible movements of the given cell 
movements(Cell, Board, PossibleMovements):-
	get_bug_type(Cell, BugType), 
	BugType == queen,
	!,
	movements_queen(Cell, Board, PossibleMovements).

movements(Cell, Board, PossibleMovements):-
	get_bug_type(Cell, BugType), 
	BugType == ant,
	!, 
	movements_ant(Cell, Board, PossibleMovements).
	

		
