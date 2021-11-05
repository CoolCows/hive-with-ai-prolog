:- module( board, [ init_board/1,
					get_cells/2,
					get_cell/4,
					get_current_turn/2,
					get_white_player/2,
					get_black_player/2,
					set_cells/3,
					set_turns/3,
					set_white_player/3,
					set_black_player/3,
					increase_turn/2,
					adjacent_cells/3,
					adjacent_cell_1/3,
					adjacent_cell_2/3,
					adjacent_cell_3/3,
					adjacent_cell_4/3,
					adjacent_cell_5/3,
					adjacent_cell_6/3,
					add_new_cell/3,
					move_cell/4
					]).

:- use_module( cell, [ get_row/2,
					   get_col/2,
					   init_cell/6,
					   get_bug_type/2, 
					   set_bug_type/3]
				   ).
:- use_module( utils, [push/3] ).
:- use_module( player, [init_player/1] ).

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

get_cells(board(Cells,_,_,_), Cells).

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

get_cell(Row, Col, Board, Cell):-
	get_cells(Board, Cells),
	get_cell_(Row,Col,Cells, Cell).

get_cell(Row, Col, Board, Cell):-
	get_cells(Board, Cells),
	not( get_cell_(Row,Col, Cells, Cell) ),
	init_cell(none,Row,Col,none,0, Cell).

get_cell_(Row,Col,[cell(BugType,Row,Col,Color,StackPos)|_], cell(BugType,Row,Col,Color, StackPos)).
get_cell_(Row,Col,[_|T],Cell):- get_cell_(Row,Col,T,Cell).

%---------------------------------------------------------------%
%                         ADJACENT CELLS                        %
%---------------------------------------------------------------%
%							   1 2                              %
%						 	  3 0 4                             %
%  	   						   5 6                              %
%---------------------------------------------------------------%
%---------------------------------------------------------------%

adjacent_cell_1(Cell,Board,AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjRow is Row - 1,
	get_cell(AdjRow, Col, Board, AdjCell).

adjacent_cell_2(Cell,Board, AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjRow is Row - 1,
	AdjCol is Col + 1,
	get_cell(AdjRow, AdjCol, Board, AdjCell).

adjacent_cell_3(Cell,Board, AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjCol is Col - 1,
	get_cell(Row, AdjCol, Board, AdjCell).

adjacent_cell_4(Cell,Board, AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjCol is Col + 1,
	get_cell(Row, AdjCol, Board, AdjCell).

adjacent_cell_5(Cell,Board, AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjRow is Row + 1,
	AdjCol is Col - 1,
	get_cell(AdjRow, AdjCol, Board, AdjCell).

adjacent_cell_6(Cell,Board,AdjCell):-
	get_row(Cell, Row),
	get_col(Cell, Col),
	AdjRow is Row + 1,
	get_cell(AdjRow, Col, Board, AdjCell).

adjacent_cells(Cell,Board, AdjCell):-
	adjacent_cell_1(Cell,Board, AdjCell);
	adjacent_cell_2(Cell,Board, AdjCell);
	adjacent_cell_3(Cell,Board, AdjCell);
	adjacent_cell_4(Cell,Board, AdjCell);
	adjacent_cell_5(Cell,Board, AdjCell);
	adjacent_cell_6(Cell,Board, AdjCell).

%---------------------------------------------------------------%
%---------------------------------------------------------------%

move_cell(SourceCell, DestCell, Board, NewBoard):-
	get_cells(Board,Cells),
	delete(Cells, SourceCell, NewCells),
	move_cell_(SourceCell, DestCell, NewCells, NewCells1),
	set_cells(NewCells1, Board, NewBoard).

move_cell_(cell(BugType, _, _, Color,_), cell(none,Row, Col,_,_),Cells, NewCells):-
	init_cell(BugType, Row, Col, Color,0,NewCell),
	!,
	push(NewCell, Cells,NewCells).
	
move_cell_(cell(BugType,_,_,Color,_), cell(_,Row,Col,_,StackPos),Cells, NewCells):-
	NewStackPos is StackPos + 1,
	init_cell(BugType,Row,Col,Color, NewStackPos, NewCell),
	push(NewCell, Cells, NewCells).

add_new_cell(Cell,Board, NewBoard) :-
	get_cells(Board,BoardCells),
	push(Cell, BoardCells, NewBoardCells),
	set_cells(NewBoardCells,Board,NewBoard).


