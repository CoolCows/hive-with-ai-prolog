:- module( board, [	adjacent_cell_1/2,
					adjacent_cell_2/2,
					adjacent_cell_3/2,
					adjacent_cell_4/2,
					adjacent_cell_5/2,
					adjacent_cell_6/2,
					adjacent_cell/2,
					adjacent_cells/2,
					valid_new_cell/2,
					one_hive/1
					]).

:- use_module(cell).
:- use_module(utils).
:- use_module(player).


%---------------------------------------------------------------%
%                         ADJACENT CELLS                        %
%---------------------------------------------------------------%
%							   1 2                              %
%						 	  3 0 4                             %
%  	   						   5 6                              %
%---------------------------------------------------------------%
%---------------------------------------------------------------%

adjacent_cell_1(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row -1,
	(
		cell:get_cell(cell(_,AdjRow,Col,_,_),AdjCell),!;
		AdjCell = cell(none,AdjRow,Col,none,0)
	).

adjacent_cell_2(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row -1,
	AdjCol is Col +1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell),!;
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
	).

adjacent_cell_3(cell(_,Row,Col,_,_),AdjCell):-
	AdjCol is Col -1,
	(
		cell:get_cell(cell(_,Row,AdjCol,_,_),AdjCell),!;
		AdjCell = cell(none,Row,AdjCol,none,0)
	).


adjacent_cell_4(cell(_,Row,Col,_,_),AdjCell):-
	AdjCol is Col +1,
	(
		cell:get_cell(cell(_,Row,AdjCol,_,_),AdjCell),!;
		AdjCell = cell(none,Row,AdjCol,none,0)
	).


adjacent_cell_5(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row +1,
	AdjCol is Col -1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell),!;
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
	).


adjacent_cell_6(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row +1,
	(
		cell:get_cell(cell(_,AdjRow,Col,_,_),AdjCell),!;
		AdjCell = cell(none,AdjRow,Col,none,0)
	).

adjacent_cell(Cell, AdjCell):-
	adjacent_cell_1(Cell, AdjCell);
	adjacent_cell_2(Cell, AdjCell);
	adjacent_cell_3(Cell, AdjCell);
	adjacent_cell_4(Cell, AdjCell);
	adjacent_cell_5(Cell, AdjCell);
	adjacent_cell_6(Cell, AdjCell).

adjacent_cells(Cell, AdjCells):-
	findall(AdjCell, adjacent_cell(Cell,AdjCell),AdjCells).

%---------------------------------------------------------------%
%---------------------------------------------------------------%

% move_cell(SourceCell, DestCell, Board, NewBoard):-
% 	get_cells(Board,Cells),
% 	delete(Cells, SourceCell, NewCells),
% 	move_cell_(SourceCell, DestCell, NewCells, NewCells1),
% 	set_cells(NewCells1, Board, NewBoard).

% move_cell_(cell(BugType, _, _, Color,_), cell(none,Row, Col,_,_),Cells, NewCells):-
% 	init_cell(BugType, Row, Col, Color,0,NewCell),
% 	!,
% 	push(NewCell, Cells,NewCells).
	
% move_cell_(cell(BugType,_,_,Color,_), cell(_,Row,Col,_,StackPos),Cells, NewCells):-
% 	NewStackPos is StackPos + 1,
% 	init_cell(BugType,Row,Col,Color, NewStackPos, NewCell),
% 	push(NewCell, Cells, NewCells).

% add_new_cell(Cell,Board, NewBoard) :-
% 	get_color(Cell, white),
% 	!,
% 	get_cells(Board, Cells),
% 	get_bug_type(Cell, BugType),
% 	get_white_player(Board, WhitePlayer),
% 	decrease_bug(BugType, WhitePlayer, NewWhitePlayer),
% 	set_white_player(NewWhitePlayer,Board,B),
% 	push(Cell, Cells, NewBoardCells),
% 	set_cells(NewBoardCells,B,NewBoard).

% add_new_cell(Cell,Board, NewBoard) :-
% 	get_color(Cell, black),
% 	!,
% 	get_cells(Board, Cells),
% 	get_bug_type(Cell, BugType),
% 	get_black_player(Board, BlackPlayer),
% 	decrease_bug(BugType, BlackPlayer, NewBlackPlayer),
% 	set_black_player(NewBlackPlayer, Board,B),
% 	push(Cell, Cells, NewBoardCells),
% 	set_cells(NewBoardCells,B,NewBoard).
%

valid_new_cell(Color,ValidCell):-
	get_cell(cell(_,_,_,Color,0),SameColorCell),
	adjacent_cell(SameColorCell, ValidCell),
	ValidCell = cell(none,_,_,none,0),
	adjacent_cells(ValidCell,AdjCells),
	forall(member(N,AdjCells), valid_color(N,Color)).

% valid adjacent cells' color for new cell
valid_color(N,Color):-
	get_color(N,Color);
	get_color(N,none).

one_hive(Cell):-
	cells(Cells),
	delete(Cells,Cell, [X|Y]),
	reachable(X,[X],ReachableCells),
	len(ReachableCells,R),
	len([X|Y],R).

non_visited(Cell,Visited, AdjCell):-
	adjacent_cell(Cell,AdjCell),
	not(get_bug_type(AdjCell,none)),
	not(member(AdjCell, Visited)).

reachable([],X,X):-!.
reachable([Cell|RestOfCells],Visited, ReachableCells):-
	!,
	reachable(Cell,Visited,A),
	% NOTE: visit only the rest of non-visited cells instead of RestOfCells
	reachable(RestOfCells,A,ReachableCells).

reachable(Cell,Visited,ReachableCells):-
	findall(AdjCell, non_visited(Cell, Visited,AdjCell),AdjCells),
	append(AdjCells,Visited,A),
	reachable(AdjCells,A,ReachableCells).

% insect_above(Cell):-
% 	cells(Cells),
% 	any(Cells,[above,Cell]).

% above(Cell,AnotherCell):-
% 	same_position(Cell,AnotherCell),
% 	get_stack_pos(Cell,X),
% 	get_stack_pos(AnotherCell,Y),
% 	Y > X.
