:- module( board, [	adjacent_cell_1/2,
					adjacent_cell_2/2,
					adjacent_cell_3/2,
					adjacent_cell_4/2,
					adjacent_cell_5/2,
					adjacent_cell_6/2,
					adjacent_cell/2,
					adjacent_cells/2,
					add_new_cell/1,
					valid_new_cell/2,
					move_cell/2,
					one_hive/1,
					adjacent_to_hive/2,
					adjacent_to_hive/1,
					insect_above/2,
					accesible_cell/2,
					current_player_color/1,
					set_last_moved_cell/2,
					delete_last_moved_cell/2,
					get_last_moved_cell/3,
					set_fixed_cell/1,
					delete_fixed_cell/1,
					get_fixed_cell/2,
					oponent_color/2
					]).

:- use_module(cell).
:- use_module(utils).
:- use_module(player).
:- use_module(turns).

:- dynamic fixed_cell/2.
:- dynamic last_moved_cell/2.

last_moved_cell(cell(none,none,none,none,none),none).
fixed_cell(cell(none,none,none,none,none),0).

set_last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor):-
	assertz(last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor)).

delete_last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor):-
	retract(last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor)).

get_last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor,cell(Bug,Row,Col,Color,StackPos)):-
	last_moved_cell(cell(Bug,Row,Col,Color,StackPos),PlayerColor).

set_fixed_cell(cell(Bug,Row,Col,Color,StackPos)):-
	total_turns(T),
	NT is T + 1,
	assertz(fixed_cell(cell(Bug,Row,Col,Color,StackPos),NT)).
	
delete_fixed_cell(cell(Bug,Row,Col,Color,StackPos)):-
	retract(fixed_cell(cell(Bug,Row,Col,Color,StackPos),_)).

get_fixed_cell(cell(Bug,Row,Col,Color,StackPos),cell(Bug,Row,Col,Color,StackPos)):-
	fixed_cell(cell(Bug,Row,Col,Color,StackPos)).

% adds a new cell to the board 
% NOTE: to find  new cells possible positions use findall with valid_new_cell/2
add_new_cell(cell(Bug,Row,Col,Color,0)):-
	current_player_turns(3),
	not(get_cell(cell(queen,_,_,Color,_),_)),
	Bug \= queen,!,
	write_ln("QUEEN MUST BE ON BOARD ON THE 4TH MOVE"),
	fail.

add_new_cell(cell(Bug,Row,Col,Color,0)):-
	get_player(player(Color,_,_,_,_,_,_,_,_),Player),
	decrease_bug(Bug,Player,NewPlayer),
	delete_player(Player),
	init_player(NewPlayer),
	init_cell(cell(Bug,Row,Col,Color,0)),
	increase_turns().


move_cell(SourceCell,DestCell):-
	delete_cell(SourceCell),
	init_cell(DestCell),
	delete_last_moved_cell(_,_),
	current_player_color(PlayerColor),
	set_last_moved_cell(DestCell,PlayerColor),
	write_ln("LAST MOVE CELL BY PLAYER"),
	write_ln(PlayerColor),
	write_ln(SourceCell),
	increase_turns().


current_player_color(Color):-
	total_turns(T),
	is_even(T),
	Color = white.

current_player_color(Color):-
	total_turns(T),
	is_odd(T),
	Color = black.

adjacent_cell_1(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row - 1,
	AdjCol is Col,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
	).

adjacent_cell_2(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row - (1-( Col mod 2 )),
	AdjCol is Col+1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
	).

adjacent_cell_3(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row + ( Col mod 2 ),
	AdjCol is Col + 1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
	).


adjacent_cell_4(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row + 1,
	AdjCol is Col,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
	).


adjacent_cell_5(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row + ( Col mod 2 ),
	AdjCol is Col - 1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
	).


adjacent_cell_6(cell(_,Row,Col,_,_),AdjCell):-
	AdjRow is Row - ( 1 - ( Col mod 2 ) ),
	AdjCol is Col - 1,
	(
		cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell);
		(
		not(cell:get_cell(cell(_,AdjRow,AdjCol,_,_),AdjCell)),
		AdjCell = cell(none,AdjRow,AdjCol,none,0)
		)
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

%---------------------------------------------------------------%
% Useful predicates to check some conditions on the board
%---------------------------------------------------------------%

% checks if ValidCell is a valid cell position for player of color Color
%

valid_new_cell(_,ValidCell):-
	cells([X|Y]),
	len([X|Y],1),
	adjacent_cell(X,ValidCell).

valid_new_cell(_,ValidCell):-
	cells([]),
	ValidCell = cell(none,0,0,none,0).

valid_new_cell(Color,ValidCell):-
	get_cell(cell(_,_,_,Color,_),SameColorCell),
	not(insect_above(SameColorCell,_)),
	adjacent_cell(SameColorCell, ValidCell),
	ValidCell = cell(none,_,_,none,0),
	adjacent_cells(ValidCell,AdjCells),
	forall(member(N,AdjCells), valid_adj_cell(N,Color)).

% checks for one hive rule
one_hive(Cell):-
	cells(Cells),
	delete_cell(Cell),
	cells([X|Y]),
	reachable(X,[X],ReachableCells),
	init_cell(Cell),
	len(ReachableCells,R),
	len([X|Y],R).

% non visited cells during dfs
non_visited(Cell,Visited, AdjCell):-
	adjacent_cell(Cell,AdjCell),
	not(get_bug_type(AdjCell,none)),
	not(member(AdjCell, Visited)).

non_visited(Cell,Visited, AboveCell):-
	insect_above(Cell,AboveCell),
	not(member(AboveCell,Visited)).

non_visited(Cell,Visited, BelowCell):-
	insect_below(Cell,BelowCell),
	not(member(BelowCell,Visited)).

% dfs
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

insect_above(cell(_,Row,Col,_,X),AboveCell):-
	AboveCell = cell(Bug,Row,Col,Color,Y),
	get_cell(cell(_,Row,Col,_,_),
			 cell(Bug,Row,Col,Color,Y)),
	X < Y.

insect_below(cell(_,Row,Col,_,X),BelowCell):-
	BelowCell = cell(Bug,Row,Col,Color,Y),
	get_cell(cell(_,Row,Col,_,_),
			 cell(Bug,Row,Col,Color,Y)),
	X > Y.
	

% triumph if the top level adjacent cell is from the same color 
valid_adj_cell(N,Color):-
	(
		insect_above(N,M),!,
		valid_adj_cell(M,Color)
	);
	get_color(N,Color);
	get_color(N,none).

% triumph if Cell is adjacnt to the hive represented by a list of cells
adjacent_to_hive(Cell, HiveCells):-
	adjacent_cell(Cell,AdjCell),
	member(AdjCell,HiveCells),!.

adjacent_to_hive(Cell):-
	cells(HiveCells),
	adjacent_cell(Cell,AdjCell),
	member(AdjCell,HiveCells),!.

% triumph if DestCell is accesible from SourceCell through sliding 
accesible_cell(SourceCell,DestCell):-
	adjacent_cell(SourceCell,AdjCell),
	adjacent_cell(DestCell,AdjCell),
	get_bug_type(AdjCell,none).

oponent_color(white, black).
oponent_color(black, white).
