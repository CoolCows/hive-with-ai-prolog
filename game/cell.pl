:- module(cell, [ init_cell/5,
				  init_cell/1,
				  delete_cell/1,
				  get_cell/2,
				  cells/1,
				  get_bug_type/2,
			      get_row/2,
			      get_col/2,
				  get_color/2,
				  get_stack_pos/2,
			      set_bug_type/3,
			  	  set_row/3,
			  	  set_col/3,
				  set_color/3,
				  set_stack_pos/3,
				  same_position/2,
				  set_cells/1
			  	  ]).

:- dynamic cell/5.
% ---------------------------------------------------------------------------------
% Cell structure -> cell(BugType, Row , Col, Color, StackPos)
%
% BugType: type of bug in the current cell
% Row: relative row position
% Col: relative column position
% StackPos: if two cells are located in the same relative position then this value
% acts like a z-index, giving the stack position of the object. Default is 0.
% ---------------------------------------------------------------------------------

init_cell(cell(Bug, Row, Col, Color, StackPos)):-
	assertz(cell(Bug, Row, Col, Color, StackPos)),!.
init_cell(Bug, Row, Col, Color, StackPos):- 
	assertz(cell(Bug, Row, Col, Color, StackPos)),!.

delete_cell(cell(Bug, Row, Col, Color, StackPos)):-
	retract(cell(Bug,Row,Col,Color,StackPos)).

set_cells(NewCells):-
	cells(Cells),
	forall(member(Cell,Cells),delete_cell(Cell)),
	forall(member(NewCell,NewCells),init_cell(NewCell)).

cells(Cells):-
	findall(Cell, get_cell(Cell,_),Cells).

get_cell(cell(Bug, Row, Col, Color, StackPos), cell(Bug, Row, Col, Color, StackPos)):-
	cell(Bug, Row, Col, Color, StackPos).

get_bug_type(cell(Bug,_,_,_,_),Bug).

get_row(cell(_,Row,_,_,_),Row).

get_col(cell(_,_,Col,_,_),Col).

get_color(cell(_,_,_,Color,_),Color).

get_stack_pos(cell(_,_,_,_,StackPos),StackPos).

set_bug_type(Bug, cell(_,Row,Col,Color,StackPos),
			cell(Bug, Row, Col,Color,StackPos)).

set_row(Row, cell(Bug,_,Col,Color,StackPos),
		cell(Bug, Row, Col,Color,StackPos)).

set_col(Col, cell(Bug,Row,_,Color,StackPos),
	    cell(Bug, Row, Col,Color,StackPos)).

set_color(Color, cell(Bug,Row,Col,_,StackPos),
	    cell(Bug, Row, Col,Color,StackPos)).

set_stack_pos(StackPos, cell(Bug,Row,Col,Color,_),
	    cell(Bug, Row, Col,Color,StackPos)).

same_position(cell(_,Row,Col,_,_),cell(_,Row,Col,_,_)).
