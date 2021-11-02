:- module(cell, [ init_cell/4,
				  get_bug_type/2,
			      get_row/2,
			      get_col/2,
			      set_bug_type/3,
			  	  set_row/3,
			  	  set_col/3
			  	  ]).

% Cell structure -> cell(BugType, Row , Col)
init_cell(Bug, Row, Col, cell(Bug, Row, Col)).

get_bug_type(cell(Bug,_,_),Bug).
get_row(cell(_,Row,_),Row).
get_col(cell(_,_,Col),Col).

set_bug_type(Bug, cell(_,Row,Col),cell(Bug, Row, Col)).
set_row(Row, cell(Bug,_,Col),cell(Bug, Row, Col)).
set_col(Col, cell(Bug,Row,_),cell(Bug, Row, Col)).

