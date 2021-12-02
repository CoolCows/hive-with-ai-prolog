:- module( queen, [valid_queen_movement/2 ] ).

:- use_module("../board").
:- use_module("../cell").
:- use_module("../utils").

valid_queen_movement(SourceCell, cell(none, Row, Col, none, 0)) :-
    % delete_cell(SourceCell),
    adjacent_cell(SourceCell, cell(none, Row, Col, none, 0)).
