:- module( queen, [valid_queen_movement/2 ] ).

:- use_module("../board").
:- use_module("../cell").
:- use_module("../utils").

valid_queen_movement(SourceCell, AdjCell) :-
    % delete_cell(SourceCell),
    adjacent_cell(SourceCell, AdjCell),
    AdjCell = cell(none, _, _, none, _).
