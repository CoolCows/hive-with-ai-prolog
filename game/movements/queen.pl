:- module(queen, [ valid_queen_movement/2 ]).

:- use_module("../board").
:- use_module("../cell").

valid_queen_movement(SourceCell,DestCell).
