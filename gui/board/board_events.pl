:- module(board_events, [select_bug_to_play/2]).

:- use_module(library(pce)).
:- use_module(tools, [max/3, min/3]).

select_bug_to_play(Board, Pos) :-
   get(Board, height, H),
   get(Board, width, W),
   MX is W/2, MY is H/2,
   get(Pos, x, X),
   get(Pos, y, Y),
   get_index_from_coordinates(X, Y, MX, MY, 1, Col, Row),
   write_ln("Col"+Col+"Row"+Row),
   write_ln(X-Y).

% Found board center!
get_index_from_coordinates(MX, MY, MX, MY, _, 0, 0).

% Aligned with X index and near MY!
get_index_from_coordinates(MX,Y, MX, MY, S,Col,Row) :-
    ((Y > MY, New_Y is Y - S*75, max(New_Y, MY, R), R =:= MY);
    (Y < MY, New_Y is Y + S*75, min(New_Y, MY, R), R =:= MY)),
    get_index_from_coordinates(MX, MY, MX, MY, S, Col, Row).

% Aligned with X index and smaller than MY
get_index_from_coordinates(MX, Y, MX, MY, S, Col, Row) :-
    Y < MY,!,
    New_Y is Y + S*100, Valid_Y is min(New_Y, MY),
    get_index_from_coordinates(MX, Valid_Y, MX, MY, S, Col, New_Row),
    Row is New_Row - 1.

% Aligned with X index and bigger than MY
get_index_from_coordinates(MX, Y, MX, MY, S, Col, Row) :-
    Y > MY,!,
    New_Y is Y - S*100, Valid_Y is max(New_Y, MY),
    get_index_from_coordinates(MX, Valid_Y, MX, MY, S, Col, New_Row),
    Row is New_Row + 1.

% X position Near MX
get_index_from_coordinates(X,Y, MX, MY, S,Col,Row) :-
    ((X > MX, New_X is X - S*75, max(New_X, MX, R), R == MX);
    (X < MX, New_X is X + S*75, min(New_X, MX, R), R == MX)),
    get_index_from_coordinates(MX, Y, MX, MY, S, Col, Row).

% X position smaller than MX
get_index_from_coordinates(X,Y, MX, MY, S,Col,Row) :-
    X < MX,!,
    New_X is X + S*75, Valid_X is min(New_X, MX),
    get_index_from_coordinates(Valid_X, Y, MX, MY, S, New_Col, Row),
    Col is New_Col - 1.

% X position bigger than MX
get_index_from_coordinates(X,Y,MX, MY, S,Col,Row) :-
    X > MX,!, 
    New_X is X - S*75, Valid_X is max(New_X, MX),
    get_index_from_coordinates(Valid_X, Y, MX, MY, S, New_Col, Row),
    Col is New_Col + 1.

