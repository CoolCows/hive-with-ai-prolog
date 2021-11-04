:- module(board_events, [select_bug_to_play/2]).

:- use_module(library(pce)).
:- use_module(tools, [max/3, min/3]).

select_bug_to_play(Board, ClickedPos) :-
    nb_getval(graphic_cells, GCells),
    scan_graphic_cells(GCells, ClickedPos, Index).

scan_graphic_cells([GrCell|Rest], ClickedPos, Index) :-
    get(GrCell, click_inside(ClickedPos), Result),!,
    (Result, Index is 15);
    scan_graphic_cells([Rest], ClickedPos).
