:- module(board_events, [select_bug_to_play/2]).

:- use_module(library(pce)).
:- use_module(tools, [max/3, min/3]).

select_bug_to_play(Picture, ClickedPos) :-
    get(@cells, board, CCells),
    chain_list(CCells, GCells),
    write_ln(CCells),
    write_ln(GCell),
    X = [1,2,3],
    write_ln(X),
    scan_graphic_cells(GCells, ClickedPos, Index).

scan_graphic_cells([GrCell|Rest], ClickedPos, Index) :-
    write_ln(GrCell),
    get(GrCell, click_inside(ClickedPos), Result),!,
    (Result, Index is 15);
    scan_graphic_cells([Rest], ClickedPos).
