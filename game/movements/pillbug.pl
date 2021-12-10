:- module(pillbug, [
    valid_pillbug_movement/2,
    movable_cells_by_pillbug/2,
    movable_positions_by_pillbug/2
]).

:- use_module("../board").
:- use_module("../cell").


valid_pillbug_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
    adjacent_cell(SourceCell, DestCell),
	get_bug_type(DestCell,none),
	adjacent_hive_cell(DestCell, AdjHiveCell),
	adjacent_cell(SourceCell,AdjHiveCell).

movable_cell_by_pillbug(PillbugCell,MovableBug):-
	not(insect_above(PillbugCell,_)),
	adjacent_cell(PillbugCell,MovableBug),
	not(get_fixed_cell(fixed_cell(MovableBug,_),_)),
	accesible_cell_top_level(PillbugCell,MovableBug),
	one_hive(MovableBug),
	not(insect_above(MovableBug,_)),
	get_stack_pos(MovableBug,0),
	not(get_bug_type(MovableBug,none)),
	get_color(PillbugCell,PlayerColor),
	oponent_color(PlayerColor,OponentColor),
	not(get_last_moved_cell(MovableBug,OponentColor,MovableBug)).

movable_cells_by_pillbug(PillbugCell,MovableBugs):-
	findall(MovableBug,movable_cell_by_pillbug(PillbugCell,MovableBug),MovableBugs).

movable_position_by_pillbug(PillbugCell, DestCell) :-
	not(insect_above(PillbugCell,_)),
    adjacent_cell(PillbugCell, DestCell),
	accesible_cell_top_level(PillbugCell,DestCell),
    DestCell = cell(none, _, _, none, 0).

movable_positions_by_pillbug(PillbugCell, MovToCells) :-
    findall(AdjCell, movable_position_by_pillbug(PillbugCell, AdjCell), MovToCells).

