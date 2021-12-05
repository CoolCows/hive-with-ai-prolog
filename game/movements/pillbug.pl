:- module(pillbug, [
    valid_pillbug_movement/2,
    movable_cells_by_pillbug/2,
    movable_positions_by_pillbug/2
	% is_cell_moved_by_pillbug/2
]).

:- use_module("../board").
:- use_module("../cell").


valid_pillbug_movement(SourceCell,DestCell):-
	one_hive(SourceCell),
    adjacent_cell(SourceCell, DestCell),
	get_bug_type(DestCell,none),
	cells(Cells),
	delete(Cells,SourceCell,CellsWithoutSourceCell),
	adjacent_to_hive(DestCell,CellsWithoutSourceCell).

movable_cell_by_pillbug(PillbugCell,MovableBug):-
	adjacent_cell(PillbugCell,MovableBug),
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
    adjacent_cell(PillbugCell, DestCell),
    DestCell = cell(none, _, _, none, 0).

movable_positions_by_pillbug(PillbugCell, MovToCells) :-
    findall(AdyCell, movable_position_by_pillbug(PillbugCell, AdyCell), MovToCells).

% triumph if SourceCell-DestCell movement is done by pillbug
% is_cell_moved_by_pillbug(SourceCell,DestCell):-
% 	current_player_color(Color),
% 	get_cell(cell(pillbug,_,_,Color,_),PillbugCell),
% 	movable_cells_by_pillbug(PillbugCell,MovableBugs),
%     movable_positions_by_pillbug(PillbugCell, MovablePositions),
% 	member(SourceCell,MovableBugs),
% 	member(DestCell,MovablePositions).
	
