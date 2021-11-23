:- module( queen, [ movements_queen/3 ] ).

:- use_module("../board", [ 
					get_cells/2,
					get_cell/4,
					get_current_turn/2,
					get_white_player/2,
					get_black_player/2,
					set_cells/3,
					set_turns/3,
					set_white_player/3,
					set_black_player/3,
					increase_turn/2,
					adjacent_cells/3,
					adjacent_cell_1/3,
					adjacent_cell_2/3,
					adjacent_cell_3/3,
					adjacent_cell_4/3,
					adjacent_cell_5/3,
					adjacent_cell_6/3
 					]).

:- use_module("../cell", [ get_color/2,
							get_row/2,
							get_col/2,
							set_row/3,
							same_position/2
							]).

movements_queen(Cell, Board, Adj):-
	adjacent_cells(Cell,Board, Adj),
	get_bug_type(Adj, none),
	adjacent_cells(Adj,Board,Adj2),
	not(get_bug_type(Adj2,none)),
	not(same_position(Adj2,Cell)).

