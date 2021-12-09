:- module(mosquito, [valid_mosquito_movement/2, mosquito_adjacent_to_pillbug/1]).

:- use_module("../board").
:- use_module("../cell").
:- use_module(ant).
:- use_module(queen).
:- use_module(spider).
:- use_module(grasshopper).
:- use_module(ladybug).
:- use_module(beetle).
:- use_module(pillbug).


valid_mosquito_movement(cell(mosquito,Row,Col,Color,StackPos),DestCells):-
	StackPos > 0,!,
	delete_cell(cell(mosquito,Row,Col,Color,StackPos)),
	init_cell(cell(beetle,Row,Col,Color,StackPos)),
	findall(DestCell,valid_movement(cell(AdjBug,Row,Col,Color,StackPos),DestCell),DestCells),
	delete_cell(cell(beetle,Row,Col,Color,StackPos)),
	init_cell(cell(mosquito,Row,Col,Color,StackPos)).

valid_mosquito_movement(cell(mosquito,Row,Col,Color,StackPos),DestCells):-
	adjacent_cell(cell(mosquito,Row,Col,Color,StackPos),AdjCell),
	not(get_bug_type(AdjCell,none)),
	top_level_cell(AdjCell,TopCell),
	get_bug_type(TopCell,AdjBug),
	delete_cell(cell(mosquito,Row,Col,Color,StackPos)),
	init_cell(cell(AdjBug,Row,Col,Color,StackPos)),
	findall(DestCell,valid_movement(cell(AdjBug,Row,Col,Color,StackPos),DestCell),DestCells),
	delete_cell(cell(AdjBug,Row,Col,Color,StackPos)),
	init_cell(cell(mosquito,Row,Col,Color,StackPos)).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,beetle),!,
	valid_beetle_movement(SourceCell,DestCell).

valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,ant),!,
	valid_ant_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,queen),!,
	valid_queen_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,spider),!,
	valid_spider_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,grasshopper),!,
	valid_grasshopper_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,ladybug),!,
	valid_ladybug_movement(SourceCell,DestCell).


valid_movement(SourceCell,DestCell):-
	get_bug_type(SourceCell,pillbug),!,
	valid_pillbug_movement(SourceCell,DestCell).

mosquito_adjacent_to_pillbug(SourceCell) :-
	get_stack_pos(SourceCell,0),
    adjacent_cell(SourceCell, DestCell),
    DestCell = cell(pillbug, _, _, _, _).
