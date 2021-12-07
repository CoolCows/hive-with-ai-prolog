:- module( turns,[ init_turns/0, increase_turns/0, current_player_turns/1, total_turns/1 ] ).

:- use_module(utils).
:- dynamic turns/1.

total_turns(T):-
	turns(T).

init_turns():-
	assertz(turns(0)).

set_turns(T):-
	retract(turns(_)),
	assertz(turns(T)).

increase_turns():-
	turns(T),
	retract(turns(T)),
	NewT is T + 1,
	assertz(turns(NewT)).

current_player_turns(P):-
	turns(0),!,
	P is 0.

current_player_turns(P):-
	turns(T),
	is_even(T),!,
	P is T div 2.

current_player_turns(P):-
	turns(T),
	is_odd(T),!,
	NT is T - 1,
	P is NT div 2.

