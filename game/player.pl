:- module( player, [ init_player/1,
					 delete_player/1,
					 get_player/2,
					 players/1,
					 get_queens/2,
					 get_ants/2,
					 get_beetles/2,
					 get_grasshoppers/2,
					 get_ladybugs/2,
					 get_mosquitos/2,
					 get_pillbugs/2,
					 get_spiders/2,
					 set_queens/3,
					 set_ants/3,
					 set_beetles/3,
					 set_grasshoppers/3,
					 set_ladybugs/3,
					 set_pillbugs/3,
					 set_mosquitos/3,
					 set_spiders/3,
					 decrease_bug/3
					 ] ).
:- dynamic player/9.

% ---------------------------------------------------------------------------------
% Player structure -> player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)
%
% Color : player color (black or white)
% Queen: total queens outside the board
% Ants: total ants outside the board
% Beetle: total beetles outside the board
% Grasshopper: total grasshopper outside the board
% Ladybug: total ladybugs outside the board
% Mosquito: total mosquitos outside the board
% Pillbug: total pillbugs outside the board
% Spider: total spiders outside the board
% ---------------------------------------------------------------------------------

init_player(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)):-
	assertz(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),!.
init_player(Color):-
	assertz(player(Color,1,3,2,3,1,1,1,2)),!.

delete_player(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)):-
	retract(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),!.

players(Players):-
	findall(Player, get_player(Player,_),Players).

get_player(player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		   player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)
):-
	player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider).


get_queens(player(_,Queens,_,_,_,_,_,_,_),Queens).

get_ants(player(_,_,Ants,_,_,_,_,_,_),Ants).

get_beetles(player(_,_,_,Beetles,_,_,_,_,_),Beetles).

get_grasshoppers(player(_,_,_,_,Grasshoppers,_,_,_,_),Grasshoppers).

get_ladybugs(player(_,_,_,_,_,Ladybugs,_,_,_),Ladybugs).

get_mosquitos(player(_,_,_,_,_,_,Mosquitos,_,_),Mosquitos).

get_pillbugs(player(_,_,_,_,_,_,_,Pillbugs,_),Pillbugs).

get_spiders(player(_,_,_,_,_,_,_,_,Spiders),Spiders).

set_queens(Queen, player(Color, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_ants(Ants, player(Color,Queen, _, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_beetles(Beetle, player(Color,Queen, Ants, _, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_grasshoppers(Grasshopper, player(Color,Queen, Ants, Beetle, _, Ladybug, Mosquito, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_ladybugs(Ladybug, player(Color,Queen, Ants, Beetle, Grasshopper, _, Mosquito, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_mosquitos(Mosquito, player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, _, Pillbug, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_pillbugs(Pillbug, player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, _, Spider),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_spiders(Spider, player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, _),
		           player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

% NOTE: need testing
decrease_bug(queen, Player, NewPlayer):-
	!,
	get_queens(Player, Bug),
	NewBugCount is Bug - 1,
	set_queens(NewBugCount,Player, NewPlayer).

decrease_bug(ant, Player, NewPlayer):-
	!,
	get_ants(Player, Bug),
	NewBugCount is Bug - 1,
	set_ants(NewBugCount,Player, NewPlayer).

decrease_bug(beetle, Player, NewPlayer):-
	!,
	get_beetles(Player, Bug),
	NewBugCount is Bug - 1,
	set_beetles(NewBugCount,Player, NewPlayer).

decrease_bug(grasshopper, Player, NewPlayer):-
	!,
	get_grasshopper(Player, Bug),
	NewBugCount is Bug - 1,
	set_grasshoppers(NewBugCount,Player, NewPlayer).

decrease_bug(ladybug, Player, NewPlayer):-
	!,
	get_ladybugs(Player, Bug),
	NewBugCount is Bug - 1,
	set_ladybugs(NewBugCount,Player, NewPlayer).

decrease_bug(mosquito, Player, NewPlayer):-
	!,
	get_mosquitos(Player, Bug),
	NewBugCount is Bug - 1,
	set_mosquitos(NewBugCount,Player, NewPlayer).

decrease_bug(pillbug, Player, NewPlayer):-
	!,
	get_pillbugs(Player, Bug),
	NewBugCount is Bug - 1,
	set_pillbugs(NewBugCount,Player, NewPlayer).

decrease_bug(spider, Player, NewPlayer):-
	!,
	get_spiders(Player, Bug),
	NewBugCount is Bug - 1,
	set_spiders(NewBugCount,Player, NewPlayer).
