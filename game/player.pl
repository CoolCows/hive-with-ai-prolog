:- module( player, [ init_player/1,
					 get_queens/2,
					 get_ants/2,
					 get_beetles/2,
					 get_grasshopper/2,
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
					 set_spiders/3
					 ] ).

% ---------------------------------------------------------------------------------
% Player structure -> player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)
%
% Queen: total queens outside the board
% Ants: total ants outside the board
% Beetle: total beetles outside the board
% Grasshopper: total grasshopper outside the board
% Ladybug: total ladybugs outside the board
% Mosquito: total mosquitos outside the board
% Pillbug: total pillbugs outside the board
% Spider: total spiders outside the board
% ---------------------------------------------------------------------------------
init_player( player(1,     3,    2,      3,          1,       1,        1,       2)).

get_queens(player(Queens,_,_,_,_,_,_,_),Queens).

get_ants(player(_,Ants,_,_,_,_,_,_),Ants).

get_beetles(player(_,_,Beetles,_,_,_,_,_),Beetles).

get_grasshopper(player(_,_,_,Grasshoppers,_,_,_,_),Grasshoppers).

get_ladybugs(player(_,_,_,_,Ladybugs,_,_,_),Ladybugs).

get_mosquitos(player(_,_,_,_,_,Mosquitos,_,_),Mosquitos).

get_pillbugs(player(_,_,_,_,_,_,Pillbugs,_),Pillbugs).

get_spiders(player(_,_,_,_,_,_,_,Spiders),Spiders).

set_queens(Queen, player(_, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_ants(Ants, player(Queen, _, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_beetles(Beetle, player(Queen, Ants, _, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_grasshoppers(Grasshopper, player(Queen, Ants, Beetle, _, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_ladybugs(Ladybug, player(Queen, Ants, Beetle, Grasshopper, _, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_mosquitos(Mosquito, player(Queen, Ants, Beetle, Grasshopper, Ladybug, _, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_pillbugs(Pillbug, player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, _, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_spiders(Spider, player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, _),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

decrease_bug(queen, Player, NewPlayer):-
	!,
	get_queens(Player, Bug),
	Bug is Bug - 1,
	set_queens(Bug,Player, NewPlayer).

decrease_bug(ant, Player, NewPlayer):-
	!,
	get_ants(Player, Bug),
	Bug is Bug - 1,
	set_ants(Bug,Player, NewPlayer).

decrease_bug(beetle, Player, NewPlayer):-
	!,
	get_beetles(Player, Bug),
	Bug is Bug - 1,
	set_beetles(Bug,Player, NewPlayer).

decrease_bug(grasshopper, Player, NewPlayer):-
	!,
	get_grasshopper(Player, Bug),
	Bug is Bug - 1,
	set_grasshoppers(Bug,Player, NewPlayer).

decrease_bug(ladybug, Player, NewPlayer):-
	!,
	get_ladybugs(Player, Bug),
	Bug is Bug - 1,
	set_ladybugs(Bug,Player, NewPlayer).

decrease_bug(mosquito, Player, NewPlayer):-
	!,
	get_mosquitos(Player, Bug),
	Bug is Bug - 1,
	set_mosquitos(Bug,Player, NewPlayer).

decrease_bug(pillbug, Player, NewPlayer):-
	!,
	get_pillbugs(Player, Bug),
	Bug is Bug - 1,
	set_pillbugs(Bug,Player, NewPlayer).

decrease_bug(spider, Player, NewPlayer):-
	!,
	get_spiders(Player, Bug),
	Bug is Bug - 1,
	set_spiders(Bug,Player, NewPlayer).
