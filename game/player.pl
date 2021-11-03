:- module( player, [ init_player/1,
					 get_queen/2,
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

set_ants(Ants, player(Queens, _, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_beetles(Beetle, player(Queens, Ants, _, Grasshopper, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_grasshoppers(Grasshopper, player(Queens, Ants, Beetle, _, Ladybug, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_ladybugs(Ladybug, player(Queens, Ants, Beetle, Grasshopper, _, Mosquito, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_mosquitos(Mosquito, player(Queens, Ants, Beetle, Grasshopper, Ladybug, _, Pillbug, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_pillbugs(Pillbug, player(Queens, Ants, Beetle, Grasshopper, Ladybug, Mosquito, _, Spider),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).

set_spiders(Spider, player(Queens, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, _),
		           player(Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)).
