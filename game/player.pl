:- module( player, [ init_player/9,
					 get_queen/9,
					 get_ants/9,
					 get_beetles/9,
					 get_grasshopper/9,
					 get_ladybugs/9,
					 get_mosquitos/9,
					 get_pillbugs/9,
					 get_spiders/9
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

get_queen(player(Queen,_,_,_,_,_,_,_),Queen).

get_ants(player(_,Ants,_,_,_,_,_,_),Ants).

get_beetles(player(_,_,Beetles,_,_,_,_,_),Beetles).

get_grasshopper(player(_,_,_,Grasshoppers,_,_,_,_),Grasshoppers).

get_ladybugs(player(_,_,_,_,Ladybugs,_,_,_),Ladybugs).

get_mosquitos(player(_,_,_,_,_,Mosquitos,_,_),Mosquitos).

get_pillbugs(player(_,_,_,_,_,_,Pillbugs,_),Pillbugs).

get_spiders(player(_,_,_,_,_,_,_,Spiders),Spiders).

