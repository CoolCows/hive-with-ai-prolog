% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 

:- use_module(ai_api, [
    ai_current_player_color/1,
    ai_update_state/2
]).
:- use_module("../game/hive_api").
:- use_module(total_visits, [increase_total_visits/0]).

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)).
run_simulation(
    Node,
    NextNode
) :-
    get_address(Node, Address),
    ai_get_game_state(RealGameState),
    select_next_move(Address, NextMove),  
    % ===== Multi-Threading (for later) =====
    % Make the dynamic predicates thread independent
    % Use mutexes to when writing to database
    % create some threads to analyse several path down 
    % =====          End                =====

    % After simulation select cells to move
    ai_set_game_state(RealGameState),
    select_next_move(Address, NextMove),
    update_game_state(NextMove),
    get_game_state(NewGameState),
    ai_game_status(NodeType),
    force_find_node(Address, NewGameState, NextMove, NodeType, true, NextNode).

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    ai_current_player_color(Color),
    increase_total_visits,
    backpropagate(Node, Color).
search(
    node(Address, _, _, _, _, _, Explored, WhiteWon, BlackWon),
    EndNode
) :-
    select_next_move(Address, NextMove),
    update_game_state(NextMove),
    get_game_state(NewGameState),
    ai_game_status(NodeType),
    force_find_node(Address, NewGameState, NextMove, NodeType, Node),
    search(Node, EndNode).

backpropagate(Node, Color) :-
    get_parent_address(Node, 0),!,
    update_node(Node, Color).
backpropagate(Node, Color) :-
    update_node(Node, Color),
    get_parent_address(Node, ParentGameStateAddress),
    find_node_by_game_state(ParentGameStateAddress, ParentNode),
    backpropagate(ParentNode, Color).

select_next_move(Address, NextMove) :-
    get_next_moves(NextMoves), 
    analyze_moves(Address, NextMoves, -2^64, [], [NextMove|_]),
    true.

analyze_moves(_, [], _, BestMoves, BestMoves).
analyze_moves(Address, [Move|NextMoves], MaxValue, TopMoves, BestMoves) :-
    keccak256(Address, Move, AuxAddress),
    (
        (
            find_node_by_edge_move(AuxAddress, Node),
            uct(Node, NewValue)
        );
        (
            % Call to Heuristics and Multiply for constant Value
            total_visits(TotalVisits),
            NewValue is sqrt(TotalVisits)
        )
    ),
    (
        (
            NewValue > MaxValue,!, 
            analyze_moves(Address, NextMoves, NewValue, [Move], BestMoves)
        );
        (
            NewValue =:= MaxValue,!,
            analyze_moves(Address, NextMoves, MaxValue, [Move|TopMoves], BestMoves)
        );
        analyze_moves(Address, NextMoves, MaxValue, TopMoves, BestMoves)
    ).

% Get all possible moves
possible_moves(Color,move(SourceCell,DestCell)):-
	hive_get_cell(cell(_,_,_,Color,_),SourceCell),
	hive_get_possible_moves(SourceCell,PosMoves),
	member(DestCell,PosMoves).

possible_place(Color,place(Cell)):-
	hive_get_player(player(Color,_, _, _, _, _, _, _, _),
					player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),
	hive_get_possible_positions(Color,PosPositions),
	member(cell(_,Row,Col,_,_,_),PosPositions),
	(
		(
			Ants > 0,
			Cell = cell(ant,Row,Col,Color,0) 
		);
		(
			Beetle > 0,
			Cell = cell(beetle,Row,Col,Color,0) 
		);
		(
			Grasshopper > 0,
			Cell = cell(grasshopper,Row,Col,Color,0) 
		);
		(
			Ladybug > 0,
			Cell = cell(ladybug,Row,Col,Color,0) 
		);
		(
			Mosquito > 0,
			Cell = cell(mosquito,Row,Col,Color,0) 
		);
		(
			Pillbug > 0,
			Cell = cell(pillbug,Row,Col,Color,0) 
		);
		(
			Spider > 0,
			Cell = cell(spider,Row,Col,Color,0) 
		)
	).

get_next_moves(NextMoves) :-
	hive_current_player_color(Color),
	findall(Move,possible_moves(Color,Move), Moves ),
	findall(Place, possible_place(Color,Place),Places),
	append(Moves,Places,NextMoves).

update_game_state(NextMove) :-
    (
        NextMove = place(Cell),
        ai_put_cell(Cell)
    );
    (
        NextMove = move(SourceCell, DestCell),
        ai_move_cell(SourceCell, DestCell)
    ).

% Upper Confidence Bound
uct(node(_, _, _, _, Explored, WhiteWon, BlackWon), Result) :-
    ai_current_player_color(Color),
    (
        (Color = white, TimesWon = WhiteWon);
        TimesWon = BlackWon 
    ),
    % Apply Heuristics
    total_visits(TotalVisits),
    Result is TimesWon/Explored + sqrt(TotalVisits)/Explored.
