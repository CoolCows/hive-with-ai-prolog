% TODO:
% Explore children in different threads
% Speed up the process
%
% Number of time this node is explored
% Number of game won through this node
% Total explorations
%
% Use reinforced learning 
:- module(ai, [
    run_simulation/2
]).

:- use_module(ai_api, [
    ai_current_player_color/1,
    ai_game_status/1,
    ai_get_game_state/1,
    ai_set_game_state/1,
    ai_change_game_state/1
]).
:- use_module("../game/hive_api").
:- use_module(total_visits, [increase_total_visits/0, get_total_visits/1]).
:- use_module(heuristics).
:- use_module(node).

run_simulation(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    write_ln('End Case Simulation').
run_simulation(
    Node,
    NextNode
) :-
    write_ln('Running simulation'),
    write_ln(Node),
    get_address(Node, Address),
    ai_get_game_state(RealGameState),!,
    write_ln('Got Game State'),
    write_ln(RealGameState),
    select_next_move(Address, NextMove),  
    % ===== Multi-Threading (for later) =====
    % Make the dynamic predicates thread independent
    % Use mutexes to when writing to database
    % create some threads to analyse several path down 
    % =====          End                =====

    % After simulation select cells to move
    % ai_set_game_state(RealGameState),
    % select_next_move(Address, NextMove),
    write_ln('Definitive Move:'),
    write_ln(NextMove),
    ai_change_game_state(NextMove),
    ai_get_game_state(NewGameState),
    write_ln('Updated Game State'),
    write_ln(NewGameState),
    ai_game_status(NodeType),
	write_ln("hfkadffjakfsfsddf"),
	write_ln("\n"),
	write_ln(NodeType),

    force_find_node(Address, NewGameState, NextMove, NodeType, true, NextNode),
    write_ln('Simulation Ended').

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!,
    ai_current_player_color(Color),
    increase_total_visits,
    backpropagate(Node, Color).
search(
    Node,
    EndNode
) :-
    get_address(Node, Address),
    select_next_move(Address, NextMove),
    ai_change_game_state(NextMove),
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
    write_ln('Selecting Next Move'),
    get_next_moves(NextMoves), 
    write_ln('Analyzing Next Moves'),
    write_ln(Address),
    analyze_moves(Address, NextMoves, 0, [], [NextMove|_]),
    true.

analyze_moves(_, [], _, BestMoves, BestMoves).
analyze_moves(Address, [Move|NextMoves], MaxValue, TopMoves, BestMoves) :-
    keccak256(Address, Move, AuxAddress),
    (
        (
            find_node_by_edge_move(AuxAddress, Node),
            uct(Node, Move, NewValue)
        );
        (
            % Call to Heuristics and Multiply for constant Value
            % apply_heuristics(Move, C),
			C =1,
            get_total_visits(TotalVisits),
            NewValue is C*sqrt(TotalVisits)
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
get_next_moves(NextMoves) :-
	hive_current_player_color(Color),
	write_ln("GETTING POSSIBLE MOVES"),
	findall(Move,possible_moves(Color,Move), Moves ),
	write_ln(Moves),
	write_ln("GETTING POSSIBLE PLACE"),
	findall(Place, possible_place(Color,Place),Places),
	write_ln(Places),
	append(Moves,Places,NextMoves).


possible_moves(Color,move(SourceCell,DestCell)):-
	hive_get_cell(cell(_,_,_,Color,_),SourceCell),
	hive_get_possible_moves(SourceCell,PosMoves),
	member(DestCell,PosMoves).

possible_place(Color,place(Cell)):-
	hive_current_player_turns(3),
	hive_get_player(player(Color,_, _, _, _, _, _, _, _),
					player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),
	Queen = 1,	
	hive_get_possible_positions(Color,PosPositions),!,
	member(cell(_,Row,Col,_,_),PosPositions),
	Cell = cell(queen,Row,Col,Color,0).

possible_place(Color,place(Cell)):-
	hive_get_player(player(Color,_, _, _, _, _, _, _, _),
					player(Color,Queen, Ants, Beetle, Grasshopper, Ladybug, Mosquito, Pillbug, Spider)),
	hive_get_possible_positions(Color,PosPositions),
	member(cell(_,Row,Col,_,_),PosPositions),
	(
		(
			Queen > 0,
			Cell = cell(queen,Row,Col,Color,0) 
		);
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

% Upper Confidence Bound
uct(Node, Move, Result) :-
    ai_current_player_color(Color),
    (
        (Color = white, get_stats(Node, TimesWon, _, Explored));
        get_stats(Node, _, TimesWon, Explored)
    ),
	%apply_heuristics(Move, C),
    C = 1,
    get_total_visits(TotalVisits),
    Result is TimesWon/Explored + C*sqrt(TotalVisits)/Explored.
