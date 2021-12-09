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
    ai_get_game_state(RealGameState),
    write_ln('S1'),
    get_next_moves(AllPosMoves),
    write_ln('S2'),
    % analyze_moves(Address, AllPosMoves, 0, [], BestMoves),
    % do_searches(Address, BestMoves),
  
    % ===== Multi-Threading (for later) =====
    % Make the dynamic predicates thread independent
    % Use mutexes to when writing to database
    % create some threads to analyse several path down 
    % =====          End                =====
    
    % ai_set_game_state(RealGameState),
    write_ln('S3'),
    select_next_move(Address, AllPosMoves, FinalNextMove),
    write_ln('S4'),
    explore_node(Address, FinalNextMove, true, NextNode),
    write_ln('Simulation Ended').

do_searches(_, _, 0).
do_searches(_, [], _).
do_searches(Address, [BestMove|OtherMoves], Amount) :-
    explore_node(Address, NextMove, Node),
    search(Node, EndNode),
    increase_total_visits,
    ai_game_status(Status),
    backpropagate(EndNode, Status),
    
    DecAmount is Amount - 1,
    do_searches(Address, OtherMoves, DecAmount).
  

search(Node, Node) :-
    not(get_type(Node, non_terminal)),!.
search(
    Node,
    EndNode
) :-
    get_address(Node, Address),
    select_next_move(Address, NextMove),
    explore_node(Address, NextMove, false, NewNode),
    search(NewNode, EndNode).

explore_node(Address, NextMove, Node) :-
    explore_node(Address, NextMove, false, Node).
explore_node(Address, NextMove, Visited, Node) :-
    write_ln('E1'),
    ai_change_game_state(NextMove),
    write_ln('E2'),
    ai_get_game_state(GameState),
    write_ln('E3'),
    ai_game_status(NodeType),
    write_ln('E4'),
    force_find_node(Address, GameState, NextMove, NodeType, Visited, Node).

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
    select_next_move(Address, NextMoves, NextMove).
select_next_move(Address, NextMoves, NextMove) :-
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
            apply_heuristics(Move, C),
			% C =1,
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
    (
	    (Moves = [], Place = [], NextMoves = [skip_move]);
        append(Moves,Places,NextMoves)
    ).


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
