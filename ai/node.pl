:- module(node, [
    get_address/2,
    get_type/2,
    get_stats/4,
    get_times_explored/2,
    get_times_white_won/2,
    get_times_black_won/2,
    force_find_node/5,
    force_find_node/6,
    start_up_ia/0,
    add_initial_node/0,
    find_node_by_game_state/2,
    find_node_by_edge_move/2,
    keccak256/3
]).

:- use_module(library(persistency)).
:- use_module(total_visits, [init_total_visits/0, load_total_visits_db/0]).

% define persistent game node
% TODO
% There is no need to keep game_state!!!
:- persistent
    node(
        address:atom,          % hash(parent_address + str(game_state))
        auxiliar_address:atom, % hash(parent_address + str(move that made new_state from old_state))
        parent_address:atom,   
        node_type: atom,         % non_terminal | white_won | black_won | draw
        node_visited: atom,      % true | false
        times_explored: integer,
        times_white_won: integer,
        times_black_won: integer
    ).

% load stored tree
load_tree_db :-
    db_attach('../ai/db/tree_db.pl', []).

% Properties:

get_address(
    node(Address, _, _, _, _, _, _, _),
    Address
). 

get_aux_address(
    node(_, AuxAddress, _, _, _, _, _, _),
    AuxAddress
).

get_parent_address(
    node(_, _, ParentAddress, _, _, _, _, _, _),
    ParentAddress
). 

get_type(
    node(_, _, _, Type, _, _, _, _),
    Type
).

get_stats(
    node(_, _, _, _, _, _, Explored, WhiteWon, BlackWon),
    Explored, WhiteWon, BlackWon
).

get_times_explored(
    node(_, _, _, _, _, _, Explored, _, _),
    Explored
).

get_times_white_won(
    node(_, _, _, _, _, _, _, WhiteWon, _),
    WhiteWon
).

get_times_black_won(
    node(_, _, _, _, _, _, _, _, BlackWon),
    BlackWon
).

% Finds a Node and returns it
% If node does not exist, it creates a new Node and returns it
force_find_node(ParentAddress, GameState, EdgeMove, NodeType, Node) :-
    force_find_node(ParentAddress, GameState, EdgeMove, NodeType, false, Node).
force_find_node(ParentAddress, GameState, EdgeMove, NodeType, NodeVisited, Node) :-
   keccak256(ParentAddress, GameState, NodeAddress),
   (
        find_node_by_game_state(NodeAddress, Node);
        (
            keccak256(ParentAddress, EdgeMove, AuxAddress),
            add_node(
                NodeAddress, AuxAddress, ParentAddress, NodeType, NodeVisited, Node
            )
        )
    ).

start_up_ia :-
    load_total_visits_db,
    load_tree_db.

add_initial_node:-
    find_node_by_game_state('1', _);
    (
        write_ln('No node found'),
        init_total_visits,
        write_ln('Initializing total visits'),
        assert_node(
            '1',
            '1',
            '0',
            non_terminal,
            true,
            0,
            0,
            0
        ),
        write_ln('First node added')
    ).

add_node(
    NodeAddress,
    AuxAddress,
    ParentAddress, 
    NodeType,
    NodeVisited,
    node(
        NodeAddress, AuxAddress, ParentAddress,
        NodeType, NodeVisited, 0, 0, 0
    )
):-
    assert_node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        NodeType, 
        NodeVisited, 
        0, 
        0,
        0
    ).

% returns a node by it's address
find_node_by_game_state(
    NodeAddress,
    node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        NodeType,
        NodeVisited,
        Explored, 
        WhiteWon, 
        BlackWon
    )
):-
    node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        NodeType,
        NodeVisited,
        Explored, 
        WhiteWon, 
        BlackWon
    ).

find_node_by_edge_move(
    AuxAddress,
    node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        NodeType,
        NodeVisited,
        Explored, 
        WhiteWon, 
        BlackWon
    )
):-
    node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        NodeType,
        NodeVisited,
        Explored, 
        WhiteWon, 
        BlackWon
    ).

% For backpropagation:
% 1. Finds node at NodeAddress
% 2. Updates properties
% 3. Returns ParentAddress
update_node(Node, Color) :-
    Node = node(Address, AuxAddress, ParentAddress, NodeType, Explored, WhiteWon, BlackWon),
    retract_node(Node),
    NewExplored is Explored + 1,
    (
        (Color = black, NewBlackWon is BlackWon + 1, NewWhiteWon = WhiteWon);
        (Color = white, NewBlackWon = BlackWon, NewWhiteWon + 1 is WhiteWon)
    ),
    assert_node(
        Address,
        AuxAddress,
        ParentAddress,
        NodeType,
        NodeVisited,
        NewExplored,
        NewWhiteWon,
        NewBlackWon
    ).

% ParentAddress: str
% Salt: term
% Hash: str (returns)
keccak256(ParentAddress, Salt, StrHash) :-
    term_string(Salt, StrSalt),
    string_concat(ParentAddress, StrSalt, Seed),
    crypto_data_hash(Seed, Hash, [algorithm(sha256)]).
