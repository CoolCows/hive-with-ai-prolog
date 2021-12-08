:- module(node, [
    get_address/2,
    get_type/2,
    get_times_explored/2,
    get_times_white_won/2,
    get_times_black_won/2,
    force_find_node/4,
    add_initial_node/0,
    find_node_by_game_state/2,
    find_node_by_edge_move/2,
    keccak256/3
]).

:- use_module(library(persistency)).

% define persistent game node
:- persistent
    node(
        address:string,          % hash(parent_address + str(game_state))
        auxiliar_address:string, % hash(parent_address + str(move that made new_state from old_state))
        parent_address:string,   
        game_state:list,         % [board, players, turns, last_played]
        node_type: atom,         % non_terminal | white_won | black_won | draw
        node_visited: bool,      % true | false
        times_explored: integer,
        times_white_won: integer,
        times_black_won: integer
    ).

% load stored tree
load_tree_db :-
    db_attach('./tree_db.pl', []).

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
    node(_, _, _, Type, _, _, _, _, _),
    Type
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

% Methods:
force_find_node(ParentAddress, GameState, EdgeMove, NodeType, Node) :-
   keccak256(ParentAddress, GameState, NodeAddress),
   (
        find_node_by_game_state(NodeAddress, Node);
        (
            keccak256(ParentAddress, EdgeMove, AuxAddress),
            add_node(
                NodeAddres, AuxAddress, ParentAddress, GameState, NodeType, Node
            )
        )
    ).

add_initial_node:-
    find_node_by_game_state(1, _);
    assert_node(
        1,
        1,
        0,
        %define game state
        non_terminal,
        true,
        0,
        0,
        0
    ).

add_node(
    NodeAddress,
    AuxAddress,
    ParentAddress, 
    GameState, 
    NodeType,
    node(
        NodeAddress, AuxAddress, ParentAddress, GameState,
        NodeType, false, 0, 0, 0
    )
):-
    assert_node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        GameState, 
        NodeType, 
        false, 
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
        GameState, 
        Explored, 
        WhiteWon, 
        BlackWon
    )
).

find_node_by_edge_move(
    AuxAddress,
    node(
        NodeAddress, 
        AuxAddress,
        ParentAddress, 
        GameState, 
        Explored, 
        WhiteWon, 
        BlackWon
    )
).

% For backpropagation:
% 1. Finds node at NodeAddress
% 2. Updates properties
% 3. Returns ParentAddress
update_node(NodeAddress, NewExplored, NewWhiteWon, NewBlackWon) :-
    find_node(NodeAddress, Node),
    Node = node(_, AuxAddress, ParentAddress, GameState, NodeType, Explored, WhiteWon, BlackWon),
    retract_node(Node),
    assert_node(
        NodeAddres,
        AuxAddress,
        ParentAddress,
        GameState,
        NodeType,
        NewExplored,
        NewWhiteWon,
        NewBlackWon
    ).

% ParentAddress: str
% Salt: term
% Hash: str (returns)
keccak256(ParentAddress, Salt, Hash) :-
    term_string(Salt, StrSalt),
    string_concat(ParentAddress, StrSalt, Seed),
    crypto_data_hash(Seed, Hash, [algorithm(sha3)]).
