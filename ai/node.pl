:- use_module(library(persistency)).

% define persistent game node
:- persistent
    node(
        address:string,          % hash(parent_address + str(game_state))
        parent_address:string,   
        game_state:list,         % [board, players, turns, last_played]
        node_type: atom,         % non_terminal | white_won | black_won | draw
        times_explored: integer,
        times_white_won: integer,
        times_black_won: integer
    ).

% load stored tree
load_tree_db :-
    db_attach('./tree_db.pl', []).

% From parent address and Game State calculate node properties
% and insert in tree_db.pl
add_node(ParentAddress, GameState) :-
    keccak256(ParentAddress, GameState, NodeAddres),
    assert_node(
        NodeAddress, 
        ParentAddress, 
        GameState, 
        undefined, 
        undefined, 
        undefined, 
        undefined
    ).

% returns a node by it's address
find_node(
    NodeAddress,
    node(
        NodeAddress, 
        ParentAddress, 
        GameState, 
        Explored, 
        WhiteWon, 
        BlackWon
    )
).

% For backpropagation:
% 1. Finds node at NodeAddres
% 2. Updates properties
% 3. Returns ParentAddress
update_node(NodeAddress, NewExplored, NewWhiteWon, NewBlackWon, ParentAddress) :-
    find_node(NodeAddress, Node),
    Node = node(_, ParentAddress, GameState, NodeType, Explored, WhiteWon, BlackWon),
    retract_node(Node),
    assert_node(
        NodeAddres,
        ParentAddress,
        GameState,
        NodeType,
        NewExplored,
        NewWhiteWon,
        NewBlackWon
    ).
keccak256(ParentAdrress, GameState, Hash) :-
    term_string(GameState, StrGameState),
    string_concat(ParentAdrress, StrGameState, Seed),
    crypto_data_hash(Seed, Hash, [algorithm(sha3)]).
