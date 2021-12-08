:- module(total_visits, [
    load_total_visits_db/0,
    init_total_visits/0,
    increase_total_visits/0
]).

:- use_module(library(persistency)).

:- persistent total_visits(num: integer).

load_total_visits_db :-
    db_attach('../ai/db/total_visits_db.pl', []).

init_total_visits :-
    assert_total_visits(0).

increase_total_visits :-
    retract_total_visits(TotalVisits),
    NewTotalVisits is TotalVisits + 1,
    assert_total_visits(NewTotalVisits).

