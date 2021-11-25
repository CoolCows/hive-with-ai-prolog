:- use_module("events_commons", [click_inside_hexagon/3]).

select_remaining_board(Colour, Canvas, ClickPosition) :-
    (
        (Colour = white, nb_getval(white_player, Player));
        (Colour = black, nb_getval(black_player, Player))
    ),


select_remaining_cell(Canvas, ClickPosition, Player) :-
    true.

click_inside(ClickPosition, Index) :-
    % This should not be needed
    % get(ClickPosition, point, ClickPoint),
    get(ClickPosition, x, ClickX),
    get(ClickPosition, y, ClickY),
    ModIndex is Index mod 4,
    Row is Index // 4,
    X is ModIndex*60*S + ModIndex*75*S + 50,
    Y is 60*S + 110*S*Row,
    Dist is 50*Scale,
    click_inside_hexagon(point(ClickX, ClickY), point(X, Y), Dist).
