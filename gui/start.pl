% Load libraries and tools
:- use_module(library(pce)).
:- use_module("./primitives/draw_hex", [draw_hexagon/3]).

% Start Main Window
?- new(@p, picture('Board')).
?- draw_hexagon(500, 500, H), send(@p, display, H).
?- send(@p, open).
