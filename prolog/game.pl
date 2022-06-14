:- module(game, [
  mutate/1,
  player_location/2,
  board_tile/3
]).

:- use_module(library(http/http_session)).
:- use_module(symbolic_constant).


:- sym(startx, 128).
:- sym(starty, 128).
:- sym(cellw, 64).
:- sym(cellh, 64).
:- sym(numcols, 20).
:- sym(numrows, 10).
%arrow keys
:- sym(up, 38).
:- sym(down, 40).
:- sym(right, 39).
:- sym(left, 37).
:- sym(plant, 0'P).
:- sym(harvest, 0'H).

		 /*******************************
		 *   Extract Info              *
		 *******************************/

player_location(X, Y) :-
    http_session_data(player_loc(X, Y)).
player_location($startx, $starty).

player_locationrc(R, C) :-
    player_location(X, Y),
    R is 1 + floor(Y / $cellh),
    C is 1 + floor(X / $cellw).

board_tile(R, C, T) :-
    http_session_data(tile(R, C, T)).
board_tile(_, _, grass).

%!  a_square(-R:integer, -C:integer) is nondet
%
%   enumerate the tiles
%
a_square(R, C) :-
    between(1, $numcols, C),
    between(1, $numrows, R).

		 /*******************************
		 *      Overall Game Logic      *
		 *******************************/

mutate(Cmd) :-
    cmd_mutate(Cmd),
    time_pass.

cmd_mutate($up) :-
    debug(game, 'move up', []),
    player_location(X, Y),
    NY is max(0, Y - $cellh),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(X, NY)).
cmd_mutate($right) :-
    debug(game, 'move right', []),
    player_location(X, Y),
    NX is min($cellw * $numcols, X + $cellw),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(NX, Y)).
cmd_mutate($down) :-
    debug(game, 'move down', []),
    player_location(X, Y),
    NY is min($cellh * $numrows, Y + $cellh),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(X, NY)).
cmd_mutate($left) :-
    debug(game, 'move left', []),
    player_location(X, Y),
    NX is max(0, X - $cellw),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(NX, Y)).
cmd_mutate($plant) :-
    debug(game, 'plant', []),
    player_locationrc(R, C),
    board_tile(R, C, T),
    plant_on(R, C, T).
cmd_mutate($harvest) :-
    debug(game, 'harvest', []),
    player_locationrc(R, C),
    board_tile(R, C, T),
    harvest_on(R, C, T).
cmd_mutate(K) :-
    debug(game, 'move key ~w', [K]).

plant_on(_, _, T) :-
    \+ plantable_ground(T, _).
plant_on(R, C, T) :-
    plantable_ground(T, T1),
    http_session_data(tile_alter_progress(R, C, 0)),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_retractall(tile(R, C, _)),
    http_session_asserta(tile(R, C, T1)).
plant_on(R, C, _) :-
    http_session_data(tile_alter_progress(R, C, N)),
    N > 0,
    succ(NN, N),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_asserta(tile_alter_progress(R, C, NN)).
plant_on(R, C, T) :-
    plantable_ground(T, _),
    \+ http_session_data(tile_alter_progress(R, C, _)),
    plant_time(T, Time),
    http_session_asserta(tile_alter_progress(R, C, Time)).

harvest_on(_, _, T) :-
    \+ harvestable_ground(T, _).
harvest_on(R, C, T) :-
    harvestable_ground(T, T1),
    http_session_data(tile_alter_progress(R, C, 0)),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_retractall(tile(R, C, _)),
    http_session_asserta(tile(R, C, T1)).
harvest_on(R, C, _) :-
    http_session_data(tile_alter_progress(R, C, N)),
    N > 0,
    succ(NN, N),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_asserta(tile_alter_progress(R, C, NN)).
harvest_on(R, C, T) :-
    harvestable_ground(T, _),
    \+ http_session_data(tile_alter_progress(R, C, _)),
    plant_time(T, Time),
    http_session_asserta(tile_alter_progress(R, C, Time)).


plantable_ground(grass, plowed).
plantable_ground(plowed, planted).

harvestable_ground(veggie, grass).

plant_time(grass, 3).
plant_time(plowed, 2).
plant_time(veggie, 2).

time_pass :-
    a_square(R, C),
    board_tile(R, C, Type),
    time_pass(R, C, Type),
    fail.
time_pass.

time_pass(R, C, Type) :-
    grows(Type, _, Becomes),
    http_session_data(tile_alter_progress(R, C, 0)),
    !,
    debug(game(time), '~w at R~wC~w becomes ~w', [Type, R, C, Becomes]),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_retractall(tile(R, C, _)),
    http_session_asserta(tile(R, C, Becomes)).
time_pass(R, C, Type) :-
    grows(Type, _, _),
    http_session_data(tile_alter_progress(R, C, N)),
    N > 0,
    !,
    debug(game(time), '~w at R~wC~w changes in ~w', [Type, R, C, N]),
    succ(NN, N),
    http_session_retractall(tile_alter_progress(R, C, _)),
    http_session_asserta(tile_alter_progress(R, C, NN)).
time_pass(R, C, Type) :-
    \+ http_session_data(tile_alter_progress(R, C, _)),
    grows(Type, Time, _),
    !,
    debug(game(time), '~w at R~wC~w will change in ~w', [Type, R, C, Time]),
    http_session_asserta(tile_alter_progress(R, C, Time)).

grows(planted, 2, veg1).
grows(veg1, 4, veg2).
grows(veg2, 4, veggie).
grows(veggie, 3, wilted).



