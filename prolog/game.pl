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

		 /*******************************
		 *      Overall Game Logic      *
		 *******************************/
mutate($up) :-
    debug(game, 'move up', []),
    player_location(X, Y),
    NY is max(0, Y - $cellh),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(X, NY)).
mutate($right) :-
    debug(game, 'move right', []),
    player_location(X, Y),
    NX is min($cellw * $numcols, X + $cellw),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(NX, Y)).
mutate($down) :-
    debug(game, 'move down', []),
    player_location(X, Y),
    NY is min($cellh * $numrows, Y + $cellh),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(X, NY)).
mutate($left) :-
    debug(game, 'move left', []),
    player_location(X, Y),
    NX is max(0, X - $cellw),
    http_session_retractall(player_loc(_, _)),
    http_session_asserta(player_loc(NX, Y)).
mutate($plant) :-
    debug(game, 'plant', []),
    player_locationrc(R, C),
    board_tile(R, C, T),
    plant_on(R, C, T).
mutate(K) :-
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

plantable_ground(grass, plowed).
plantable_ground(plowed, planted).

plant_time(grass, 3).
plant_time(plowed, 2).




% old stuff

:- if(false).

% leaving in case we drop the reload whole page, otherwise I'll fight
% CORS again
game_turn(Request) :-
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    do_in_chr_thread(new_state(S, Payload),
                     get_chr_response_dict(S, Response)),
    format('Access-Control-Allow-Origin: *~n'),
    reply_json_dict(Response).

new_state(S, _Payload) :-
    make_player_inited(S).
%    act(S, Payload.action). TODO this may all go away

get_chr_response_dict(S, Response) :-
    get_state(S, Response).

:- endif.
