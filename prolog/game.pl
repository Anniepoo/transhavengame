:- module(game, [
  mutate/2,
  create_chr_thread/0,
  player_location/3,
  board_tile/3
]).

:- use_module(library(chr)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).
:- use_module(board).

:- chr_constraint
  chr_key/2,
  player_loc/3,   % S, X, Y
  get_player_loc/3,
  begin_game/1,
  tile/4,
  get_tile/4.


player_loc(S, _, _) \ begin_game(S) <=> true.

begin_game(S) <=> player_loc(S, 128, 128).

chr_key(S, 38), player_loc(S, X, Y) <=> NX is X + 64, player_loc(S, NX, Y).
chr_key(_, _) <=> true.

player_loc(S, X, Y) \ get_player_loc(S, NX, NY) <=> NX = X, NY = Y.
tile(S, R, C, T) \ get_tile(S, R, C, NT) <=> T = NT.

player_location(S, X, Y) :-
    get_player_loc(S, X, Y).

board_tile(R, C, T) :-
    b_getval(current_session, S),
    get_tile(S, R, C, T).
board_tile(_, _, grass).


		 /*******************************
		 * Debug help                   *
		 *******************************/
debug_constraints(Where) :-
    find_chr_constraint(X),
    debug(constraint(Where), '~w', [X]),
    fail.
debug_constraints(_).

		 /*******************************
		 *  Thread Component            *
		 *******************************/

:- dynamic thread_name/2.

create_chr_thread :-
   retractall(thread_name(_,_)),
   gensym(sub, Sub),
   gensym(par, Par),
   gensym(chr, Chr),
   asserta(thread_name(sub, Sub)),
   asserta(thread_name(par, Par)),
   asserta(thread_name(chr, Chr)),
   message_queue_create(_, [ alias(Sub) ]),
   message_queue_create(_, [ alias(Par) ]),
   thread_create((init_global, polling_sub), _, [ alias(Chr),
           at_exit(debug(lines, 'CHR thread exited', []))]).

polling_sub :-
   % listen for new message on `sub` queue
   thread_name(sub, Sub),
   thread_get_message(Sub, sync(ActionCHR, ResultCHR)),
   debug_constraints(polling_sub),
   % do the actual constraint call
   (   call(ActionCHR)
   ;
       debug(constraint(polling_sub),
             'action constraint ~w failed unexpectedly~n',
             [ActionCHR]),
       gtrace
   ),
   debug_constraints(polling_sub),
   % get the result using the get_foo pattern
   strip_module(ResultCHR, M, P),
   P =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(M:CallMe)
   ;
       debug(constraint(polling_sub),
             'result constraint ~w failed unexpectedly~n',
             [ResultCHR]),
       gtrace
   ),
   !, % nondet calls not allowed
   % send it back to the `par` message queue
   thread_name(par, Par),
   thread_send_message(Par, Result),
   % repeat
   polling_sub.

:- meta_predicate do_in_chr_thread(0, 0).

%!  do_in_chr_thread(+ActionCHR:chr_constraint,
%!         +ResultCHR:chr_constraint) is det
%
%   queries ActionCHR in the chr thread, which must be
%   grounded chr_constraint or prolog predicate,
%   then calls ResultCHR, whose last argument must be unbound.
%   the last argument will be bound as if a direct chr call
%   was made.
%
% eg to touch the egg to the pan and then get the egg's costume do
% do_in_chr_thread(touch(S, egg, pan), get_costume(S, egg, Costume))
%
% Note that these are effectively called in once/1
%
do_in_chr_thread(ActionCHR, ResultCHR) :-
   ResultCHR =.. List,
   append(_, [Result], List),
   thread_name(sub, Sub),
   thread_send_message(Sub, sync(ActionCHR, ResultCHR)),
   thread_name(par, Par),
   thread_get_message(Par, Result).

:- debug(constraint(_)).
:- debug(lines).

		 /*******************************
		 *      Overall Game Logic      *
		 *******************************/

init_global.  % init for all chr for the server

% return the tokenized html for the board
rendered_result(S, Board) :-
    b_setval(current_session, S),
    phrase(html(\board), Board).

mutate(Key, Result) :-
    http_in_session(S), % grab in the http thread
    do_in_chr_thread(alter_state(S, Key),
                     rendered_result(S, Result)).


alter_state(S, Key) :-
    begin_game(S),
    chr_key(S, Key).


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
