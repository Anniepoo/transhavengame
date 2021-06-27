:- module(server, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(chr)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).

:- use_module(gamepage).
:- use_module(game).

go :- server(8888).

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    create_chr_thread,
    http_server(http_dispatch,
                [ port(Port)
                ]).

:- http_handler('/static/html/', http_reply_from_files('../web/html/', []), [prefix]).

:- http_handler('/static/svg/', http_reply_from_files('../web/svg/', []), [prefix]).

:- http_handler('/static/img/', http_reply_from_files('../web/img/', []), [prefix]).

:- http_handler('/static/js/', http_reply_from_files('../web/js/', []), [prefix]).

:- http_handler('/static/font/', http_reply_from_files('../web/fonts/', []), [prefix]).

:- http_handler('/static/image/', http_reply_from_files('../web/image/', []), [prefix]).

:- http_handler('/game_turn', game_turn , []).

game_turn(Request) :-
    http_in_session(S),
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    do_in_chr_thread(new_state(S, Payload),
                     get_chr_response_dict(S, Response)),
    format('Access-Control-Allow-Origin: *~n'),
    reply_json_dict(Response).

		 /*******************************
		 *      Overall Game Logic      *
		 *******************************/

new_state(S, Payload) :-
    make_player_inited(S).
%    act(S, Payload.action). TODO this may all go away

get_chr_response_dict(S, Response) :-
    get_state(S, Response).

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
   ResultCHR =.. List,
   append(StubList, [_], List),
   append(StubList, [Result], CallMeList),
   CallMe =.. CallMeList,
   (   call(CallMe)
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











