:- module(server, [go/0]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
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

new_state(S, _Payload) :-
    make_player_inited(S).
%    act(S, Payload.action). TODO this may all go away


