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

:- http_handler('/static/css/', http_reply_from_files('../web/css/', []), [prefix]).












