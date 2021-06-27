:- module(gamepage, []).
/** <module> serves the main game page
 *
 */

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(chr)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).


:- use_module(board).
:- use_module(game).

:- http_handler('/', main_page, []).

main_page(Request) :-
    http_parameters(Request,
                    [
                        key(Key, [default(0)])
                    ]),
    mutate(Key, Result),
    reply_html_page(title('Trans Haven'),
                    \haven_body(Result)).

haven_body(Board) -->
    html([
        h1('Trans haven'),
        \Board
    ]).




