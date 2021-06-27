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


:- use_module(board).

:- http_handler('/', main_page, []).

main_page(_Request) :-
    reply_html_page(title('Trans Haven'),
                    \haven_body).


haven_body -->
    html([
        h1('Trans haven'),
        \board
    ]).


