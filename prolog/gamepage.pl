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
                        key(Key, [default(0), integer])
                    ]),
    mutate(Key),
    print_session,
    reply_html_page(title('Trans Haven'),
                    \haven_body).

haven_body -->
    html([
        div(class(banner), [
                \help,
                h1('Trans haven')
            ]),
        \board
    ]).


help -->
    html(
        div(class(help),
            table([
                tr([td('\u2190'), td('move left'), td('\u2192'), td('move right')]),
                tr([td('\u2191'), td('move up'), td('\u2193'), td('move down')]),
                tr([td(p), td('(3x) plant veggies')])
            ]))
    ).

print_session :-
     http_session_data(D),
     debug(game(session), '~q', [D]),
     fail.
print_session.


