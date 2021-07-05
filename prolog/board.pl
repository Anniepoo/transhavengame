:- module(board, [
              board//0
          ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_head)).

:- use_module(game, [player_location/2, board_tile/3]).

board_size(40, 20). % NumCols, NumRows

:- html_resource(style, [virtual,
                         requires('/static/css/style.css')]).

board -->
    html_requires('/static/css/style.css'),
    html_requires('/static/js/player.js'),
    { board_size(NumCols, NumRows),
      setof(X, between(1, NumCols, X), Row),
      setof(Y-Row, between(1, NumRows, Y), Board)
    },
    html(div(class(board), [\rows(Board),\objects])).

rows([]) --> [].
rows([Y-Row |More]) -->
    html([div(class(row), \row(Y, Row))]),
    html(\rows(More)).

row(_, []) --> [].
row(Y, [X | More]) -->
    {board_tile(Y, X, T) },
    html(img(src="/static/img/"+T+".png")),
    row(Y, More).


objects -->
    { player_location(X, Y) },
    html(img([class(object), style("left: ~dpx; top: ~dpx;"-[X,Y]), src("/static/img/player-r.png")], [])).


