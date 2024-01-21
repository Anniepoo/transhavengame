:- module(board, [
              board//0
          ]).

:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).

board_size(20, 20).

% TODO use setof's group-by to make nested lists of T
board -->
    { board_size(NumCols, NumRows),
      setof(X, between(1, NumCols, X), Row),
      setof(Y-Row, between(1, NumRows, Y), Board)
    },
    html(table(\rows(Board))).

rows([]) --> [].
rows([Y-Row |More]) -->
    html([tr(\row(Y, Row)) | \rows(More)]).

row(_, []) --> [].
row(Y, [X | More]) -->
    {(   http_session_data(board(Y, X, T))
    ;   T = grass
    )},
    html(td(img(src("/static/img/"+T+".png")))),
    row(Y, More).

