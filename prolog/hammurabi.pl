:- module(hammurabi, [
              go/0
          ]).
/** <module> In this game you must run a trans haven.
 *
 */


:- use_module(library(chr)).

ps :-
    current_chr_constraint(Module:Name),
    format('constraint store contains ~w:~w~n', [Module, Name]),
    fail.
ps.

:- chr_constraint wipe_out/0, own/1, one_owned/1, get_owned/1, collect_owned/1,
    do/1, time/0, eat/0, hungry/1.

wipe_out \ own(_) <=> true.
wipe_out <=>
    own(bldg(trailer)),
    own(field(veggies)),
    own(vehicle(truck)),
    own(person(me)),
    own(money(10000)),
    own(stored(oats, 10)).

get_owned(_), own(X) ==> one_owned(X).
get_owned(L) <=> collect_owned(L).

one_owned(X), collect_owned(L) <=>
    L = [X|L1],
    collect_owned(L1).
collect_owned(L) <=> L = [].

own(field(C)) \ do(harvest(C)) <=>
    format('You harvest some ~w~n', [C]),
    own(stored(C, 10)).

time, own(person(P)) ==> hungry(P).
time <=> eat.

eat, own(person(P)) \ hungry(P), own(stored(C, N)) <=>
    format('~w eats some ~w~n', [P , C]),
    succ(NN, N),
    own(stored(C, NN)).
eat <=> true.

go :-
    prompt(Old, 'cmd: '),
    wipe_out,
    loop,
    prompt(_, Old).

loop :-
    get_owned(Stuff),
    maplist(describe, Stuff),
    get_cmd(Cmd),
    (   Cmd = stop
    ;   call(Cmd),
        loop
    ).

describe(bldg(B)) :- format('You have a ~w~n', [B]).
describe(field(C)) :- format('You have a field planted with ~w~n', [C]).
describe(vehicle(V)) :- format('You have a running ~w~n', [V]).
describe(person(P)) :-
    P \= me,
    format('~w is here~n', [P]).
describe(person(me)).
describe(money(M)) :-
    format('you have ~w$~n', [M]).
describe(stored(C, N)) :-
    format('You have ~w ~ws ~n', [N, C]).
describe(X) :-
    format('You have something literally indescribable! ~q~n', [X]).

get_cmd(C) :-
    read(C).



