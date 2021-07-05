:- module(symbolic_constant,
          [
              sym/2
          ]).


:- dynamic symbolic_constant/2.

sym(Name, Val) :-
    asserta(symbolic_constant(Name, Val)).

expand_constants(In, In) :-
    var(In),
    !.
expand_constants($In, Out) :-
    symbolic_constant(In, Out).
expand_constants($In, _Out) :-
    \+ symbolic_constant(In, _),
    throw(format('no such constant ~w'-[In])).
expand_constants(In, In) :-
    atomic(In).
expand_constants(In, Out) :-
    is_dict(In),
    dict_pairs(In, Tag, Pairs),
    maplist(expand_pair, Pairs, PairsOut),
    dict_pairs(Out, Tag, PairsOut),
    !.
expand_constants(In, Out) :-
    compound(In),
    In =.. InList,
    maplist(expand_constants, InList, OutList),
    Out =.. OutList.
expand_constants(In, Out) :-
    is_list(In),
    maplist(expand_constants, In, Out).

expand_pair(K-V, KO-VO) :-
    expand_constants(K, KO),
    expand_constants(V, VO).

user:term_expansion(In, Out) :-
    expand_constants(In, Out).

