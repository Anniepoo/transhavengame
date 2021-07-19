

% old stuff

:- if(false).

% leaving in case we drop the reload whole page, otherwise I'll fight
% CORS again
game_turn(Request) :-
    http_read_json_dict(Request, Payload, [value_string_as(atom)]),
    do_in_chr_thread(new_state(S, Payload),
                     get_chr_response_dict(S, Response)),
    format('Access-Control-Allow-Origin: *~n'),
    reply_json_dict(Response).

new_state(S, _Payload) :-
    make_player_inited(S).
%    act(S, Payload.action). TODO this may all go away

get_chr_response_dict(S, Response) :-
    get_state(S, Response).

:- endif.
