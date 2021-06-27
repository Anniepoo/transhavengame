:- module(game, [
  get_state/2,
  init_global/0,
  make_player_inited/1 % +S inited
]).

:- use_module(library(chr)).

:- chr_constraint
  cur_ticks/2,            % session, ticks         the current time in ticks for session s
  get_ticks/2,            % session, Ticks         transient getter of the current ticks
  days_go_by/2,           % session, ticks         increment the current time (and make world go)
  reset_time/1,           % session                reset the current time to start

  chr_reset/1,            % session                transient reset the game state
  inited/1,               % session                exists if this session initialized
  make_player_inited/1,   % session                transient idempotic insure session inited

  act/2,                  % session, actionname    transient, causes this action to happen
  potential_action/1,     % actionname             same as known_action but as CHR constraint
  get_available_actions/2, % session, ActionName   transient get all actions we can do now
  collect_available_actions/2, % session, Actions  transient, after we've made available_actions collect them
  available_action/2,     % session, actionname     transient this action is available
  acty_done/2,            % session, actyname      simple one time activities. succeeds if this acty done
  acty/2,                 % session, actyname      record we did this acty


  thing/3,               % session, type, status   an individual object
  count_things/3,        % session, type, Count    return the count of things
  set_init_inventory/1,  % session                 transient create the initial inventory

  news/2,                % session, news           the news for this turn
  get_news/2.            % session, News           getter for this turns news

get_state(S, Response) :-
    (   get_state_(S, Response)
    ->  true
    ;   gtrace
    ).
get_state_(_S, _{ foo: 7}). % :-
   %  b_setval(session, S). S is the session #

		 /*******************************
		 * Global initialization.
		 *
		 * Things that happen once at startup.
		 *
		 *******************************/
init_global. % :-
%  setof(X, known_action(X), List),
%  maplist(potential_action, List).


make_player_inited(_S).  % session
