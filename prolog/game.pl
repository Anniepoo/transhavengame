:- module(game, [
  get_state/2,
  init_global/0,
  make_player_inited/1 % +S inited
]).


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
