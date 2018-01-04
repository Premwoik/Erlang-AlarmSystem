%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Dec 2017 18:17
%%%-------------------------------------------------------------------
-module(alarm).
-author("prw").
%% API
-export([turn_off_alarm/0, turn_on_alarm/0, turn_on_alarm/1, remote_action/1]).

remote_action({zone, Name}) ->
  error(not_implemented);

remote_action({output, State}) ->
  error(not_implemented).


turn_on_alarm() ->
  error(not_implemented).

turn_on_alarm(Reson) ->
  error(not_implemented).


turn_off_alarm() ->
  error(not_implemented).



