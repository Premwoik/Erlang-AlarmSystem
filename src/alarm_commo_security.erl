%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dec 2017 20:53
%%%-------------------------------------------------------------------
-module(alarm_commo_security).
-author("prw").
-behaviour(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2]).


init(InitArgs) ->
  erlang:error(not_implemented).

handle_event(Event, State) ->
  erlang:error(not_implemented).

handle_call(Request, State) ->
  erlang:error(not_implemented).