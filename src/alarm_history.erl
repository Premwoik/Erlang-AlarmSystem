%%%-------------------------------------------------------------------
%%% @author prw
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jan 2018 14:43
%%%-------------------------------------------------------------------
-module(alarm_history).
-author("prw").
-behaviour(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2]).



init(_InitArgs) ->
  {ok, []}.

handle_event(_Msg, State) ->
  {ok, State}.

handle_call(_Msg, State) ->
  {ok,ok, State}.

